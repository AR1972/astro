;************************************************************************
;									*
;	Copyright (C) 1991 by Trace Center 				*
;									*
;	PARAM.ASM							*
;									*
;************************************************************************

;-----------------------------------------------------------------------------
; This file contains all the parameters to pass back and forth with the user
; interface part of this program
;-----------------------------------------------------------------------------

TITLE	Parameters

INCLUDE	keyboard.inc

	PUBLIC		_dataBlock
	PUBLIC		_dataBlockSize

PUBLIC          SerialKeys_Param
        PUBLIC  _serialKeysOn
        PUBLIC  _skCommPortAddr
        PUBLIC  _skBaudRate
	PUBLIC 	_skCommIRQ
	PUBLIC	_injectByte
	PUBLIC	_forcedInt9Flag

PUBLIC	FilterKeys_Param
	PUBLIC	fFilterKeysOn
	PUBLIC	fFK_On_Off_Feedback
	PUBLIC	fUser_SetUp_Option1
	PUBLIC	fUser_SetUp_Option2
	PUBLIC	wait_ticks
	PUBLIC	delay_ticks
	PUBLIC	repeat_ticks
	PUBLIC	recovery_ticks
	PUBLIC	fmax_default
	PUBLIC	fclick_on
	PUBLIC	fRecovery_On
	PUBLIC	fDialog_Filter_off


PUBLIC  SticKeys_Param
	PUBLIC	fSticKeysOn
	PUBLIC	fSK_On_Off_Feedback
	PUBLIC	fAudible_Feedback
	PUBLIC	fTriState
	PUBLIC	fTwo_Keys_Off
	PUBLIC	fDialog_Stickeys_off
	PUBLIC	fstickeys_click


PUBLIC	MouseKeys_Param
	PUBLIC	fMouseKeysOn
	PUBLIC	fMK_On_Off_Feedback
	PUBLIC	Max_Speed
	PUBLIC	Time_To_Max_Speed
	PUBLIC	fDialog_Mouse_off
	PUBLIC	fmkeys_override


PUBLIC	ToggleKeys_Param
	PUBLIC	fToggleKeysOn
	PUBLIC	fTK_On_Off_Feedback
	PUBLIC	fDialog_Toggle_off


PUBLIC	TimeOut_Param
	PUBLIC	fTime_Out
	PUBLIC	fTO_On_Off_Feedback
	PUBLIC	to_value
	PUBLIC	fDialog_TimeOut_off

PUBLIC	Handicap_Param
	PUBLIC	fcomp_dialog
	PUBLIC	fcomp_dialog_id
	PUBLIC	fDialog_Action
	PUBLIC	fhearing_on
	PUBLIC	fvideo_flash
	PUBLIC	fspace_saver
	PUBLIC	fcomputer_not_found

	PUBLIC	fslow_baud_mouse 

	PUBLIC	fserial_keys_loaded	
        PUBLIC  fAccessAlreadyLoaded

PUBLIC  Find_Mouse_Param
        PUBLIC  btn_1
        PUBLIC  btn_2
        PUBLIC  Current_Button
	PUBLIC	_comp_id
	PUBLIC  _combase
	PUBLIC	fmouse_driver
	PUBLIC	_finject_keys
	PUBLIC	_vector
	PUBLIC	comp_flag
	PUBLIC	ExtendedSeg	
	PUBLIC	_fmouse_id
        PUBLIC  fvideo_type





_TEXT	segment	word public 'CODE'

	assume CS:_TEXT
	assume DS:NOTHING
	assume ES:NOTHING
	assume SS:NOTHING

	
_dataBlock		label	byte

SerialKeys_Param        label   byte
_serialKeysOn           DB      false           ; position 00 -- must be in this order -- SerialKeys Off       
_skCommPortAddr         DW      03f8h           ; position 01, 02 -- must be in this order -- COM1:
_skBaudRate             DW      0180h           ; position 03, 04 -- must be in this order -- 300 Baud


FilterKeys_Param	label	byte
fFilterKeysOn		DB	false		; position 05 -- must be in this order
fFK_On_Off_Feedback	DB	true		; position 06 -- must be in this order
fUser_SetUp_Option1	DB	false		; position 07 -- must be in this order
fUser_SetUp_Option2	DB	false		; position 08 -- must be in this order
wait_ticks		DW	wait_dflt_cnt	; position 09, 10 -- must be in this order
delay_ticks		DW	dly_dflt_cnt   	; position 11, 12 -- must be in this order
repeat_ticks		DW	rpt_dflt_cnt	; position 13, 14 -- must be in this order
;recovery_ticks		DW	rec_dflt_cnt	; position 15, 16 -- must be in this order
recovery_ticks		DW	32760		; position 15, 16 -- must be in this order

fmax_default		DB	false		; position 17 -- must be in this order
fclick_on		DB	true		; position 18 -- must be in this order
fRecovery_On		DB	false		; position 19 -- must be in this order
fDialog_Filter_off	DB	false		; position 20 -- must be in this order -- flag for dialog box on/off
FilterKeys_Param_Len	equ	16		; 16 bytes long



SticKeys_Param	label	word
fSticKeysOn		DB	false           ; position 21 -- must be in this order
fSK_On_Off_Feedback	DB	true            ; position 22 -- must be in this order
fAudible_Feedback	DB	true            ; position 23 -- must be in this order
fTriState  		DB	true            ; position 24 -- must be in this order
fTwo_Keys_Off		DB	true            ; position 25 -- must be in this order
fDialog_Stickeys_off	DB	false		; position 26 -- must be in this order -- flag for dialog box on/off
fstickeys_click	        DB	false		; position 27 -- must be in this order -- flag if user want StickeyKeys with a click
SticKeys_Param_Len	equ	7		; 7 bytes long 


MouseKeys_Param	label	word
fMouseKeysOn		DB	false		; position 28 -- must be in this order -- flag for if on
fMK_On_Off_Feedback	DB	true		; position 29 -- must be in this order
Max_Speed		DW	8 		; position 30, 31 -- must be in this order
Time_To_Max_Speed	DW	1		; position 32, 33 -- must be in this order
fDialog_Mouse_off	DB	false		; position 34 -- must be in this order -- flag for dialog box on/off
fmkeys_override		DB	true		; position 35 -- must be in this order -- flag to allow user to have MouseKey mouse button take 
MouseKeys_Param_Len	equ	8		; 8 bytes long


ToggleKeys_Param	label	word
fToggleKeysOn		DB	false           ; position 36 -- must be in this order
fTK_On_Off_Feedback	DB	true            ; position 37 -- must be in this order
fDialog_Toggle_off	DB	false		; position 38 -- must be in this order -- flag for dialog box on/off
ToggleKeys_Param_Len	equ	3		; 3 bytes long


TimeOut_Param	label	word
fTime_Out		DB	false           ; position 39 -- must be in this order
fTO_On_Off_Feedback	DB	true            ; position 40 -- must be in this order
to_value		DW	to_dflt_cnt 	; position 41, 42 -- must be in this order -- holds time out value to turn off
fDialog_TimeOut_off	DB	false		; position 43 -- must be in this order -- flag for dialog on/off
TimeOut_Param_Len	equ	5		; 5 bytes long

Handicap_Param	label	word
fcomp_dialog		DB	false		; position 44 -- must be in this order -- holds flag if user decided to choose alternate computer
fcomp_dialog_id		DB	false		; position 45 -- must be in this order -- holds type of computer user choose 1,2, or 3
fDialog_Action		DB	false		; position 46 -- must be in this order -- flag to tell us that a change occurred in Dialog or Menu box
fspace_saver		DB	false		; position 47 -- must be in this order
fcomputer_not_found	DB	false		; position 48 -- must be in this order -- flag to tell menu that computer was not ID'ed and to display messages
fhearing_on		DB	true		; position 49 -- must be in this order -- flag set by user to provide visual feedback for beeps

fserial_keys_loaded	db	false	   	; position 50 -- must be in this order -- flag to inform Startup.asm
					   	; when to call other SerialKeys 
					   	; routines if "AccesDOS" is restarted
fAccessAlreadyLoaded    db      false           ; position 51 -- must be in this order -- true if accessDOS is already loaded
Handicap_Param_Len	equ	8		; 8 bytes long

_skCommIRQ		db	4		; position 52 -- must be in this order -- 4 = IRQ 4,   3 = IRQ 3
fvideo_flash		db	false		; position 53 -- must be in this order -- flag for screen flash when computer beeps

filler_space		db	'1234567'       ; position 54, 55, 56, 57, 58, 59, 60 -- must be in this order
endOfDataBlock		label	word


Find_Mouse_Param        label   word
btn_1			DB	0		; position 61 -- must be in this order -- holds btn info for mouse
btn_2			DB	0		; position 62 -- must be in this order -- holds btn info for mouse
Current_Button		DB	0		; position 63 -- must be in this order -- holds which mouse button is active
_comp_id		DB	false		; position 64 -- must be in this order -- START OF COMPUTER ID FLAGS
fmouse_driver		DB	false		; position 65 -- must be in this order -- flag to indicate that a mouse and mouse driver were found
_fmouse_id		DB	false		; position 66 -- must be in this order -- id byte to tell software what type of mouse is connected to host
_combase       		DW	0		; position 67, 68 -- must be in this order -- holds address of com port which has the serial mouse if found
ExtendedSeg		DW	0		; position 69, 70 -- must be in this order -- holds segment address of mouse driver	Extended data area
_vector			DB	0		; position 71 -- must be in this order -- holds the kybd interrupt vector we need to hook during keyboard input
_finject_keys		DB	false		; position 72 -- must be in this order -- flag to tell us the computer supports Injection of keystrokes into the hardware
fvideo_type		DB	0		; position 73 -- must be in this order -- 3=CGA 4=EGA 5=VGA
comp_flag		DB	false		; position 74 -- must be in this order -- flag to tell us if we have BIOS support for kb_flag_1 and kb_flag_3
fslow_baud_mouse	db	false		; position 75 -- flag set for 300 baud mouse to slow timer writes down

_dataBlockSize		dw	endOfDataBlock -_dataBlock

_injectByte		DB 	0		; for serial keys: holds byte to inject in Int 15h 4Fh intercept
_forcedInt9Flag		DB 	0		; for serial keys: flag that signals when serial keys is injecting a key code


_TEXT	ends

	end
