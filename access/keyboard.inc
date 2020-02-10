;***************************************************************************
;                                                                          *
;	Copyright (C) 1990 by Trace Center (just kidding)		   *
;									   *
;	KEYBOARD.INC							   *
;								           *
;***************************************************************************

ack_port 	equ	20h		; 8259 acknowledge port

;eoi		equ	20h		; 8259 end of interrupt (Specific)
;timer_port 	 equ	40h 		; 8253/4 Counter Timer

kb_data	   	 equ	60h 		; 8042 data port
kb_ctl	   	 equ	61h 		; 8042 ctrl port

;----------------------------------------------------------------------------
; these equs are port definitions for getting the key, sounding the speaker,
; and terminating the interrupt.

status_port	equ	064h		; 8042 status port
inpt_buf_full	equ	002h		; input buffer full
out_buf_full	equ	001h		; output buffer full
both_buf_full	equ	003h		; input and output buffers full on 8042 keyboard status

dis_kbd		equ	0adh		; disable keyboard command
ena_kbd		equ	0aeh		; enable keyboard command

;dis_mou		equ	0a7h		; disable mouse command
;ena_mou		equ	0a8h		; enable mouse command

;----------------------------------------------------------------------------

;TRUE		equ	1			; equ's used in startup.asm
;FALSE		equ	0

ACCESS_FUNCTION equ     0adh            ; ****** any changes here must also be made in DATABLK.C
TSR_HERE	equ	0ACCEh          ; ****** any changes here must also be made in DATABLK.C
TSR_DETECT      equ     0e1h            ; ****** any changes here must also be made in DATABLK.C
TSR_PARAMS      equ     0e2h            ; ****** any changes here must also be made in DATABLK.C

DEFAULT_INTERRUPT 	equ	63h
START_INT_NUMBER	equ	60h
END_INT_NUMBER		equ	67h


newprog	        equ	82h
newstr	        equ	81h

;***************************************************************************
; Definition of BIOS RAM and ROM data area
;***************************************************************************

RAMBIOS	SEGMENT	AT 40H

	org 17h
kb_flag label byte

	org 18h
kb_flag_1 label byte

	org  96h
kb_flag_3 label byte  			; contains Extended Keyboard ID, Rt Alt/Ctrl. and E0/E1 info

	org  65h
vid_flag	label	byte

RAMBIOS	ENDS

ROMBIOS SEGMENT AT 0f000h

	org	0fffeh
computer_model_id	label	byte

;	org	0fffch
;computer_submodel	label	byte
;
;	org	0fffah	
;computer_revision	label	byte
;
	org	0fff5h
computer_bios_date	label	byte

ROMBIOS ENDS

;***************************************************************************
; bits in kb_flag and kb_flag_1
;***************************************************************************


; Flag bits in kb_flag

fCaps		equ	01000000b		; caps (or shift-) lock
fNum		equ	00100000b		; numlock
fScroll		equ	00010000b		; scroll lock
fAlt		equ	00001000b		; ALT key
fCtrl		equ	00000100b		; Control key
fLshift		equ	00000010b		; left shift key
fRshift		equ	00000001b		; right shift key
fShift		equ	fLshift+fRshift		; either or both shift keys

;----------------------------------------------------------------------------
;			       E Q U S

false		equ	0
true		equ	1			; to match Microsoft C's TRUE, when working with C code

break_bit	equ	080h			; set if key was a break
kb_ext_code1	equ	0e0h			; 101/102 extra or extended key hidden codes
kb_ext_code2	equ	0e1h			; 101/102 extra or extended key hidden codes

fLCtrl		equ	00000100b
fLAlt		equ	00001000b
fRCtrl		equ	00010000b
fRAlt		equ	00100000b
fNotToggles   	equ	fRAlt + fRCtrl + fLAlt + fLCtrl

RAlt		equ	0e038h
RCtrl		equ	0e01dh
;kybd_Ret     	equ	01ch			; scan set 1 return key make code 
;kybd_BkSpace	equ	00eh			; scan set 1 backspace key make code
kybd_Command	equ	0EDh
Alt		equ	038h
RShift		equ	036h
Ctrl		equ	01dh
LShift		equ	02ah

no_tone_len	equ	 1			; very soft click
low_tone_len	equ	20			; 0.04 seconds
high_tone_len	equ	80			; 0.04 seconds
click_tone_len	equ	 3			; 

shift_flags	equ	00fh		
clear_flags	equ	not shift_flags
clear_fShift	equ	not fShift

fShift_word	equ	0303h			; left and right shift
fCtrl_word	equ	1414h			; left and right ctl
fAlt_word	equ	2828h			; left and right alt

fLShift_word	equ	0102h			; opposite (rshift) + left shift
;fLShift_flag	equ	0201h			; left and right shift for StickeyKeys

fRShift_word	equ	0201h			; opposite (lshift) + right shift
;fRShift_flag	equ	0102h			; right and left shift for StickeyKeys

fLCtrl_word	equ	0104h			; opposite (rctrl) + ctrl
;fLCtrl_flag	equ	0104h			; left and right ctrl, kb_flag_1 and kb_flag_3

fRCtrl_word	equ	0410h			; opposite (lctrl) + rctrl
;fRCtrl_flag	equ	0401h			; right and left ctrl, kb_flag_1 and kb_flag_3

fLAlt_word	equ	0208h			; opposite (ralt) + alt
;fLAlt_flag	equ	0208h			; left and right alt, kb_flag_1, and kb_flag_3

fRAlt_word	equ	0820h			; opposite (lalt) + ralt
;fRAlt_flag	equ	0802h			; right and left alt, kb_flag_1, and kb_flag_3

dly_dflt_cnt		equ	18		; = 1 sec at 18.2/sec
rpt_dflt_cnt		equ	18		; = 1 sec at 18.2/sec
wait_dflt_cnt		equ	18		; = 1 sec at 18.2/sec
;rec_dflt_cnt		equ	18		; = 1 sec at 18.2/sec.

no_accept_cnt		equ	73		; = 4 sec at 18.2/sec
warning_cnt		equ	218		; = 12 sec at 18.2/sec
toggle_dflt_cnt  	equ	146		; = 8 sec at 18.2/sec
reset_cnt		equ	291		; = 16 sec at 18.2/sec
max_dflt_wait_cnt	equ	36		; = 2 sec at 18.2/sec
tk_reset_cnt		equ	91		; = 5 sec at 18.2/sec

NumLock		equ	045h
CapsLock	equ	03ah
ScrollLock	equ	046h

NumPad_0	equ	00052h
NumPad_1	equ	0004fh
NumPad_2	equ	00050h
NumPad_3	equ	00051h
NumPad_4	equ	0004bh
NumPad_5	equ	0004ch
NumPad_6	equ	0004dh
NumPad_7	equ	00047h
NumPad_8	equ	00048h
NumPad_9	equ	00049h
NumPad_Period	equ	00053h
NumPad_Plus	equ	0004eh
NumPad_Minus	equ	0004ah
NumPad_Times	equ	00037h
NumPad_Enter	equ	0e01ch			; has kb_ext_code_1
NumPad_Divide	equ	0e035h			; has kb_ext_code_1

to_dflt		equ	10			; time out default in minutes	  
to_dflt_cnt	equ	to_dflt*60*18		; *60 for seconds *18 for ticks

;***************************************************************************
;	 System ID byte or word values used for reference only
;***************************************************************************
;-------comp_id=1
;
;IBM_pc			equ	0ffh	    	; IBM PC, all
;				       	
;IBM_xt			equ	0feh	    	; IBM PC XT  BIOS 11/8/82
;
;
;;-------comp_id=2
;
;IBM_xt_1		equ	0fbh	    	; IBM PC XT  revision 1 BIOS 1/10/86 
;IBM_xt_2		equ	0fbh	    	; IBM PC XT  revisiom 2 BIOS 5/9/86
;
;
;;-------comp_id=3
;
;IBM_pcjr		equ	0fdh		; IBM PC JR  BIOS 6/1/83
;
;;-------comp_id=4
;
;IBM_conv		equ	0f9h		; IBM Convertible  BIOS 9/13/85
;
;;-------comp_id=5
;
;IBM_at			equ	0fch		; IBM PC AT  BIOS 1/10/84
;
;;-------comp_id=6
;
;IBM_ps2_30		equ	0fah 		; IBM PS/2 mod 30, 25 (8086 machines ???)  BIOS 9/2/86
;
;;-------comp_id=7
;
;; Old AT
;IBM_at_1		equ	0fch		; IBM PC AT  submodel 0 revision 1 BIOS 6/10/85 (01/10/84 also)
;
;; New AT
;IBM_at_2		equ	0fch		; IBM PC AT  submodel 1 BIOS 11/15/85				       	
;
;
;IBM_xt_286		equ	0fch		; IBM XT/286 submodel 2 BIOS 4/21/86
;						; PS/2 Models 50, 60 have an AT ID! NO KIDDING
;IBM_ps2_50		equ	0fch		; IBM PS/2 mod 50, submodel 4
;IBM_ps2_60		equ	0fch		; IBM PS/2 mod 60, submodel 5
;
;
;;-------comp_id=8
;
;IBM_ps2_80		equ	0f8h		; IBM PS/2 mod 80, submodel 0, 16 Mhz
;IBM_ps2_80		equ	0f8h		; IBM PS/2 mod 80, submodel 1, 20 Mhz
;
;IBM_ps2_55		equ	0f8h		; IBM PS/2 mod 55, submodel 0C
;
;IBM_ps2_70		equ	0f8h		; IBM PS/2 mod 80, submodel 09
;IBM_ps2_70		equ	0f8h		; IBM PS/2 mod 80, submodel 04
;IBM_ps2_70		equ	0f8h		; IBM PS/2 mod 80, submodel 0D


;----------------------------------------------------------------------------------------------------
; We will determine fvideo_type as follows
;
;	1=monocrome display adapter
;	2= open
;	3= CGA
;	4= EGA mono
;	5= EGA color
;	6= MCGA	(multi color graphics adapter ) special PS/2, usually 25/30-8086
;	7= VGA

