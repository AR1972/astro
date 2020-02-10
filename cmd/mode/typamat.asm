.XLIST
INCLUDE STRUC.INC
.LIST
.SALL
	PAGE	,132			;
	TITLE	TYPAMAT.SAL - TYPAMATIC RATE AND DELAY CONTROL FOR MODE COMMAND

;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */


;�������������������������������������������������������������������������������
;�
;� SET_TYPAMATIC
;� -------------
;� Translate the input parameters into BIOS digestable values and make the
;� required calls.  Typamatic support in BIOS exists only in machines with
;� BIOS dated 11/15/85 or later, XT286 and all PS/2 products.
;�
;�
;�; INPUT:	 A binary value from 1 to 32 indicating the typamatic rate
;�		 desired and a binary value from 1 to 4 indicating the delay.
;�		 Status is not supported on machines known today.
;�		 The translation of the typamatic value to BIOS
;�		 input is 32 - r where 'r' is the input value.  The translation
;�		 from 'd' the input delay value to the BIOS is d - 1.  'r'
;�		 is passed in BL, 'd' is passed in BH.
;�
;�  RETURN:	 none
;�
;�
;�  MESSAGES:	 none
;�
;�
;�
;�  REGISTER
;�  USAGE AND
;�  COMVENTIONS: BX is used to pass parameters to BIOS
;�		 The names used to represent the valid machines are: AT3, XT286,
;�		 PS2Model30, PS2Model50, PS2Model60 and PS2Model80.
;�
;�  ASSUMPTIONS: Input values are valid.
;�
;�
;�  SIDE EFFECT:
;�
;�
;�   �����������������������������������������������������������������������
;�
;� BEGIN
;�
;� IF (machine_type>AT) OR ((machine_type=AT) AND (BIOS_date >= 11/15/85)) OR
;�    (machine_type=XT286) THEN
;�    IF typamatic_rate <> 0 THEN						   �
;�	 MOV	 BL,32-BL	    ;translate typamatic rate			   �
;�    ELSE									   �
;�	 MOV	 BL,previous_typamatic_rate	    ;not specified so no change    �
;�    ENDIF									   �
;�    IF delay <> 0 THEN							   �
;�	 SUB	 BH,1		    ;translate delay				   �
;�    ELSE									   �
;�	 MOV	 BH,previous_delay_rate 					   �
;�    ENDIF									   �
;�    MOV	 AH,set_typamatic_rate_and_delay    ;INT 16 set typamatic function �
;�    MOV	 AL,typamatic_function		    ;set typamatic subfunction	   �
;�    INT	 16H								   �
;� ELSE 									�
;�    queue Function_not_supported						�
;� ENDIF									�
;�										�
;� END										�
;�
;�������������������������������������������������������������������������������




;���������������������������������  M A C R O S  ����������������������������������������ͻ
;�											  �



DISPLAY MACRO	MESSAGE
	MOV	DX,OFFSET MESSAGE
	CALL	PRINTF
ENDM

SET	MACRO	REG,VALUE		;SET REG TO VALUE. DON'T SPECIFY AX FOR REG

	PUSH	AX
	MOV	AX,VALUE
	MOV	REG,AX
	POP	AX

ENDM

;�											  �
;���������������������������������  M A C R O S  ����������������������������������������ͼ


;��������������������������������  E Q U A T E S  ���������������������������������������ͻ
;�											  �

get_installed_state  EQU   0	  ;function of MODE INT 2FH handler
get_typamatic_delay  EQU   04	  ;function of MODE INT 2FH handler
get_typamatic_rate   EQU   03	  ;function of MODE INT 2FH handler
installed	     EQU   0FFH   ;return from MODE INT 2FH handler
no_previous_setting  EQU   0FFH   ;return from MODE INT 2FH handler get setting call
one_half_second      EQU   01	  ;value for BIOS INT 16H
resident_MODE	     EQU   0AFH   ;INT 2F multiplex number for resident part of MODE
save_typamatic_delay EQU   02	  ;function of MODE INT 2FH handler
save_typamatic_rate  EQU   01	  ;function of MODE INT 2FH handler
set_typamatic_rate_and_delay  EQU   3
typamatic_function   EQU   5
ten_chars_per_second EQU   0CH	  ;value for BIOS INT 16H

INCLUDE modequat.inc		 ;definitions of machine types
include version.inc		; defines version of DOS to be built

;�											  �
;��������������������������������  E Q U A T E S  ���������������������������������������ͼ


PRINTF_CODE SEGMENT PUBLIC
	ASSUME	DS:NOTHING, CS:PRINTF_CODE


;��������������������������������  E X T R N S	�����������������������������������������ͻ
;�											  �

EXTRN	 PRINTF:NEAR
EXTRN	 MACHINE_TYPE:BYTE		     ;MACHINE TYPE as determined by "modeleng"
;EXTRN	  typamatic_rate_set_to:WORD
;EXTRN	  chars_per_second:WORD
EXTRN	 noerror:BYTE
;EXTRN	  no_previous_typamatic_rate:WORD
EXTRN	 Function_not_supported:BYTE		      ;see modedefs.inc
;EXTRN	  delay_set_to:WORD
;EXTRN	  second:WORD
;EXTRN	  no_previous_delay_setting:WORD
;EXTRN	  delay_set_to_one_half_second:WORD

;�											  �
;��������������������������������  E X T R N S	�����������������������������������������ͼ


;����������������������������������� D A T A ��������������������������������������������ͻ
;�											  �

thirty_two  DB	  20H			    ;adjustment and work area for typamatic rate

;�											  �
;����������������������������������� D A T A ��������������������������������������������ͼ


;��������������������������������  P U B L I C S  ���������������������������������������ͻ
;�											  �
;�											  �
;��������������������������������  P U B L I C S  ���������������������������������������ͼ




PAGE
;*********************************************************************************************

TYPAMAT PROC	NEAR
PUBLIC	TYPAMAT

if IBMCOPYRIGHT
.IF <machine_type EQ AT4> OR
.IF <machine_type EQ XT286> OR
;.IF <machine_type EQ AT> OR
.IF <machine_type EQ PS2Model30> OR
.IF <machine_type EQ PS2Model50> OR
.IF <machine_type EQ PS2Model60> OR
.IF <machine_type EQ PS2Model80> THEN
endif
;  MOV	   AH,resident_MODE
;  MOV	   AL,get_installed_state
;  INT	   2FH
;  CMP	   AL,installed
;  .IF	   NE					    ;IF the code is not resident THEN
;     CALL    modeload				       ;make it resident
;  .ENDIF					    ;ENDIF
;  CMP	   BL,0 				    ;CASE typamatic_rate OF
;  .IF	   GT						greater than zero:
      SUB     thirty_two,BL			    ;	   MOV	   BL,32-BL	      ;translate typamatic rate
      MOV     BL,thirty_two

;     DISPLAY	typamatic_rate_set_to,BL
;     DISPLAY	chars_per_second

;  .ELSEIF EQ					    ;	zero:
;     MOV     AH,resident_MODE
;     MOV     AL,get_typamatic_rate		    ;	   MOV	   BL,previous_typamatic_rate	      ;not specified so no change
;     INT     2FH
;     CMP     BL,no_previous_setting
;     .IF     E
;	 DISPLAY   no_previous_typamatic_rate
;	 DISPLAY   typamatic_set_to_ten_chars_per_second
;	 MOV	 BL,ten_chars_per_second
;     .ENDIF
;  .ELSE					    ;	less than zero:
;     MOV     AH,resident_MODE
;     MOV     AL,get_typamatic_rate		    ;	   return setting in BL
;     INT     2FH
;     .IF     <BL EQ no_previous_setting> THEN
;	 DISPLAY    no_previous_typamatic_rate
;     .ELSE
;	 SUB	 thirty_two,BL
;	 MOV	 BL,thirty_two			    ;	   translate to humanese
;     .ENDIF
;  .ENDIF					    ;ENDCASE
;  CMP	   BH,0 				    ;CASE delay OF
;  .IF	   GT						>zero:
      DEC     BH				    ;	   SUB	   BH,1 	      ;translate delay


;     DISPLAY	delay_set_to_?_second


;  .ELSEIF EQ					    ;	zero:
;     MOV     AH,resident_MODE
;     MOV     AL,get_typamatic_delay		    ;	   MOV	   BH,previous_delay_rate
;     INT     2FH
;     CMP     BH,no_previous_setting
;     .IF     E
;	 DISPLAY   no_previous_delay_setting
;	 DISPLAY   delay_set_to_one_half_second
;	 MOV	 BH,one_half_second
;     .ENDIF
;  .ELSE					    ;	<zero:
;     MOV     AH,resident_MODE
;     MOV     AL,get_typamatic_rate		    ;	   return setting in BH
;     INT     2FH
;     .IF     <BH EQ no_previous_setting> THEN
;	 DISPLAY    no_previous_delay_setting
;     .ELSE
;	 INC   BH
;     .ENDIF
;  .ENDIF					    ;ENDCASE
;  MOV	   AH,resident_MODE
;  MOV	   AL,save_typamatic_rate
;  INT	   2FH
;  MOV	   AH,resident_MODE
;  MOV	   AL,save_typamatic_delay
;  INT	   2FH
   MOV	   AH,set_typamatic_rate_and_delay	    ;MOV	AH,set_typamatic_rate_and_delay    ;INT 16 set typamatic function
   MOV	   AL,typamatic_function		    ;MOV	AL,typamatic_function		   ;set typamatic subfunction
   INT	   16H					    ;INT	16H
if IBMCOPYRIGHT
.ELSE
   DISPLAY Function_not_supported
   MOV	 noerror,false
.ENDIF
endif

RET

TYPAMAT ENDP


PRINTF_CODE ENDS
	END

