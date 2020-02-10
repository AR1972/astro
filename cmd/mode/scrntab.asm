.SALL
	PAGE	,132			;
	TITLE	SCRNTABL.ASM - SCREEN SHIFT TABLE CONTROL FOR MODE COMMAND

;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

;���������������������������������  M A C R O S  ����������������������������������������ͻ
;�											  �

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

ROM_BIOS_SEG EQU 0F000H 		;USED TO ADDRESS INIT TABLE IN ROM
VIDEO_VECTOR EQU 74H			;OFFSET INTO VECTOR TABLE (SEG=0)
					;TO DOUBLE WORD POINTER TO VIDEO PARMS
VIDEO_PARMS_WORD_LEN EQU 32		;NO. WORDS IN VIDEO PARM AREA

;�											  �
;��������������������������������  E Q U A T E S  ���������������������������������������ͼ


PRINTF_CODE SEGMENT PUBLIC
	ASSUME	DS:NOTHING, CS:PRINTF_CODE


;��������������������������������  E X T R N S	�����������������������������������������ͻ
;�											  �

EXTRN	MODELOAD:NEAR
EXTRN	RESSEG:DWORD			;ADD OF POINTER TO RESIDENT CODE
EXTRN	NEW_SCRNTABL:ABS		;OFFSET OF INIT TABLE WITHIN THE RESIDENT CODE
EXTRN	NEW_VIDEO_PARMS_OFFSET:WORD
EXTRN	NEW_VIDEO_PARMS_SEGMENT:WORD

;�											  �
;��������������������������������  E X T R N S	�����������������������������������������ͼ


;��������������������������������  P U B L I C S  ���������������������������������������ͻ
;�											  �
;�											  �
;��������������������������������  P U B L I C S  ���������������������������������������ͼ



ROM_BIOS SEGMENT AT 0F000H

	ORG	0FFFEH

	MACHINE_TYPE LABEL   BYTE	;MACHINE TYPE BURNED IN ROM

ROM_BIOS ENDS

PAGE
;*********************************************************************************************

SCRNTAB PROC	NEAR
PUBLIC	SCRNTAB
;	 GO TO THE 64 BYTES POINTED TO BY THE VECTOR AT 0:74H AND MOVE A COPY
;	 OF THESE BYTES TO THE MODE COMMAND RESIDENT AREA LEAVING DS POINTING TO
;	 THE SEGMENT OF THE TABLE, AND BX CONTAINING THE OFFSET OF THE TABLE.
;	 BIGTOP BIOS OVERWRITES AREA AT 40:90, AS DO OTHER APPS SO MUST LOAD THE
;	 TABLE INTO MODE'S RESIDENT CODE.

 PUSH	 ES			 ;SAVE ADDR OF PROGRAM HEADER PREFIX AREA
 MOV	 AX,0			 ;ADDR OF VECTOR TABLE
 MOV	 DS,AX			 ;SET DATA SEGMENT TO 0 MEMORY
 MOV	 BX,DS:VIDEO_VECTOR+2	 ;SEGMENT OF VIDEO PARMS
 CMP	 BX,ROM_BIOS_SEG
;$IF	 E			 ;IF haven't moved the table THEN
 JNE $$IF1
			    ;load the resident code
    PUSH    DS
    SET     DS,CS	    ;MODELOAD DEPENDS ON DS BEING SEG OF TO BE RES CODE
    CALL    MODELOAD	    ;LOAD RESIDENT CODE OVERLAYING PSP
    POP     DS		    ;RESTORE TO SEGMENT 0
			    ;Save address of resident code
    SET     CS:NEW_VIDEO_PARMS_SEGMENT,< WORD PTR DS:RESSEG+2 >
    MOV     AX,WORD PTR DS:RESSEG	;AX=offset of MODETO within resident code
    ADD     AX,NEW_SCRNTABL		;AX=OFFSET OF VIDEO PARMS FROM RES CODE SEG
    MOV     WORD PTR CS:NEW_VIDEO_PARMS_OFFSET,AX ;SEE "RESCODE"

    MOV     DI,AX	;DI=NEW_VIDEO_PARMS_OFFSET, SET UP ES:DI FOR MOVSW INSTRUCTION
    MOV     AX,CS:NEW_VIDEO_PARMS_SEGMENT
    MOV     ES,AX		;ES=RESIDENT CODE SEGMENT
    MOV     SI,DS:VIDEO_VECTOR	;GET SEGMENT OF VIDEO PARMS
    MOV     DS:VIDEO_VECTOR,DI	;CHANGE VIDEO OFFSET
    MOV     DS:VIDEO_VECTOR+2,ES ;CHANGE VIDEO SEGMENT
    MOV     DS,BX		;CHANGE DS TO ROM SEGMENT
    MOV     CX,VIDEO_PARMS_WORD_LEN ;COUNT TO MOVE 32 WORDS
    CLD 			;INCREMENT SI AND DI WHILE MOVING THE TABLE
    REP     MOVSW		;MOVE 64 BYTES VIDEO PARMS TO RES AREA

;$ENDIF 			 ;I HAVE MOVED IT YET
$$IF1:

 SET	 DS,0
 LDS	 BX,DS:RESSEG	     ;DS:BX POINTS TO RESIDENT CODE
 ADD	 BX,NEW_SCRNTABL     ;DS:BX POINTS TO NEW VIDEO TABLE

 POP	 ES			 ;RESTORE ADDR OF PROGRAM HEADER PREFIX
 RET				 ;RETURN TO MODESCRN PROC

SCRNTAB ENDP


PRINTF_CODE ENDS
	END

