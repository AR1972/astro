;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */
PAGE	,132
TITLE	DOS - CON Code Page Switching Device Driver (INIT)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;  FILENAME:	 INIT.ASM
;  PROGRAM:	 DISPLAY.SYS (Load module)
;  LINK PROCEDURE:  Linkk
;  INSTALLATION:
;
;  This is the routine used to initialize the DOS CPS
;  (Code Page Switching) device driver.  It is linked into the
;  DISPLAY.SYS file at the very end.
;
;  PSEUDO CODE:
;		  INIT PROC
;
;		      not specified yet....patience is a virtue!!!
;
;		  INIT ENDP
;
;
;  DATE:    August 28, 1986
;
;Modification history *********************************************************
;AN001; D358  New device driver INIT function package		  12/07/87 J.K.
;AN002; D493  Undo D358.					  02/24/88 J.K.
;******************************************************************************

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
IF1					;
        ;%out .COMPILING:     INIT.ASM
        ;%out .               � INSTALL CPS-CON DRIVER
        ;%out .               � VERSION 3.30
        ;%out .INCLUDE FILES:
ENDIF					;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;	REFERENCES
;
;	THESE REFERENCES ARE FOR VARIABLES SET WITHIN THE
;	MAIN-LINE PROGRAM, NOT CONTAINED IN THIS MODULE.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;
.XLIST					;
INCLUDE SYSMSG.INC			; WGR				       ;AN000;
MSG_UTILNAME <DISPLAY>			; WGR				       ;AN000;
.LIST					;
					;
					;
CODE	SEGMENT PUBLIC BYTE 'CODE'      ;
	ASSUME	CS:CODE,DS:CODE 	;
	PUBLIC	INIT			;
					;
	EXTRN	ROM_INT_2F:WORD 	;
	EXTRN	ROM_INT_10:WORD 	;
	EXTRN	ROM_INT_1F:WORD 	;
	EXTRN	OLD_INT_1F:WORD 	;
	EXTRN	ROM_INT_44:WORD 	;
	EXTRN	INT_2F_COM:NEAR 	;
	EXTRN	INT_10_COM:NEAR 	;
	EXTRN	EOF_MARKER:BYTE 	;
	EXTRN	ABORT:BYTE		;
	EXTRN	CPD_ACTIVE:WORD 	;
	EXTRN	CPD_CLASS:BYTE		;
	EXTRN	CPD_HDWR_N_MAX:ABS	;
	EXTRN	CPD_DESG_N_MAX:ABS	;
	EXTRN	CPD_HDWR_N:WORD 	;
	EXTRN	CPD_DESG_N:WORD 	;
	EXTRN	CPD_FONTS_N:WORD	;
	EXTRN	IRPT_2:NEAR		;
	EXTRN	IRPT_CMD_EXIT:NEAR	;
	EXTRN	FONT_SIZE:BYTE		;
	EXTRN	LOAD_MECH:BYTE		;
	EXTRN	CPD_FONT_PNTER:WORD	;
	EXTRN	PARSER:NEAR		;
	EXTRN	GET_DEVICE_ID:NEAR	;
	EXTRN	TABLE:BYTE		;
	EXTRN	ASK_BIOS_FONT_SIZE:NEAR ;
	EXTRN	ASK_BIOS_SCAN_LINES:NEAR;
	EXTRN	REDUCED_SUPPORT:ABS	;
	EXTRN	DEV_HDR:WORD		;
	EXTRN	MODE_VALUE:BYTE 	;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;	LOCAL VARIABLES
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
HANDLE_ROUTINES LABEL WORD		; INDEX TABLE OF 'FUNCTIONS' TO
	DW OFFSET HANDLE_DEVICE_NAME	; OPERATE ON THE COMMAND_LINE
	DW OFFSET HANDLE_DEVICE_ID	;
	DW OFFSET HANDLE_HDWR_CP	;
	DW OFFSET HANDLE_DESG_CP	;
LEN_HANDLE_ROUTINES EQU ($-HANDLE_ROUTINES)/2
					;
RESIDENT_END  DW   OFFSET EOF_MARKER	; POINTER TO SEG:OFFSET OF CODE END!
RESIDENT_ENDS DW   SEG CODE		;
					;
DEVICE_ID_INDEX DW	0		; INDEX VALUE FOR DEVICE SUB-TYPE

EndMemSeg	dw	?		;address of the top of memory
					;passed in BreakAddr as input

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;	INCLUDE FILES
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	INCLUDE MACROS.INC		; GENERAL PURPOSE 'MACROs'
	INCLUDE DEF-EQU.INC		; CPS DRIVER STRUCTURES AND EQUATES
	INCLUDE TABLES.INC		; DEVICE SUB-TYPE CONFIGURATION TABLES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;	INITIALIZATION ROUTINE
;
;	THE INIT PROC IS CALLED BY THE DEVICE DRIVER DURING DOS BOOT TIME!
;	THE CALL WILL BE MADE ONCE, AND ONLY ONCE.  THEREFORE, WHEN THIS
;	CODE IS COMPLETED, IT IS DISCARDED INTO THAT 'great bit bucket
;	in the sky!
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
INIT	PROC				;
	PUSH	DS			;
					;
	PUSH	CS			;
	POP	DS			; FIRST SET DS = CS
					;
	PUSH	BX			; WGR				       ;AN000;
	CALL	SYSLOADMSG		; WGR load messages		       ;AN000;
	JNC	INIT_00 		; WGR if no error then continue..      ;AN000;
	CALL	SYSDISPMSG		; WGR display error message..	       ;AN000;
	POP	BX			; WGR				       ;AN000;
        JMP     short INIT_1            ; WGR exit with error status           ;AN000;
INIT_00:				; WGR				       ;AN000;
	POP	BX			; WGR				       ;AN000;
	MOV	RESIDENT_ENDS,CS	; SET INITIAL RESIDENT END SEGMENT
					;
	LES	DI,DWORD PTR BUF.RH_PTRO; GET RH

	mov	ax,es:[di].RH0_ENDS
	mov	EndMemSeg,ax		;get top of memory segment

	CALL	PARSER			;
	JC	INIT_1A 		;
					;
	MOV	SI,OFFSET TABLE 	; ADDRESS [ TABLE 1 ]
	MOV	CX,[SI].NUM_DEVICES	; CHECK THE NUMBER OF DEVICES SPECIFIED
	CMP	CX,ONE			; IF <> 1, THEN WE HAVE A PROBLEM...
	JNE	INIT_1A 		; ERROR
	MOV	SI,[SI].DEVICE1_OFFSET	; MOVE FROM [ TABLE 1 ] TO [ TABLE 2 ]
	MOV	CX,[SI].NUM_ITEMS	;
	LEA	DI,HANDLE_ROUTINES	;
					;
INIT_0: CALL	CS:[DI] 		; CALL ROUTINE WITH '[SI].OFFSET_LIST'
					;
	JC	INIT_1			; ERROR IN ROUTINE (COMMAND_LINE)
	INC	SI			; ADVANCE POINTERS FOR CALL AND
	INC	SI			; THE OFFSET_LIST!
	INC	DI			;
	INC	DI			;
	LOOP	INIT_0			;
					;
	CALL	INSTALL_ID		; CONFIGURE DEVICE DRIVER FOR MATCHED
	JC	INIT_1			; DISPLAY ADAPTER!
					;
	PUSH	BX			;
	MOV	AH,15			; PERFORM CALL TO GET ACTIVE MODE
	INT	10H			;
	MOV	MODE_VALUE,AL		; SET IT IN THE INT10COM.INC ROUTINE
	POP	BX			;
					;
        jmp     short INIT_2                  ;
					;
INIT_1A:				;
	PUSH	AX			; WGR				       ;AN000;
	PUSH	DX			; WGR				       ;AN000;
	MOV	AX,ERROR_2		; PARSING ERROR CONDITION	       ;AN000;
	MOV	DH,UTILITY_MSG_CLASS	; WGR				       ;AN000;
	CALL	ISSUE_MESSAGE		; WGR				       ;AN000;
	POP	DX			; WGR				       ;AN000;
	POP	AX			; WGR				       ;AN000;
					;
INIT_1: XOR	AX,AX			; CLEAR ERROR CODE TO BE RETURNED
	LES	DI,DWORD PTR BUF.RH_PTRO; GET RH
	MOV	RH.RH0_ENDO,AX		; OFFSET = 0
	MOV	RH.RH0_ENDS,CS		;
	MOV	RH.RH0_UNIT,AL		; SET 0 UNITS....
					;
	CMP	NOT_CPS_ID,OFF		; WGR did we find a non-CPS id?        ;AN000;
	JNE	INIT_1B 		; WGR yes....do not set an error       ;AN000;
	OR	BUF.STATUS,103H 	; SET INIT ERROR OCCURRED
        JMP     short INIT_1C           ; WGR                                  ;AN000;
					;
INIT_1B:				;
	OR	BUF.STATUS,AX		; WGR no error code..but do not load   ;AN000;
;	 mov	 RH.RH0_SYSINIT_MSG, 0	 ;AN001;AN002; Ask IBMBIO not to show "Error in CONFIG.SYS lines= #" message
INIT_1C:				;
	PUSH	DI			; 'DETACH' THE LINK LIST TO THE
	MOV	AX,-1			; NEXT DEVICE NAME....THIS MAY not
	LEA	DI,DEV_HDR		; NEED TO BE RESET FOR INIT ERROR....
	MOV	CS:[DI],AX		;
	MOV	CS:[DI+2],AX		;
	POP	DI			;
	STC				; SET CY TO INDICATE FNC DONE
	POP	DS			;
	RET				;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;	NOW MOVE ONTO INSTALLATION OF INT 2F HEX
;
;	SINCE WE ARE GOING TO CHAIN THE INTERRUPT 2FH VECTOR,
;	IT IS IMPORTANT THAT THE lower LEVEL IS VALID!	THIS
;	IS VERIFIED BY SEEING IF THE ORIGINAL VECTOR <> 0000:0000.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
INIT_2: PUSH	ES			;
	XOR	AX,AX			; SET ES TO INTERRUPT REGION
	MOV	ES,AX			;
	MOV	AX,ES:WORD PTR INT_2F_LOW;GET OFFSET VALUE OF INT 2F hex
	MOV	CS:ROM_INT_2F,AX	;		&
	MOV	CX,ES:WORD PTR INT_2F_HI;   ALSO THE SEGMENT VALUE
	MOV	CS:ROM_INT_2F+2,CX	;
	OR	AX,CX			; IS INT 2F hex = 0 ?
	JNE	INIT_6			;
	MOV	AX,OFFSET ABORT 	; YES:	USE IRET AS THE LOWER CODE
	MOV	CS:ROM_INT_2F,AX	;	REFER TO ABORT LABEL FOR IRET!
	MOV	AX,CS			;
	MOV	CS:ROM_INT_2F+2,AX	;
INIT_6: CLI				; NEXT CHANGE THE ACTIVE INT 2F hex
	MOV	ES:WORD PTR INT_2F_LOW,OFFSET INT_2F_COM;
	MOV	ES:WORD PTR INT_2F_HI,CS; TO THE NEWLY LOADED CODE (INT2FCOM)
	STI				; (with interrupts off, of course)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;	READ THE INTERRUPT 1F HEX FOR INT10COM MODULE
;	THIS IS READ DURING INITIAL LOAD TO RECORD THE
;	DEFAULT VALUE.	THE CPS SUPPORT WILL not CHANGE
;	THE INT 1F HEX VECTOR IF IT WAS CHANGED FROM
;	THIS VALUE....ie. GRAFTABL.COM or other HAS THE
;	VECTOR.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	MOV	CX,ES:WORD PTR INT_1F_LOW; GET OFFSET OF INTERRUPT 1FH
	MOV	CS:ROM_INT_1F,CX	;	    &
	MOV	CS:OLD_INT_1F,CX	;
	MOV	CX,ES:WORD PTR INT_1F_HI; SEGMENT OF INTERRUPT 1FH
	MOV	CS:ROM_INT_1F+2,CX	;
	MOV	CS:OLD_INT_1F+2,CX	;
					;
	MOV	CX,ES:WORD PTR INT_44_LOW; GET OFFSET OF INTERRUPT 44H
	MOV	CS:ROM_INT_44,CX	;	    &
	MOV	CX,ES:WORD PTR INT_44_HI; SEGMENT OF INTERRUPT 44H
	MOV	CS:ROM_INT_44+2,CX	;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;	NOW MOVE ONTO INSTALLATION OF INT 10 HEX
;	THE INT 10H VECTOR IS TAKEN TO MONITOR THE
;	MODE_SET INTERFACE (AH=0).  THIS IS THE KEY
;	TO THE CPS SUPPORT MECHANISM...ie. THE ACTIVE
;	CODE PAGE IS LOADED EVERYTIME A MODE_SET OCCURS!
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	MOV	CX,ES:WORD PTR INT_10_LOW; GET OFFSET OF INT 10H
	MOV	CS:ROM_INT_10,CX	;	   &
	MOV	CX,ES:WORD PTR INT_10_HI;  SEGMENT OF INT 10H
	MOV	CS:ROM_INT_10+2,CX	;
	CLI				; INSTALL THE NEW INT 10H CODE
	MOV	ES:WORD PTR INT_10_LOW,OFFSET INT_10_COM;
	MOV	ES:WORD PTR INT_10_HI,CS;
	STI				; (with interrupts off, of course)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;	INSTALL THE DEVICE DRIVER CODE.  THE RESIDENT_END
;	VALUE HAS BEEN SET TO:
;
;		1 - REDUCED_SUPPORT used when #DESG = 0 (CGA, MONO)
;		2 - LAST DESIGNATED BUFFER - calculated by #DESG x #FONTS
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	POP	ES			; RESTORE THE ES REGISTER
	LES	DI,DWORD PTR BUF.RH_PTRO; GET REQUEST HEADER ADDRESS
	MOV	AX,RESIDENT_END 	; GET OFFSET OF RESIDENT END
	MOV	RH.RH0_ENDO,AX		;
INIT_7: MOV	AX,RESIDENT_ENDS	; GET SEGMENT OF RESIDENT END
	MOV	RH.RH0_ENDS,AX		;
	STC				; CY = 1, CALL FULLY SERVICED = ON
	POP	DS			;
	RET				;
INIT	ENDP				;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;	DEVICE NAME PARSING IS LIMITED ONLY TO 1 DEVICE NAME
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
DEVICE_NAMES	DB	"CON     "      ; ONLY ONE VALID NAME FOR DEVICE...
L_DEVICE_NAMES	EQU ($-DEVICE_NAMES)	;
N_DEVICE_NAMES	EQU ($-DEVICE_NAMES)/L_DEVICE_NAMES
					;
HANDLE_DEVICE_NAME PROC 		; [ TABLE 3 ]
	PUSH	CX			;
	PUSH	SI			;
					;
	MOV	SI,[SI].OFFSET_LIST	;
	MOV	CX,[SI] 		; TEST THAT DEVICE NAME IS 8 BYTES
					;
	CMP	CX,L_DEVICE_NAMES	;
	JNE	H_DN_0			; LEN(DEVICE_NAME) <> 8, THEN ERROR
					;
	PUSH	ES			;
	PUSH	DI			;
					;
	PUSH	CS			;
	POP	ES			;
	LEA	DI,DEVICE_NAMES 	; SET COMPARE STRING TO "CON     "
	INC	SI			;
	INC	SI			; ADVANCE SI TO POINT TO DEVICE_NAME
	REPE	CMPSB			; CHECK OUT THE 1 NAME.....
	POP	DI			;
	POP	ES			;
	JNE	H_DN_0			;
					;
	POP	SI			;
	POP	CX			;
	CLC				;
	RET				;
H_DN_0: POP	SI			;
	POP	CX			;
	CALL	ISSUE_ERROR_1		; ERROR OCCURRED...NOW ISSUE THE MESG
					;
	STC				; TELL CALLER ABOUT ERROR!
	RET				;
HANDLE_DEVICE_NAME ENDP 		;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;	THIS ROUTINE IS USED TO VERIFY THE ADAPTER 'NAME' AGAINST
;	THE SUPPORTED ONES.  IF A MATCH IS FOUND THEN THE APPROPRIATE
;	CONFIGURATION TABLE IS LOADED INTO RESIDENT MEMORY, ELSE THE
;	'CY' FLAG IS SET AND AN ERROR IS ISSUED.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ID_NAMES	DB	"EGA     "      ;
L_ID_NAMES	EQU ($-ID_NAMES)	;
		DB	"EGA 8   "      ; INTERNAL USE ONLY
		DB	"EGA 14  "      ; INTERNAL USE ONLY
		DB	"EGA 14M "      ; INTERNAL USE ONLY
		DB	"LCD     "      ;
		DB	"MONO    "      ;
		DB	"CGA     "      ;
N_ID_NAMES	EQU ($-ID_NAMES)/L_ID_NAMES
					;
HANDLE_DEVICE_ID   PROC 		; [ TABLE 4 ]
	PUSH	DI			;
	PUSH	CX			;
	PUSH	SI			;
					;
	CALL	MODIFY_ID_NAME		; THIS CALL IS USED TO CONVERT THE
	JC	HDID_4			; 'EGA' CLASS TYPE INTO SUB-CATEGORY'S
					;
	MOV	SI,[SI].OFFSET_LIST	;
	MOV	CX,[SI] 		; TEST THAT ID_NAME IS 8 BYTES
	CMP	CX,L_ID_NAMES		;
	JNE	HDID_4			;
					;
	INC	SI			;
	INC	SI			; ADVANCE SI TO POINT TO ID_NAME
					;
	LEA	DI,ID_NAMES		; SET COMPARE STRING TO "DEVICEID"
	MOV	CX,N_ID_NAMES		;
					;
HDID_0: PUSH	DI			; SAVE REGISTERS BETWEEN PASSES
	PUSH	ES			;
	PUSH	SI			;
	PUSH	CX			;
					;
	PUSH	CS			;
	POP	ES			;
	MOV	CX,L_ID_NAMES		;
	REPE	CMPSB			; TEST NAME (ALL 8 BYTES) AGAINST
					; THE NEXT NAME!
	POP	CX			;
	POP	SI			;
	POP	ES			;
	POP	DI			;
	JE	HDID_1			;
	ADD	DI,L_ID_NAMES		;
	LOOP	HDID_0			; TRY ALL ENTRIES FOR A MATCH...
HDID_4: 				;
	CMP	NOT_CPS_ID,ON		; WGR did we find a non-CPS id?        ;AN000;
	JNE	HDID_5			; WGR no...not the problem..continue   ;AN000;
	STC				; WGR yes...so no message...but..      ;AN000;
        JMP     short HDID_6            ; WGR set error for return             ;AN000;
					;
HDID_5: 				; WGR				       ;AN000;
	POP	SI			; IF NONE WERE FOUND, THEN ERROR...
	POP	CX			;
	POP	DI			;
					;
	DEC	SI			; BACK TO SUB-TYPE ID AND ISSUE
	DEC	SI			; THE ERROR MESSAGE.
	CALL	ISSUE_ERROR_1		;
	STC				; INFORM CALLER ABOUT THE ERROR!
	RET				;
HDID_1: SUB	CX,N_ID_NAMES		; ID NAME HAS BEEN FOUND
	NEG	CX			; NORMALIZE CX FOR TABLE INDEXING!!
					;
	MOV	DEVICE_ID_INDEX,CX	;
	CLC				;
HDID_6: 				; WGR				       ;AN000;
	POP	SI			;
	POP	CX			;
	POP	DI			;
	RET				;
HANDLE_DEVICE_ID   ENDP 		;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;	THIS ROUTINE CHECKS THE HARDWARE CP VALUE (if specified)!
;	THE VALUE OF THE HARDWARE CP IS NOT VERIFIED, ALLOWING
;	FUTURE ADAPTERS WITH non-437 HARDWARE PAGES.
;
;	IF #HDWR_CP > 1 (FOR ANY DEVICES IN RELEASE 1.00) THEM
;		ERROR
;	   ELSE
;		RECORD VALUE (1 WORD)
;	ENDIF
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
HANDLE_HDWR_CP	   PROC 		; [ TABLE 5 ]
	PUSH	CX			;
	PUSH	SI			;
	MOV	SI,[SI].OFFSET_LIST	;
	MOV	CX,[SI] 		; GET THE NUMBER OF HDWR_CP'S
	CMP	CX,CPD_HDWR_N_MAX	; TEST IF GREATER THAN MAX ALLOWED!
	JA	H_HC_0			;
					;
	CMP	CX,1			; CHECK THE NUMBER OF HDWR CP's
	JA	H_HC_0			; IF MORE THAN ONE..THEN WE HAVE A
					; PROBLEM....TELL USER!
	PUSH	CX			;
	MOV	CX,[SI+2]		; GET THE ACTUAL HARDWARE CP
	CMP	CX,-1			; IS IT INVALID???
	POP	CX			;
	JE	H_HC_0			;
					;
	PUSH	ES			;
	PUSH	DI			;
					;
	PUSH	CS			;
	POP	ES			;
	LEA	DI,CPD_HDWR_N		; POINT DESTINATION TO INTERNAL TABLE
	INC	CX			; ALSO ACCOUNT FOR COUNT N
	REP	MOVSW			; SI ALREADY POINTS TO 'N,CP1,CP2..'
					;
	POP	DI			;
	POP	ES			;
					;
	POP	SI			;
	POP	CX			;
	CLC				;
	RET				;
H_HC_0: POP	SI			;
	POP	CX			;
					;
	PUSH	SI			; HARDWARE CP IN ERROR, PUT 'SI'
	DEC	SI			; BACK TO SUB-TYPE ID AND ISSUE
	DEC	SI			; THE ERROR MESSAGE.
	DEC	SI			; BACK TO SUB-TYPE ID AND ISSUE
	DEC	SI			; THE ERROR MESSAGE.
	CALL	ISSUE_ERROR_1		;
	POP	SI			;
	STC				;
	RET				;
HANDLE_HDWR_CP	   ENDP 		;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;	THIS ROUTINE CHECKS THE LAST PARAMETER IN THE COMMAND LINE
;	IT CONTAINS THE INFORMATION FOR THE NUMBER OF DESIGNATES AND
;	AND THE NUMBER OF DISPLAY FONTS TO BE USED.  ONLY MINIMAL
;	ERROR CHECKING OCCURS HERE.
;
;	THE FOLLOWING IS A LIST OF THE POSSIBLE VALUE IN 'OFFSET_LIST'
;	==============================================================
;
;	COND:	    # 1 		 # 2		      # 3
;		-----------------|--------------------|-----------------
;	TABLE:	   DW	0		DW   1		     DW   2
;		   DW	?		DW   n		     DW   n
;		   DW	?		DW   ?		     DW   m
;
;	DETAILS:   no parameters	only #desg	     both #desg
;		   specified.		specified	     & #font specified
;
;	RESULT:    #DESG = 0		#DESG = n	     #DESG = n
;		   #FONT = 0 (maximum)	#FONT = 0 (maximum)  #FONT = m
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
HANDLE_DESG_CP	   PROC 		; [ TABLE 6 ]
	PUSH	AX			;
	PUSH	CX			;
	PUSH	DX			;
	PUSH	SI			;
	MOV	SI,[SI].OFFSET_LIST	;
					;
	MOV	CX,-1			; SET #DESG DEFAULT = 1
	MOV	DX,-1			; SET #FONT DEFAULT = 0 (maximum)
	MOV	AX,[SI] 		; GET DESCRIPTOR FOR PARAMETER LIST
					;
	OR	AX,AX			; CHECK FOR <CONDITION #1>
	JE	H_DC_2			; SET DRIVER TO DEFAULTS
					;
	MOV	CX,[SI+2]		;
	CMP	CX,CPD_DESG_N_MAX	; CHECK IF THIS VALUE IS ABOVE MAXIMUM!
	JA	H_DC_5			;
	CMP	AX,ONE			; CHECK FOR <CONDITION #2>
	JE	H_DC_2			; SET DRIVER TO DEFAULTS
					;
	MOV	DX,[SI+4]		; HAS TO BE <CONDITION #3>
					;
H_DC_2: MOV	CPD_DESG_N,CX		; RECORD THE #DESG & THE
	MOV	CPD_FONTS_N,DX		; THE #FONT FROM THE SET VALUES
					;
	POP	SI			;
	POP	DX			;
	POP	CX			;
	POP	AX			;
	CLC				;
	RET				;
H_DC_5: POP	SI			;
	POP	DX			;
	POP	CX			;
	PUSH	DX			; WGR				       ;AN000;
	MOV	AX,ERROR_3		; WGR				       ;AN000;
	MOV	DH,UTILITY_MSG_CLASS	; WGR				       ;AN000;
	CALL	ISSUE_MESSAGE		; WGR				       ;AN000;
	POP	DX			; WGR				       ;AN000;
	POP	AX			; WGR				       ;AN000;
	STC				;
	RET				;
HANDLE_DESG_CP	   ENDP 		;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;	INSTALL_ID
;
;	THIS ROUTINE PERFORMS THE DISPLAY_CONFIG TABLE
;	LOAD WHICH IS USED FOR THE REMAINDER OF THE SESSION
;
;	INPUT:
;		DEVICE_ID_INDEX = INDEX TO LIST OF SUPPORTED DEVICES
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
INSTALL_ID	   PROC 		; ROUTINE TO INSTALL INFO FOR DEVICE
	CALL	INSTALL_TABLE		;
	JNC	INSI_3			;
					;
	PUSH	SI			;
	MOV	SI,OFFSET TABLE 	; ADDRESS [ TABLE 1 ]
	MOV	SI,[SI].DEVICE1_OFFSET	; MOVE FROM [ TABLE 1 ] TO [ TABLE 2 ]
;	INC	SI			;
;	INC	SI			;
	CALL	ISSUE_ERROR_1		;
	POP	SI			;
	STC				;
INSI_3: RET				;
INSTALL_ID	   ENDP 		;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
INSTALL_TABLE	PROC			;
	PUSH	AX			;
	PUSH	CX			;
	PUSH	DX			;
	PUSH	DI			;
	PUSH	SI			;
	PUSH	DS			;
	PUSH	ES			;
					;
	PUSH	CS			;
	POP	DS			;
	PUSH	CS			;
	POP	ES			;
					;
	MOV	AX,DEVICE_ID_INDEX	;
	MOV	CX,DC_ENTRY		;
	MUL	CX			;
	LEA	CX,DISPLAY_CONFIG	;
	ADD	CX,AX			;
	ADD	CX,8			; ACCOUNT FOR 8 CHARACTER NAME
	MOV	DI,CX			;
					;
	CALL	INSTALL_MEMORY		;
	JC	IT_5			;
					;
	INC	DI			; ACCOUNT FOR ENCODED MEMORY BYTES
	MOV	SI,[DI] 		;
	PUSH	DI			;
	MOV	CX,NUM_MODES		;
	LEA	DI,LOAD_MECH		; DUPLICATE LOAD_MECH TABLE
	REP	MOVSB			;
	POP	DI			;
	MOV	SI,[DI+2]		;
	MOV	CX,NUM_MODES		;
	LEA	DI,FONT_SIZE		; DUPLICATE FONT_SIZE TABLE
	REP	MOVSB			;
	CLC				;
					;
IT_5:	POP	ES			;
	POP	DS			;
	POP	SI			;
	POP	DI			;
	POP	DX			;
	POP	CX			;
	POP	AX			;
	RET				;
INSTALL_TABLE	ENDP			;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;	INSTALL MEMORY
;
;	ENTRY:
;		ES : DI = POINTER TO ENCODED SCAN LINES ALLOWED
;	EXIT:
;		CREATION OF CPD_FONT_PNTER TABLE
;		CALCULATION OF RESIDENT_END VALUE
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
MEMORY_SIZE_INDEX LABEL WORD		; THIS CORRESPONDS TO THE MEM RQMENTS
	DW	0,0,0,0 		;
	DW     19*256+6 		;
	DW     16*256+6 		;
	DW     14*256+6 		;
	DW	8*256+6 		; BYTE IN 'TABLES.SRC'.
NUM_MEMORY_SIZE EQU ($-MEMORY_SIZE_INDEX)/2
					;
INSTALL_MEMORY	PROC			;
	PUSH	AX			;
	PUSH	BX			;
	PUSH	CX			;
	PUSH	DX			;
	PUSH	SI			;
	PUSH	DI			;
	PUSH	ES			;
					;
	PUSH	CS			;
	POP	ES			; SET EXTRA SEGMENT
					;
	MOV	AL,[DI] 		; GET ENCODED VALUE FROM TABLE
	CALL	ASK_BIOS_SCAN_LINES	; IF PERMITTED...BIOS KNOWS SCAN LINES
					;
IM_0Y:	PUSH	AX			;
;       PUSH    CX                      ;
	MOV	CX,NUM_MEMORY_SIZE	; COUNT THE NUMBER OF FONTS POSSIBLE
	XOR	DX,DX			; DX (INIT=0) WILL = #DIF FONT TYPES
IM_0A:	SAL	AL,1			;
	JNC	IM_0B			;
	INC	DX			;
IM_0B:	LOOP	IM_0A			;
;       POP     CX                      ;
	POP	AX			;
					; ELSE, STAY WITH INITIAL VALUE!!!!
	MOV	CX,CPD_FONTS_N		; FIRST CHECK TO SEE IF #FONTS>TYPE
	CMP	CX,DX			; FIND OUT IF > ALLOWED...
	JBE	IM_0P			; IF NOT, THEN
	MOV	CX,-1			; SET BACK TO DEFAULT VALUE
					;
IM_0P:	CMP	CX,-1			; CHECK FOR FONTS = DEFAULT VALUE
	JNE	IM_0R			;
	MOV	CPD_FONTS_N,DX		; SET TO THE MAXIMUM FOR THE HARDWARE
					;
IM_0R:	MOV	CX,CPD_DESG_N		;
	CMP	CX,-1			; CHECK FOR #DESG = DEFAULT VALUE
	JNE	IM_0T			;
	MOV	CPD_DESG_N,DX		; DX = #FONTS ALLOWED FOR THIS DEVICE
	OR	DX,DX			; TEST IF > 0 ?
	JZ	IM_0T			;
	MOV	CPD_DESG_N,ONE		;
					;
IM_0T:	CMP	CPD_DESG_N,ZERO 	; TEST VALUE OF DESIGNATED CP's

;Myyy;	JE	IM_8			; IF THE #DESG=0, THEN REDUCED SUPPORT
	jne	@f			;Myyy
	jmp	IM_8			;Myyy
@@:					;Myyy

	CMP	CPD_FONTS_N,ZERO	; TEST VALUE OF FONTS, (knowing DESG>0)

;Myyy;	JE	IM_9			; ERROR OCCURRED!!!
	jne	@f			;Myyy
	jmp	IM_9			;Myyy
@@:					;Myyy

IM_0:	MOV	BX,CPD_FONTS_N		; BX=#FONTS REQUESTED
	MOV	CX,NUM_MEMORY_SIZE	; CX = 8 BITS
	LEA	SI,MEMORY_SIZE_INDEX	; SI SETUP FOR MEMORY VALUES, CX=#FONTS
	XOR	DX,DX			; DX=buffer size (INIT=0)
IM_1:	SAL	AL,1			; TEST NEXT BIT FOR ACTIVATION
	JNC	IM_2			; IF NOT ON...THEN IGNORE
	OR	BX,BX			; TEST IF MORE FONTS REQUESTED...
	JE	IM_2			;
	DEC	BX			; LESS #FONTS REQUESTED BY ONE
	ADD	DX,[SI] 		; AND ADD THE LARGEST BUFFER SIZE ON
IM_2:	INC	SI			; ADVANCE MEMORY INDEX POINTER
	INC	SI			; ADVANCE MEMORY INDEX POINTER
	LOOP	IM_1			;
					;
	MOV	AX,DX			; SET AX=completed buffer size
					;
IM_5:	MOV	CX,CPD_DESG_N		; NOW, SETUP POINTERS & CALC FONT BUFFER
	LEA	SI,CPD_FONT_PNTER	; FIND THE DATA_POINTERS
	MOV	DX,RESIDENT_END 	; GET LAST USEABLE ADDRESS

; First try to allocate the buffer in HMA.  If not enough space then
; allocate the buffer in conventional memory.

        push    es
        pop     bx

IM_6:
        push    bx                      ; save es (code segment)

        xchg    ax,bx                   ; bx <-- ax
        mov     ax,4a02h
        int     2fh                     ; ask for suballocation in HMA

        xchg    ax,bx                   ; ax = size of code page buffer
        pop     bx                      ; needed for RESIDENT_ENDS
        cmp     di,-1                   ; if di = 0ffffh means not enough HMA
        je      std_alloc               ; then do the conventional alloc

        mov     [si],di                 ; save the buffer's pointer
        mov     [si+2],es
        jmp     short next_buf

std_alloc:                              ;

;       PUSH    BX                      ; SAVE VALUE OF BX FOR RESTORATION...
;       MOV     BX,ES                   ; SET ES TO BX SO PNTER IS 'BX:DX'
        PUSH    DX                      ; SET SEGMENT:OFFSET OF BUFFER 'X'
	SHR	DX,1			; STRIP OFF LOW 4 BITS OF OFFSET....
	SHR	DX,1			; TO TOP OFF THE 'ES' SEGMENT REGISTER
	SHR	DX,1			;
	SHR	DX,1			;
	ADD	BX,DX			; AND MINIMIZE VALUE OF OFFSET
	POP	DX			;
	AND	DX,000FH		; THIS IS DONE TO PREVENT STRADDLING
			 		; A SEGMENT BOUNDARY WITH THE DATA BUF
	MOV	[SI],DX 		;
	MOV	[SI+2],BX		;
					;
	ADD	DX,AX			;

;
;Myyy - Begin changes
;Check if buffer segment is above end of memory
;
	push	dx
	add	dx,15			;round up
	shr	dx,1			;to nearest para
	shr	dx,1
	shr	dx,1
	shr	dx,1
	add	dx,bx
	jnc	no_ovfl		;no overflow, we are ok
	pop	dx			;overflow, insufficient memory
	jmp	short IM_9
no_ovfl:
	cmp	dx,EndMemSeg		;Seg > EndMem?
	pop	dx
	ja	IM_9			;yes, not enough memory
;
;Myyy - End changes
;
next_buf:
	ADD	SI,4			;
	LOOP	IM_6			;
					;
IM_7A:	MOV	RESIDENT_END,DX 	; SET FINAL OFFSET & SEGMENT TO
	MOV	RESIDENT_ENDS,BX	; TERMINATE THE 'INIT' WITH
;       POP     BX                      ;
	CLC				;
        jmp     short IM_A                    ;
					;
IM_8:	MOV	DX,REDUCED_SUPPORT	; SET POINTER TO REDUCED SUPPORT
;       PUSH    BX                      ;
	MOV	BX,ES			;
	JUMP	IM_7A			;
					;
IM_9:	STC				;
IM_A:	POP	ES			;
	POP	DI			;
	POP	SI			;
	POP	DX			;
	POP	CX			;
	POP	BX			;
	POP	AX			;
	RET				;
INSTALL_MEMORY	ENDP			;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	     ; WGR
;						     ; WGR
;	ISSUE MESSAGE				     ; WGR
;						     ; WGR
;	THIS ROUTINE IS USED TO PERFORM THE MESSAGE  ; WGR
;	RETRIVER.				     ; WGR
;						     ; WGR
;	INPUT:					     ; WGR
;		AX = MESSAGE NUMBER		     ; WGR
;		DH = MESSAGE CLASS		     ; WGR
;						     ; WGR
;	OUTPUT: 				     ; WGR
;		none				     ; WGR
;						     ; WGR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	     ; WGR
ISSUE_MESSAGE	   PROC 		;	     ; WGR		       ;AN000;
	PUSH	DS			; WGR				       ;AN000;
	PUSH	BX			; WGR				       ;AN000;
	PUSH	CX			; WGR				       ;AN000;
	PUSH	CS			; WGR				       ;AN000;
	POP	DS			; WGR				       ;AN000;
	MOV	BX,STDERR		; WGR				       ;AN000;
	XOR	CX,CX			; WGR				       ;AN000;
	XOR	DL,DL			; WGR				       ;AN000;
	CALL	SYSDISPMSG		; WGR				       ;AN000;
	POP	CX			; WGR				       ;AN000;
	POP	BX			; WGR				       ;AN000;
	POP	DS			; WGR				       ;AN000;
	RET				; WGR				       ;AN000;
ISSUE_MESSAGE	   ENDP 		;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;	ISSUE_ERROR_1
;
;	THIS IS USED TO CONSTRUCT AND OUTPUT THE ERROR MESSAGE;
;
;	DEVICE_ID + 'code page driver cannot be initialized'
;
;	THE DEVICE_ID IS PULLED FROM THE COMMAND_PARSER.  IF ONE
;	IS NOT DEFINED OR UNREADABLE, THEN A CONDENSED MESSAGE
;	WITHOUT THE DEVICE_ID IS USED.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

SUB_SIZE	EQU	11		; WGR sublist size		       ;AN000;
LEFT_ASCIIZ	EQU	00010000B	; WGR left-aligned asciiz string       ;AN000;
UNLIMITED	EQU	0		; WGR unlimited message size.	       ;AN000;
					; WGR				       ;AN000;
SUBLIST LABEL	DWORD			; WGR				       ;AN000;
	DB	SUB_SIZE		; WGR				       ;AN000;
	DB	0			; WGR				       ;AN000;
MSG_PTR DW	?			; WGR				       ;AN000;
MSG_SEG DW	SEG CODE		; WGR				       ;AN000;
	DB	1			; WGR				       ;AN000;
	DB	LEFT_ASCIIZ		; WGR				       ;AN000;
	DB	UNLIMITED		; WGR				       ;AN000;
	DB	1			; WGR				       ;AN000;
	DB	" "                     ; WGR                                  ;AN000;
					;
ISSUE_ERROR_1	PROC			;
	PUSH	AX			;
	PUSH	BX			; WGR				       ;AN000;
	PUSH	CX			;
	PUSH	DX			;
	PUSH	SI			;
					;
	MOV	SI,[SI].OFFSET_LIST	; POINT TO NAME TO PRINTOUT
	INC	SI			;
	INC	SI			;
					;
	PUSH	SI			;
	MOV	CX,8			; MAX LENGTH OF NAME
	MOV	AL,' '                  ;
IN_0:	CMP	[SI],AL 		;
	JBE	IN_3			;
	INC	SI			;
	LOOP	IN_0			;
IN_3:	MOV	AL,ZERO 		; SET  0  TO TERMINATE ASCII STRING
	MOV	[SI],AL 		; OVERWRITE LAST BYTE - THIS AFFECTS
					; DATA IMMEDIATELY AFTER NAME.	NOT
					; IMPORTANT SINCE FOLLOWING DATA IS IN
					; ERROR
	POP	SI			; GET START OF NAME
	MOV	MSG_PTR,SI		; WGR				       ;AN000;
	MOV	MSG_SEG,CS		; WGR				       ;AN000;
	MOV	AX,ERROR_1		; WGR				       ;AN000;
	MOV	BX,STDERR		; WGR				       ;AN000;
	MOV	CX,ONE			; WGR				       ;AN000;
	XOR	DL,DL			; WGR				       ;AN000;
	LEA	SI,SUBLIST		; WGR				       ;AN000;
	MOV	DH,UTILITY_MSG_CLASS	; WGR				       ;AN000;
	CALL	SYSDISPMSG		; WGR				       ;AN000;
					; WGR				       ;AN000;
	POP	SI			;
	POP	DX			;
	POP	CX			;
	POP	BX			; WGR				       ;AN000;
	POP	AX			;
	RET				;
ISSUE_ERROR_1	ENDP			;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;	MODIFY_ID_NAME
;
;	THE USER NEED ONLY SPECIFY THE CLASS OF THE DISPLAY ADAPTER.
;	THE AVAILABLE DESCRIPTIONS ARE 'EGA, LCD, CGA, and MONO'.
;	THIS ROUTINE IS USED TO FURTHER DETAIL WHAT TYPE OF 'EGA'
;	CLASS.	THIS IS USED SPECIFICALLY FOR THE EGA (1501200) ADAPTER
;	AND ITS MANY CONFIGURATION SETTINGS.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
MODIFY_NAME	DB	"EGA     "      ;
NEW_NAME	DB	"EGA 14M "      ;
		DB	"EGA 14  "      ;
		DB	"EGA 8   "      ;
NUM_NEW_NAME	EQU ($-NEW_NAME)/L_ID_NAMES
					;
NOT_CPS_ID	DB	OFF		; WGR device id does not support CPS   ;AN000;
					;
MODIFY_ID_NAME	PROC			;
	PUSH	DI			;
	PUSH	AX			;
	PUSH	BX			;
	PUSH	CX			;
	PUSH	SI			;
					;
	MOV	SI,[SI].OFFSET_LIST	; GET START OF ID_NAME
	CMP	WORD PTR [SI],EIGHT	; WGR				       ;AN000;
	JE	MIN_00			; WGR				       ;AN000;
	CALL	GET_DEVICE_ID		; WGR				       ;AN000;
	JNC	MIN_00			; WGR CPS id?...yes continue	       ;AN000;
	MOV	NOT_CPS_ID,ON		; WGR no....set flag		       ;AN000;
        JMP     short MIN_4             ; WGR exit with carry set              ;AN000;
MIN_00: 				; WGR				       ;AN000;
	INC	SI			;
	INC	SI			; ADVANCE SI TO POINT TO ID_NAME
					;
	PUSH	SI			; MOVE A COPY OF THE NAME TO CPD_CLASS
	PUSH	ES			; BEFORE IT IS Revised....
	PUSH	CX			;
	PUSH	CS			;
	POP	ES			;
	MOV	CX,L_ID_NAMES		;
	LEA	DI,CPD_CLASS		;
	REP	MOVSB			;
	POP	CX			;
	POP	ES			;
	POP	SI			;
					;
	LEA	DI,MODIFY_NAME		; SET COMPARE STRING TO "EGA     "
					;
	PUSH	DI			; SAVE REGISTERS BETWEEN PASSES
	PUSH	ES			;
	PUSH	SI			;
					;
	PUSH	CS			;
	POP	ES			;
	MOV	CX,L_ID_NAMES		;
	REPE	CMPSB			; TEST NAME (ALL 8 BYTES) AGAINST
					; THE NEXT NAME!
	POP	SI			;
	POP	ES			;
	POP	DI			;
	JNE	MIN_1			; THIS MAY NOT BE THE MATCH....
					;
	CALL	ASK_BIOS_FONT_SIZE	; CHECK IF THIS IS AN ADVANCED EGA?
	JNC	MIN_1			; IF CY=0, THEN ADVANCED...ELSE,
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	MOV	AH,12H			; SET FOR ALTERNATE SELECT, EGA INFO
	MOV	BL,10H			;
	INT	10H			; ASK BIOS FOR INFORMATION
	CMP	BL,10H			; (CHECK IF THIS CALL IS INVALID?)
	JE	MIN_4			; IF BL UNCHANGED, THEN NOT EGA!
					; ELSE, CL=SWITCH SETTINGS
	MOV	AX,L_ID_NAMES		; SET DEFAULT SUB-TYPE TO 1ST ENTRY
	CMP	BH,ONE			; IF ZERO THEN COLOUR, IF ONE THEN MONO
	JE	MIN_0			;
	ADD	AX,L_ID_NAMES		; MOVE TO NEXT SUB-TYPE
	AND	CL,0FH			; STRIP OFF LEADING NIBBLE FROM SWITCH
	CMP	CL,9			; SETTINGS AND TEST FOR 5154 CONFIG
	JE	MIN_0			;
	CMP	CL,THREE		;
	JE	MIN_0			; IF NOT 5154, 5151; THEN MUST BE 5153!
	ADD	AX,L_ID_NAMES		;
					;
MIN_0:	ADD	DI,AX			; SET UP INDEX REGISTER TO NEW SUBTYPE
	PUSH	DS			;
	PUSH	ES			;
					;
	PUSH	CS			;
	POP	ES			;
	XCHG	SI,DI			; SWITCH SOURCE-DESTINATION REGISTERS
	MOV	CX,L_ID_NAMES		;
	REP	MOVSB			; MOVE IN NEW STRING
	POP	ES			;
	POP	DS			;
					;
MIN_1:	CLC				;
        jmp     short MIN_5                   ;
MIN_4:	STC				;
MIN_5:	POP	SI			; IF NOT FOUND, THEN ERROR...
	POP	CX			;
	POP	BX			;
	POP	AX			;
	POP	DI			;
	RET				;
MODIFY_ID_NAME	ENDP			;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.XLIST
MSG_SERVICES <MSGDATA>			; WGR				       ;AN000;
MSG_SERVICES <DISPLAYmsg,LOADmsg,CHARmsg> ; WGR 			       ;AN000;
MSG_SERVICES <DISPLAY.CL1>		; WGR				       ;AN000;
MSG_SERVICES <DISPLAY.CL2>		; WGR				       ;AN000;
MSG_SERVICES <DISPLAY.CLA>		; WGR				       ;AN000;
.LIST					;
					;
include msgdcl.inc			;
					;
CODE	ENDS				;
	END				;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
