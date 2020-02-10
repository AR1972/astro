;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

PAGE ,132 ;
TITLE MODEECHO - REDIRECT PRINTER OUTPUT FROM PARALLEL TO SERIAL

.XLIST
INCLUDE STRUC.INC
.LIST

INCLUDE common.stc	;definition of parm_list_entry struc

DISPLAY 	MACRO	MSG
	MOV	DX,OFFSET MSG
	CALL	PRINTF
ENDM

SET	MACRO	REG,VALUE		;SET REG TO VALUE. DON'T SPECIFY AX FOR REG
	PUSH	AX
	MOV	AX,VALUE
	MOV	REG,AX
	POP	AX
ENDM


;*****************************************************************
VECT	SEGMENT AT 0
		ORG	50H
VECT14H 	LABEL	DWORD	;RS232 CALL, WILL POINT TO MODETO AFTER
				; RESIDENT CODE IS LOADED.
		ORG	400H
RS232_BASE	DW	?	;ADDR OF RS232 ADAPTER FOR COM1
		DW	?	;ADDR OF RS232 ADAPTER FOR COM2
		DW	?	;ADDR OF RS232 ADAPTER FOR COM3
		DW	?	;ADDR OF RS232 ADAPTER FOR COM4
;
		ORG	408H
PRINTR		DW	?	;LPT1
		DW	?	;LPT2
		DW	?	;LPT3
;
		ORG	530H
RESSEG		DD	?	;VECTOR OF MODETO, IF RESIDENT,OR 0
VECT	ENDS
;*****************************************************************
ascii_to_int	EQU   0FH     ;ANDed with an ascii 0-9 value yeilds binary 0-9
CR		EQU	13	;CARRIAGE RETURN
LF		EQU	10	;LINE FEED
parm_list	EQU	[BP]	;AN000; 					 ;AN000;
TO_SCREEN	EQU	9	;REQUEST OUTPUT TO SCREEN

PUBLIC	       LPTNO	      ;used by modeprin and modepars
PUBLIC		PRINTR
PUBLIC	       display_printer_reroute_status

PAGE
;***************************************************************
PRINTF_CODE	SEGMENT PUBLIC
	ASSUME CS:PRINTF_CODE,DS:PRINTF_CODE,ES:NOTHING

;������������������������������������  D A T A	�����������������������������������������ͻ
;�											  �

two	       DB    2	      ;used to change byte displacement to word disp. See 'modeecho'
LPTNO	       DB    0	      ;holder of ASCII form of the LPT number (1 thru 3)

;�											  �
;������������������������������������  D A T A	�����������������������������������������ͼ

EXTRN PARM1:BYTE,PARM2:BYTE,PARM3:BYTE,MODE:BYTE
;PARM1	DB	10 DUP(0)
;PARM2	DB	0
;PARM3	DB	0
;MODE	DB	0
;FLAG	DB	0
ENDPARM EQU	MODE
EXTRN CRLF:BYTE 		    ;AN000;carriage return linefeed
EXTRN	 parm_lst:BYTE		    ;AN000;
EXTRN	illegal_device_ptr:WORD     ;AN000;pointer to bad com device name string
EXTRN MODELOAD:NEAR
EXTRN PRINTF:NEAR		;FORMATTED "C" LIKE SCREEN OUTPUT ROUTINE
EXTRN REDIRMSG:WORD	     ;CR,LF,"LPT"
EXTRN REDPT:BYTE		;" "
;				": rerouted to COM"
EXTRN REDCOM:BYTE		;" "
;				":",CR,LF,"$"
;
EXTRN NOTREMSG:WORD	     ;CR,LF,"LPT"
EXTRN NOTREDPT:BYTE		 ;" "
;				": not redirected.",CR,LF,"$"
;EXTRN printer_reroute_mask:BYTE    ;mask for testing ptsflag1 to see if a printer is rerouted
EXTRN ptsflag1:BYTE		    ;see rescode.sal
EXTRN  ERR1:WORD	    ;"ILLEGAL DEVICE NAME"
EXTRN resflag2:ABS	    ;see rescode
EXTRN FIXUP:ABS 		;IN MODEPTS, OFFSET USED BY COND. JMP
EXTRN OFFPTS:ABS		;WHERE MODEPTS IS RELATIVE TO BEGINNING
				; OF MODETO
EXTRN NEW_PTSFLAG:ABS		;WHERE PTSFLAG1 IS IN THE RESIDENT CODE
				; RELATIVE TO BEGINING OF MODETO
;
;***************************************************************
VERIFY	PROC NEAR
	PUBLIC	VERIFY
;
	SUB	AX,AX		;CLEAR A REG
	MOV	ES,AX		;SET ES TO LOW MEMORY
;DETERMINE WHAT PRINTER IS INVOLVED (LOOK AT n OF LPTn:)
	MOV	AL,BYTE PTR DS:LPTNO
	MOV	REDPT,AL		;PUT n OF LPTn IN REDIRECT MESSAGE
	MOV	NOTREDPT,AL	 ;AND INTO NOT REDIRECTED MSG
;	IF PRINTER DEVICE IS LPT1
	CMP	AL,"1"
	JNE	ELSEIF01A
;
	  MOV	AH,01H	 ;SET FLAG FOR LPT1
	  MOV	SI,1	;SAVE LPT NUMBER AS OFFSET TO FLAG BYTE ARRAY
;	ELSEIF PRINTER DEVICE IS LPT2
	JMP SHORT ENDIF01
ELSEIF01A:
	CMP	AL,"2"
	JNE	ELSEIF01B
;
	  MOV	AH,02H	 ;SET FLAG FOR LPT2
	  MOV	SI,2	;SAVE LPT NUMBER AS OFFSET TO FLAG BYTE ARRAY
;	ELSEIF PRINTER DEVICE IS LPT3
	JMP SHORT ENDIF01
ELSEIF01B:
;	CMP	AL,"3"
;	JNE	ELSE01
;
	  MOV	AH,04H	 ;SET FLAG FOR LPT3
	  MOV	SI,03	;SAVE LPT NUMBER AS OFFSET TO FLAG BYTE ARRAY
;	ELSE ,SINCE NONE OF THE ABOVE, MUST BE INVALID
;	JMP	SHORT ENDIF01
;ELSE01:
;	  DISPLAY ERR1		 ;FUSS ABOUT INVALID n OF LPTn
;	  MOV	AH,0		;SET FLAG TO INVALID
;	ENDIF ,END OF CHECK FOR DEVICE NO. IN LPTn
ENDIF01:
;AT EXIT: AH=MASK FOR NEW_PTSFLAG REPRESENTING WHICH LPTn, (OR 0 IF n INVALID)
;	AH HAS 0000 0XXX , WHERE BIT ON REPRESENTS:
;		|| --LPT1
;		| ---LPT2
;		----LPT3
;	SI=SET TO HEX 1,2, OR 3 TO BE USED AS OFFSET BEYOND NEW_PTSFLAG
;	TO ADDRESS THE BYTES PTSFLAG2, PTSFLAG3, AND PTSFLAG4
;	(SEE MODECODE FOR BYTE DEFINITIONS)
	RET
VERIFY	ENDP
;***************************************************************


MODEECHO PROC NEAR
	PUBLIC MODEECHO


;INPUT:

;	SI=printer number value (one based)
;	AH=printer number mask: lpt1=1, lpt2=2, lpt3=4
;	AL=com number character
;	all values are known to be valid, but the existence of the com port has
;	  not been checked.


;REGISTER
;  USAGE :  DL - hold the binary COM number
;	    BX - general use




MOV   REDCOM,AL 	;PUT m OF COMm TO REDIRECT MESSAGE
PUSH  ES		;SAVE SEG REG
SET   ES,0

MOV   BH,AH			    ;save the lpt number
AND   AL,ascii_to_int		    ;convert to 1 based integer
MOV   DL,AL			    ;FLAG FOR COM1, see 'ptsflag2' in 'rescode'
;see if the COM# ACTUALLY EXISTS
DEC   AL			    ;AL= 0 based com number
MUL   TWO			    ;AL=disp from 40:0 of com port address word
MOV   AH,BH			    ;restore the lpt number
XOR   BX,BX			    ;prepare for byte move
MOV   BL,AL			    ;BX=disp from 40:0 of com port address word
.IF <ES:RS232_BASE[BX] NE 0> THEN   ;IF the com adapter does exist THEN

   ;AT THIS POINT WE KNOW THAT LPTn AND COMm ARE BOTH LEGAL

   ;THE FOLLOWING IS WHERE THE APPROPRIATE ADDRESS WORD FOR THE PRINTER
   ;INVOLVED IS SET TO 0001H TO INDICATE TO BASIC THAT IT CAN OPEN THE
   ;DEVICE BECAUSE IT HAS BEEN REDIRECTED.  SEE TECH REF PAGE A-2
   ;RS232 AND PRINTER BASE.

	       SAL SI,1 	   ;2*SI TO GET TO THE APPROPRIATE WORD
   ;	   : : IF THE PRINTER ACTUALLY EXISTS
	       CMP ES:PRINTR-2+[SI],0000H  ;IF THE WORD IS NONZERO THE
	       JNE ENDIF10	   ;DEVICE EXISTS SO DON'T PUT A 1 THERE.
		 MOV	   ES:PRINTR-2+[SI],0001H  ;PUT 0001H IN PRINTER WORD n
   ;	   : : ENDIF PRINTER EXISTS
   ENDIF10:
	       SAR SI,1 	   ;RESTORE SI TO n OF LPTn
   ;
	       CALL	   MODELOAD	   ;WITHIN 'MODECODE' MODULE
   ;
   ;AH HAS A MASK SET BY THE 'VERIFY' PROC TO CORRESPOND TO THE LPTn
	       PUSH	   ES	   ;I'M ABOUT TO USE ES, SO I'LL SAVE IT
	       PUSH	   DI
	       LES	   DI,ES:RESSEG    ;FETCH POINTER TO MODETO IN RESIDENT CODE
	       MOV	   BX,NEW_PTSFLAG  ; FROM MODETO INTO BX
	       ADD	   BX,DI   ;ES:BX IS ADDRESS OF PTSFLAG1 IN RESIDENT
				   ; CODE (PROC MODEPTS)
	       OR	   AH,BYTE PTR ES:[BX]	   ;TURN ON THIS PRINTER'S BIT
	       MOV	   BYTE PTR ES:[BX],AH	   ;RECORD WHICH LPT IS TO BE
						   ; REDIRECTED
   ;				   SI=LPTn, DL=COMm
	       DEC	   DL	   ;NOW DL=0 FOR COM1, DL=1 FOR COM2
   ;				   SINCE SI=1, 2, OR 3, THIS PUTS A 0 OR 1 INTO
   ;				    PTSFLAG2, PTSFLAG3, OR PTSFLAG4
	       MOV	   BYTE PTR ES:[BX][SI],DL ;RECORD FOR THIS LPTn
   ;				    WHICH COMm THE LPTn IS REDIRECTED TO
	       POP	   DI
	       POP	   ES	   ;RESTORE SEG REG USED FOR CHECKING/SETTING ADDRESS
			   ; WORDS AND NEW_PTSFLAG
	       CALL	   ECHO     ;CHANGE JMP IN MODEPTS COPY IN LOW MEMORY
   ;				    SO PROPER LPTn GETS REDIRECTED
	       DISPLAY	   CRLF 					;AN000;
	       DISPLAY	   REDIRMSG

.ELSE				    ;valid com name, adapter not there
   MOV	 DI,0			    ;COM device name is always the first parm, the value of "LPTX[:]=value"    ;AN000;
   MOV	 BP,OFFSET parm_lst   ;address the parm list via parm_list which is [BP]
   MOV	 CX,parm_list[DI].value1    ;AN000;					     ;AN000;
   MOV	illegal_device_ptr,CX	    ;AN000;set up message with bad com device name string
   DISPLAY ERR1 		       ;"Illegal device name - COMX"
.ENDIF


	POP	ES			;RESTORE SEG REG
	RET
MODEECHO ENDP
;***************************************************************
MODEECNO PROC NEAR
	PUBLIC MODEECNO
;INPUT: PARM1 AREA HAS:
;	LPTN:
;	OF WHICH VERIFICATION HAS BEEN MADE OF ONLY:
;	L???:
	PUSH	ES		 ;SAVE SEG REG
	CALL	VERIFY		 ;VERIFY THE n OF LPTn
;		SET AH ACCORDINGLY
;		ON EXIT FROM VERIFY, ES=0
;
;	IF n OF LPTn IS OK
	CMP	AH,0
	JE	ENDIF06
;	: IF IT IS POSSIBLE THAT REDIRECTION IS IN EFFECT
	  CMP	WORD PTR ES:530H,0000H	;HAS THE POINTER BEEN SET?
	  JE	ENDIF05 	;SINCE THE POINTER HASN'T BEEN SET, THE
				; CODE HASN'T BEEN LOADED, THEREFORE
				; THE LPT CAN'T BE REDIRECTED, SO THERE
				; IS NO NEED TO UNREDIRECT IT
;		SINCE n OF LPTn IS OK,
;		AND SI HAS 1,2,OR 3 TO CORRESPOND TO n
;		AND AH HAS:
;		0000 0001 - LPT1
;		0000 0010 - LPT2
;		0000 0100 - LPT3
	    XOR 	AH,0FFH 	;NOW AL HAS
;				1111 1110 - LPT1
;				1111 1101 - LPT2
;				1111 1011 - LPT3
	    PUSH	ES	;SAVE ES=0, SET BY VERIFY
	    PUSH	DI		;WILL BE USED AS OFFSET IN SEGMENT 0
				;ES:=O
	    LES 	DI,ES:RESSEG	;FETCH POINTER TO MODETO IN RESIDENT CODE
	    PUSH	BX	;TO AVOID 'TYPE MISMATCH' I PUT OFFSET
	    MOV 	BX,NEW_PTSFLAG	;FROM MODETO INTO BX
	    AND 	AH,BYTE PTR ES:[DI]+[BX] ;CLEAR THIS PRINTER'S BIT
	    MOV 	BYTE PTR ES:[DI]+[BX],AH ;RESTORE Revised FLAG BYTE
	    POP 	BX
	    POP 	DI
	    POP 	ES		;RESTORE ES=0
;
;LPTn IS BEING UNREDIRECTED.  IF THE PRINTER EXISTS, THE ADDRESS WORD FOR
;THAT DEVICE WILL NOT CONTAIN 0001H.  IF IT DOES CONTAIN 0001H, IT MUST BE SET
;TO 0000H SO BASIC WILL NOT ALLOW IT TO BE OPENED.  SEE PROCEDURE MODEECHO
;IN THIS MODULE, AND TECH REF PAGE A-2 (RS232 AND PRINTER BASE).
;
	    SAL 	SI,1	;SI=WORD n FOR LPTn
;	: : IF THE DUMMY PRINTER EXISTS
	    CMP 	ES:PRINTR-2+[SI],0001H	;DOES THE PRINTER EXIST?
	    JNE 	ENDIF07 ;  IF SO, DON'T MESS WITH THE ADDRESS WORD
	      MOV	ES:PRINTR-2+[SI],0000H	;IF NOT, ZERO THE ADDRESS WORD
;	: : ENDIF PRINTER EXISTS
ENDIF07:
	    SAR 	SI,1	;RESTORE SI TO n OF LPTn
;				 SO PROPER LPTn GETS REDIRECTED
;
;IF THE PRINTER EXISTS THE ADDRESS WORD IS NOW WHAT IT STARTED OUT AS
;AT POWER UP INITIALIZATION TIME. IF THE PRINTER DOESN'T EXIST, THE ADDRESS
;WORD IS NOW EQUAL TO 0000H.
;
	    CALL	ECHO		;GO INFORM THE RESIDENT CODE
;
ENDIF05:
	    DISPLAY	CRLF	       ;AN000;carriage return linefeed
	    DISPLAY	NOTREMSG
;	: ENDIF IS REDIRECTION IN EFFECT
;	ENDIF IS n OF LPTn OK?
ENDIF06:
	POP	ES		;RESTORE SEG REG
;
;A RETURN CODE IS PASSED BACK TO THE CALLER, MODEPRIN.
;AH=0 MEANS INVALID LPTn
;AH not= 0 MEANS LPTn IS OK
;
	RET			;RETURN TO CALLER
MODEECNO ENDP
;***************************************************************
ECHO	PROC	NEAR
;'MODEPTS' STARTS WITH THREE INSTRUCTIONS:
;	F6 C2 01	TEST  DL,1
;	75 05		JNZ   CK
;	EA ---- ----	JMP   PRINTER_IO
;		CK: (REDIRECT TO COMm)
;'PTSFLAG' HAS A FLAG BYTE SHOWING WHICH LPTn GETS REDIRECTED.
;THIS CODE HERE IS TO MODIFY THE FIRST TWO INSTRUCTIONS AT 'MODEPTS'
;SO REDIRECTED LPTn GOES TO 'CK:' AND OTHER LPTn GOES TO PRINTER_IO.
;
	PUSH   DI
	;ES:=0
	LES	DI,ES:RESSEG	;FETCH POINTER TO MODETO IN RESIDENT CODE
	PUSH   BX		;TO AVOID 'TYPE MISMATCH' I PUT THE OFFSET
	PUSH	CX		;NEED A HOLDER FOR FIXUP
	MOV	BX,NEW_PTSFLAG	; FROM MODETO INTO BX
	MOV	AL,BYTE PTR ES:[DI]+[BX] ;FIND WHICH LPTn GET REDIRECTED
	MOV	BX,DI		;PUT OFFSET OF MODETO INTO BX
	POP	DI
	ADD	BX,OFFPTS	;ADD OFFSET OF MODEPTS FROM MODETO, NOW
				; ES:BX POINTS TO MODEPTS
	MOV	CX,FIXUP	;HOLD FIXUP IN WORD FORM SO LINKER CAN FILL IN
	CLI			;IF AN INTERRUPT OCCURS HERE THE JUMP WOULD
				; BE INCORRECT
	MOV	BYTE PTR ES:[BX]+4,CL		;SET JUMP TARGET (CK) TO +3BH
	POP	CX		;RETURN FIXUP'S WORD FORM HOLDER
	MOV	AH,0		;CLEAR HIGH BYTE
	SHL	AL,1		;COUNT WAS 0,1,2,3,...7
;				COUNT IS NOW 0,2,4,6...14
	MOV	SI,AX		;GET INDEX OF BR TABLE ENTRY
	JMP	CASE[SI]
;
CASE	EQU THIS WORD
	DW	P0
	DW	P1
	DW	P2
	DW	P3
	DW	P4
	DW	P5
	DW	P6
	DW	P7
;
P0:
;		SINCE NO PRINTER IS TO BE REDIRECTED
	MOV	WORD PTR ES:[BX]+3,00EBH	;MAKE JUMP INTO NO-OP
;		TO CAUSE 'FALL THRU' TO JMP PRINTER_IO INSTR
	JMP	SHORT ENDC
P1:
;	REDIRECT LPT1 ONLY
	MOV	WORD PTR ES:[BX]+2,7403H	;TEST 3 : JZ CK
	JMP	SHORT ENDC
;
P2:
;	REDIRECT LPT2 ONLY
	MOV	WORD PTR ES:[BX]+2,7501H	;TEST 1 : JNZ CK
	JMP	SHORT ENDC
;
P3:
;	REDIRECT LPT1 AND LPT2 ONLY
	MOV	WORD PTR ES:[BX]+2,7402H	; TEST 2 : JZ CK
	JMP	SHORT ENDC
;
P4:
;	REDIRECT LPT3 ONLY
	MOV	WORD PTR ES:[BX]+2,7502H	;TEST 2 :JNZ CK
	JMP	SHORT ENDC
;
P5:
;	REDIRECT LPT1 AND LPT3 ONLY
	MOV	WORD PTR ES:[BX]+2,7401H	;TEST 1 : JZ CK
	JMP	SHORT ENDC
;
P6:
;	REDIRECT LPT2 AND LPT3 ONLY
	MOV	WORD PTR ES:[BX]+2,7503H	;TEST 3 : JNZ CK
	JMP	SHORT ENDC
;
P7:
;	REDIRECT ALL THREE: LPT1, LPT2, AND LPT3
	MOV	WORD PTR ES:[BX]+2,0EB00H	;TEST 0 : JMP CK
ENDC:
	STI			;REENABLE INTERRUPTS
	POP	BX
	RET
ECHO	ENDP


;�������������������������������������������������������������������������������					   ;AN000;
;�
;� DISPLAY_PRINTER_REROUTE_STATUS
;� ------------------------------
;�
;�  Setup message for the reroute status of the specified printer.
;�
;�  INPUT: BL - mask for ptsflag1 (1, 2 or 4)
;�	   REDPT - contains ASCII form of n OF LPTn in redirmsg "LPTn rerouted to COMm"
;�	   NOTREDPT - contains ASCII form of n of LPTn in message "LPTn not rerouted"
;�
;�
;�
;�  RETURN: REDCOM - filled with ASCII form of m OF COMm in MESSAGE "LPTn rerouted to COMn"none
;�
;�
;�
;�  MESSAGES:  REDIRMSG if the printer is rerouted
;�	       NOTREMSG if the printer is not rerouted
;�
;�
;�  REGISTER
;�  USAGE:
;�
;�
;�
;�  ASSUMPTIONS: input is valid
;�
;�
;�  SIDE EFFECT: ES - lost
;�		 SI - lost
;�															   ;AN000;
;�������������������������������������������������������������������������������					   ;AN000;

display_printer_reroute_status	PROC  NEAR

XOR  CX,CX								;AN665;
MOV  ES,CX			       ;set segment to zero		;AN665;

.IF <<ES:WORD PTR resseg> NE 0000H> AND      ;IF the resident code is loaded AND
MOV  ES,ES:WORD PTR resseg[2]					     ;AN665;
;;XOR	BX,BX
;;MOV	BL,printer_reroute_mask
TEST  BL,ES:PTSFLAG1	   ;see if the printer is rerouted
.IF NZ THEN				     ;the printer is rerouted
   SHR	 BL,1	  ;was 1, 2, or 4, now is 0, 1, or 2
;;;INC	 BL	  ;now is 1, 2, or 3
;;;ADD	 BL,resflag2		    ;ES:BX=>the com byte for the desired printer
   ADD	 BX,resflag2		    ;ES:BX=>the com byte for the desired printer
   .SELECT

      .WHEN <<BYTE PTR ES:[BX]> EQ 0>
	 MOV   redcom,"1"           ;printer rerouted to COM1

      .WHEN <<BYTE PTR ES:[BX]> EQ 1>
	 MOV   redcom,"2"           ;printer rerouted to COM2

      .WHEN <<BYTE PTR ES:[BX]> EQ 2>
	 MOV   redcom,"3"           ;printer rerouted to COM3

      .WHEN <<BYTE PTR ES:[BX]> EQ 3>
	 MOV   redcom,"4"           ;printer rerouted to COM4

   .ENDSELECT
   display  redirmsg	   ;"LPTn rerouted to COMm"
.ELSE
   display  notremsg	   ;"LPTn not rerouted"
.ENDIF




RET

display_printer_reroute_status	ENDP

PRINTF_CODE	ENDS
	END
