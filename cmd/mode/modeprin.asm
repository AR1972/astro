PAGE ,132 ;
TITLE MODEPRIN.SAL - PRINTER SUPPORT FOR THE MODE COMMAND

;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

;******************************************************************************
;
;  Change Log:
;
;    Date    Who   #                      Description
;  --------  ---  ---  ------------------------------------------------------
;  03/26/90  MKS  C00  General cleanup.  Making short jumps out of long
;                      jumps, ridding of nops.
;  05/25/90  AKM  C06  Changing printer timeout for first byte of setting up
;                      printer characteristics.  If "Printer Error" message
;                      happens, we get out of the loop.  Bug#1078.
;
;******************************************************************************
.XLIST
INCLUDE STRUC.INC
.LIST
.SALL


;���������������������������������  P R O L O G  ����������������������������������������ͻ                                ;AN000;
;�                                                                                        �                                ;AN000;
                                                                                                                           ;AN000;
;  AX001 - P3976: Need to have all pieces of messages in MODE.SKL so have to
;                 implement the SYSGETMSG method of getting addressability to
;                 the pieces.  This means that the code does a SYSGETMSG call
;                 which returns a pointer (DS:SI) to the message piece.  The
;                 address is then put in the sublist block for the message
;                 being issued.
;�                                                                                        �                                ;AN000;
;���������������������������������  P R O L O G  ����������������������������������������ͼ                                ;AN000;



DISPLAY         MACRO   MSG
        MOV     DX,OFFSET MSG
        CALL    PRINTF
ENDM

PRINT   MACRO
                                MOV DX,WORD PTR PT17    ;PASS PRINTER NUMBER
                                INT 17H               ;OUTPUT TO PRINTER
        ENDM

COMPARE MACRO   STRING, CHAR1, CHAR2, CHAR3
        LOCAL   END_COMPARE

;------------------------------------------------------------------------------------------------
;
;  Compare first 3 characters of STRING with CHAR1, CHAR2, and CHAR3
;
;------------------------------------------------------------------------------------------------

  PUSH  SI
  MOV   SI,0                            ;INITIALIZE INDEX
  CMP   STRING[SI],CHAR1                ;IF first char is a match THEN
  JNE   END_COMPARE
    INC   SI                    ;  GET TO NEXT CHAR OF STRING
    CMP   STRING[SI],CHAR2              ;  IF 2nd char is a match THEN
    JNE   END_COMPARE
      INC   SI
      CMP   STRING[SI],CHAR3            ;    IF all three match then ZR is not set
END_COMPARE:
  POP   SI
ENDM
;
IF_NO_PRINTER_ERROR_THEN        MACRO
        CMP     PRINTER_ERROR,TRUE
        JE      CHK_FOR_P
ENDM
;
JUMP_EQUAL_TO_CHK_FOR_P         MACRO
LOCAL   SKIP_JUMP
;
        JNE     SKIP_JUMP
          JMP   CHK_FOR_P
        SKIP_JUMP:
ENDM


SET     MACRO   REG,VALUE               ;SET REG TO VALUE. DON'T SPECIFY AX FOR REG

        PUSH    AX
        MOV     AX,VALUE
        MOV     REG,AX
        POP     AX

ENDM


set_submessage_ptr   MACRO submessage,message ;PUT pointer to "subMESSAGE" into submessage pointer field of "message".

MOV   AX,submessage                          ;AX=message number                 ;AN001;
MOV   DH,utility_msg_class                   ;DH=message class=utility class    ;AN001;
CALL  SYSGETMSG                              ;DS:SI=>message piece                                ;AN001;
MOV   BP,OFFSET sublist_&message             ;address the sublist control block ;AN001;
MOV   [BP].sublist_off,SI                    ;the sublist now points to the desired message piece ;AN001;
ENDM                                                                                              ;AN001;


;������������������������������  S T R U C T U R E S  �����������������������������������ͻ
;�                                                                                        �

INCLUDE  common.stc        ;contains the following structure

;�                                                                                        �
;������������������������������  S T R U C T U R E S  �����������������������������������ͼ


;
ROM     SEGMENT AT 0
        ORG     530H
RESSEG  LABEL   DWORD           ;VECTOR POINTING TO MODETO IF RESIDENT
ROM     ENDS

;**********************************************************************
PRINTF_CODE   SEGMENT  PUBLIC
        ASSUME  CS:PRINTF_CODE,DS:PRINTF_CODE
;

;��������������������������������  E X T R N S  �����������������������������������������ͻ
;�                                                                                        �

EXTRN   B_item_tag:ABS            ;see modepars
EXTRN   COM1_or_COM2:WORD   ;=>"Must specify COM1 or COM2" message
EXTRN   CTRL_ST:BYTE            ;CONTROL STRING SET UP FROM CONFIGURATION PARAMETERS INPUT
EXTRN   E_item_tag:ABS            ;see modepars
EXTRN   invalid_number_of_parameters:WORD   ;=>"Invalid number of parameters"
EXTRN   noerror:BYTE             ;AN000;
EXTRN   OFFRETRY:ABS            ;OFFSET TO TEST INSTR IN RETRY CODE
EXTRN   not_supported:WORD  ;message telling user he specified "P" on a net printer
EXTRN   PRINTR:WORD             ;PRINTER BASE (40:8), HOLDS PORT ADDRESSES OF PRINTER CARDS
EXTRN   ERR1:BYTE           ;POINTER TO "Invalid parameters"
EXTRN   Illegal_device_ptr:WORD  ;pointer to device name that does not exist
EXTRN   INDEX:WORD              ;INDEX OF REDIRECTED PRINTER IN NETWORK REDIRECTION LIST
EXTRN   IS_LOCAL:BYTE           ;BOOLEAN. INDICATES IF A PRINTER IS LOCAL. INITIALLY TRUE
EXTRN   keyword:ABS
EXTRN   LOCAL_NAME:BYTE         ;FILLED IN BY GET ASSIGN LIST ENTRY FUNCTION CALL
EXTRN   lpt1_retry_type:BYTE     ;byte in resident code for retry mask, see rescode
EXTRN   LPTNO:BYTE              ;see modeecho
EXTRN   machine_type:BYTE       ;see "rescode"
EXTRN   MODEECHO:NEAR           ;SET UP REDIRECTION OF PARALLEL TO SERIAL
EXTRN   MODELENG:NEAR
EXTRN   MODELOAD:NEAR           ;MOVES THE CODE AT MODETO TO 60 AND MAKES IT RESIDENT                                      ;AN000;
EXTRN   NET_ERROR:WORD      ;"NET 042: Unable to do requested comand"                                                      ;AN000;
EXTRN   NONE_item_tag:ABS           ;AN000;see modepars                                                                    ;AN000;
EXTRN   parm_lst:BYTE            ;see modepars.asm                                                                         ;AN000;
EXTRN   P_item_tag:ABS            ;see modepars.asm                                                                        ;AN000;
EXTRN   P14_model_byte:ABS                      ;see "rescode"                                                             ;AN000;
EXTRN   PRINTF:NEAR             ;FORMATTED "C" LIKE SCREEN OUTPUT ROUTINE                                                  ;AN000;
EXTRN   PARM1:BYTE,PARM2:BYTE,PARM3:BYTE,MODE:BYTE
EXTRN   parm_list_holder:WORD    ;address of parsed parameter list                                                         ;AN663;
EXTRN   parms_form:BYTE                                                                                                    ;AN000;
EXTRN   DEVICE:BYTE,PPARM:BYTE                                                                                             ;AN000;
EXTRN   R_item_tag:ABS                                                                                                     ;AN000;
EXTRN   retry_index:WORD          ;see modecom.asm                                                                         ;AN000;

EXTRN   ERR2:WORD       ;CR,LF,"Printer error",BEEP,CR,LF,"$"
EXTRN   PT80:WORD       ;CR,LF,"LPT"
EXTRN   PT80N:BYTE      ;DB " "
;                       ": set for 80",CR,LF,"$"
EXTRN   PT132:WORD      ;CR,LF,"LPT"
EXTRN   PT132N:BYTE     ;" "
;                       ": set for 132",CR,LF,"$"
EXTRN   PTLINES:WORD    ;CR,LF,"Printer lines per inch set",CR,LF,"$"
EXTRN   NORETRY:WORD        ;message number for CR,LF,'No','$' ;AC001;
EXTRN   INFINITE:WORD       ;message number for CR,LF,'Infinite' ;AC001;
EXTRN   REMOTE_DEV:BYTE         ;FILLED IN BY GET ASSIGN LIST ENTRY FUNCTION CALL
EXTRN   RETPARTO:WORD   ;message number for '%1 retry on parallel printer timeout',CR,LF,'$'
EXTRN   sublist_retparto:BYTE    ; control block for "%1 retry on parallel printer timeout" ;AC001;
EXTRN   SYSGETMSG:NEAR           ;AC001;
EXTRN   utility_msg_class:ABS    ;used for input to sysgetmsg
EXTRN   VERIFY:NEAR             ;FINDS IF n OF LPTn IS LEGAL

;�                                                                                        �
;��������������������������������  E X T R N S  �����������������������������������������ͼ


;��������������������������������  E Q U A T E S  ���������������������������������������ͻ
;�                                                                                        �

busy_status EQU   080H           ;flag telling resident code to not change anything
COLON   EQU     ":"             ;CHAR IN "LPT1:"
error_status   EQU   029H           ;status byte indicating the printer is on fire(busy, no paper, I/O error, timeout)
FALSE   EQU     00H
SPACE   EQU     " "             ;THE BLANK CHARACTER
TO_SCREEN EQU   9               ;REQUEST OUTPUT TO SCREEN
INTCONV EQU     48              ;CONVERTS ASCII TO NUMERIC
EIGHTY  EQU     80              ;80 COL PRINTER WIDTH
no_retry_flag  EQU   0           ;no retry active, stored in lptx_retry_type, see rescode.sal
ONE_THIRTY_TWO EQU      132     ;132 COL PRINTER WIDTH
PRTDC2  EQU     18              ;PRINTER CONTROL CHAR FOR 80 COL
PRTSI   EQU     15              ;PRINTER CONTROL CHAR FOR 132 CHAR
PRTCANCEL EQU   24              ;PRINTER CONTROL CHAR FOR CANCEL
CHAR6   EQU     "6"             ;REQUEST FOR 6 LINES PER INCH
CHAR8   EQU     "8"             ;REQUEST FOR 8 LINES PER INCH
L       EQU     "L"
P       EQU     "P"
T       EQU     "T"
R       EQU     "R"
N       EQU     "N"
NULL    EQU     0               ;NULL CHAR
ASC0    EQU     "0"             ;ASCII 0, REQUEST 8 LINES PER INCH
ASC2    EQU     "2"             ;ASCII 2, REQUEST 6 LINES PER INCH
escape  EQU     27              ;ESCAPE PRINTER CONTROL CHAR
PRINTERSETUP    EQU     5E02H   ;FUNCTION CODE FOR PRINTER SET UP DOS CALL
GET_LIST_ENTRY  EQU     5F02H   ;FUNCTION CODE FOR GET ASSIGN LIST ENTRY FROM NETWORK REDIR. LIST
CHAR_DEVICE     EQU     3       ;CHARACTER DEVICE "MACRO" TYPE (IN NETWORK LINGO).
PRN_NO          EQU     3       ;CHARACTER POSITION OF THE PRINTER NUMBER IN LPTn, (ZERO BASED)
ready_status     EQU      90H      ;flag telling resident code to set status indicating printer is ready for another character
UNCHANGED       EQU     -1      ;-1 INDICATES TO SERVER THAT PARAMTER IS UNCHANGED (NOT SPECIFIED)
unspecified    EQU      0FFH     ;AN000;value of parm2 if no lines per inch was specified
TRUE    EQU     0FFH
ENDPARM EQU     MODE
parm_list  EQU   [BP]

;�                                                                                        �
;��������������������������������  E Q U A T E S  ���������������������������������������ͼ


;������������������������������������  D A T A  �����������������������������������������ͻ
;�                                                                                        �

columns_holder  DB   bogus
i        DB    0        ;loop index for retry index calculation
PT17    DW      0       ;SAVES PRINTER NUMBER FOR DX AND INT 17H
LPTN    DB      0       ;SAVES ID OF WHICH LPTn IS REFERENCED
PRINTER_NO      DB      0       ;SAVES LPT NUMBER IN ZERO BASED NUMERICAL FORM
CTRL_ST_LEN     DW      0000    ;HOLDER FOR LENGTH OF THE CONTROL STRING
REDIRECTED      DB      00      ;FLAG TO INDICATE A PRINTER IS ON THE NETWORK
CHARS_LINE      DB      -1      ;HOLDS CHARACTERS PER LINE IN NUMERIC FORM
NO_COLON        DW      00      ;CHAR POSITIONS TO MOVE IF NO COLON INCLUDED IN DEVICE NAME
PTLINES_REQ     DB      00      ;BOOLEAN INDICATOR OF LINES PER INCH REQUESTED
PRINTER_ERROR   DB      00      ;BOOLEAN INDICATOR OF PRINTER NOT THERE OR OFF OR OFFLINE
EIGHTY_CHARS_LINE_REQ   DB      00      ;BOOLEAN, INDICATES IF 80 CHARS/LINE REQUESTED
REQ_132_CHARS_LINE      DB      00      ;BOOLEAN, INDICATES IF 132 CHARS/LINE REQUESTED
OLD_PTR_TIMEOUT DB      00      ;SAVE AREA FOR OLD PRINTER TIMEOUT VALUE   ;C06
PUBLIC  OLD_PTR_TIMEOUT                                                    ;C06

;�                                                                                        �
;������������������������������������  D A T A  �����������������������������������������ͼ


;��������������������������������  P U B L I C S  ���������������������������������������ͻ
;�                                                                                        �

PUBLIC   busy_status          ;used by rescode in parallel retry code
PUBLIC columns_holder                                                                                                       ;AN000;
PUBLIC   error_status         ;used by analyze_and_invoke and rescode
PUBLIC modify_resident_code   ;used by invoke                                                                               ;AN000;
PUBLIC MODEPRIN
PUBLIC   no_retry_flag        ;used by analyze_and_invoke
PUBLIC printer_no             ;needed by modepars
PUBLIC   ready_status         ;used by analyze_and_invoke
PUBLIC set_retry_type         ;used by invoke in turn_off_retry_case

;�                                                                                        �
;��������������������������������  P U B L I C S  ���������������������������������������ͼ


;*******************************************************************
MODEPRIN PROC NEAR


;On entry: "columns_holder" contains 132,80 or 88H.
;          "parm2" contains "[6�8]" (FFH if not specified).
;          "parm_list[retry_index]" contains "X_item_tag" where X is e, b, r, or p

           MOV AL,LPTNO         ;AN000;LPTNO set up by modepars in first_parm_case
           MOV PT80N,AL         ;PUT ASCII PRINTER NUMBER INTO BOTH
           MOV PT132N,AL        ; MESSAGES
           SUB AL,INTCONV+1     ;CONVERT TO INTEGER, MINUS ONE
           MOV PRINTER_NO,AL    ;SAVE ZERO BASED PRINTER NUMBER
           MOV AH,0             ;CLEAR AH
           MOV WORD PTR PT17,AX ;SET UP PRINTER NUMBER FOR INTERRUPT 17H


           MOV DI,0             ;INITIALIZE LENGTH OF CONTROL STRING
           MOV BL,columns_holder    ;BL=binary form of requested chars per line
;    : : : IF REQUEST IS FOR 80 COL
           CMP BL,EIGHTY
           JNE ELSEIF03
;
             MOV CHARS_LINE,BL  ;SAVE CHARACTERS PER LINE
             MOV EIGHTY_CHARS_LINE_REQ,TRUE
             MOV BL,PRTDC2      ;SEND A DC2 CHAR TO SELECT 80 COL
;    : : : ELSEIF SINCE NOT 80, IS REQUEST FOR 132?
           JMP SHORT ENDIF03
ELSEIF03:
           CMP BL,ONE_THIRTY_TWO        ;132?
           JNE ENDIF03                    ;AC000;
;
             MOV CHARS_LINE,BL  ;SAVE CHARACTERS PER LINE
             MOV REQ_132_CHARS_LINE,TRUE
             MOV BL,PRTSI       ;SEND 'SI', CONDENSED PRINT
;    : : : ENDIF ,END IS REQUEST FOR 80 COL
ENDIF03:

;    : : : IF ANYTHING TO PRINT,
           CMP BL,bogus                   ;AC000;
           JE ENDIF04

             MOV BYTE PTR CTRL_ST[DI],BL        ;PUT CONTROL CHAR FOR COLS/LINE IN CONTROL STRING
             INC  DI            ;GET TO NEXT CHAR POSITION IN CONTROL STRING


;    : : : ENDIF END IF ANYTHING TO PRINT? TEST
ENDIF04:

PUBLIC   ENDIF04

;*****************************************************************
;  LOOK AT THE SECOND PARM, CHECKING FOR 6 OR 8 LINES PER INCH
;*****************************************************************
           MOV BL,NULL          ;NULL CHAR, TO BE REPLACED MAYBE
;    : : : IF THERE IS A SECOND PARM,
           CMP DS:PARM2,unspecified                     ;AC000;
           JE ENDIF05
;
;    : : : : IF THE REQUEST FOR 6 LINES PER INCH?
             CMP DS:PARM2,CHAR6
             JNE ELSEIF06
;
               MOV BL,ASC2      ;REQUEST 6 LPI
;    : : : : ELSEIF REQUEST IS FOR 8 LINES PER INCH
             JMP SHORT ENDIF06
ELSEIF06:
             CMP DS:PARM2,CHAR8
             JNE ENDIF06
               MOV BL,ASC0      ;REQUEST 8 LINES PER INCH
;    : : : : ENDIF ,END IS REQUEST FOR 6 LPI? TEST
ENDIF06:
;    : : : ENDIF END IS THERE A SECOND PARM? TEST
ENDIF05:
;    : : : IF 6 OR 8 LINES PER INCH REQUESTED
           CMP BL,0
           JE ENDIF07
             MOV CTRL_ST[DI],escape        ;PUT AN ESCAPE CHAR IN THE CONTROL STRING
             INC  DI                    ;GET TO NEXT CHAR POSITION IN THE CONTROL STRING
;
             MOV BYTE PTR CTRL_ST[DI],BL        ;PUT CONTROL CHAR FOR LINES/INCH IN CONTROL STRING
             INC  DI                    ;GET TO NEXT CHAR POSITION IN THE CONTROL STRING
;
             MOV        PTLINES_REQ,TRUE        ;INDICATE LINES PER INCH SET IF NO PRINTER ERROR
;    : : : ENDIF END IS 6 OR 8 LPI REQUESTED? TEST
ENDIF07:
           CALL VERIFY          ;SEE IF n OF LPTn WAS LEGAL
           MOV  BYTE PTR LPTN,AH        ;SAVE THE n OF LPTn
;
           MOV  BX,0
           PUSH BX
           POP  ES              ;GET ADDRESSABILITY TO PRINTER BASE
           MOV  BL,PRINTER_NO   ;PUT ZERO BASED PRINTER NUMBER INTO BL
           SAL  BL,1            ;CHANGE TO WORD OFFSET FROM PRINTER BASE (40:8)
           CMP  ES:PRINTR[BX],0 ;SEE IF THERE IS NO PORT ADDRESS FOR THE SPECIFIED PRINTER THEN
           JNE  PRINTER_EXISTS
             MOV  IS_LOCAL,FALSE                ;NOT A LOCAL PRINTER
           PRINTER_EXISTS:
;
;**************************************************************************************************
;  SEARCH THE NETWORK REDIRECTION LIST
;
;  IF THE PRINTER IS FOUND IN THE LIST THEN THE CONTROL STRING HAS TO BE SENT TO THE REDIRECTOR,
;  OTHERWISE THE CONTROL STRING WILL BE SENT DIRECTLY TO THE LOCAL PRINTER.
;  AN INDEX WILL BE INITIALIZED TO ZERO.  A GET ASSIGN LIST ENTRY CALL WILL BE MADE, THE DEVICE
;  TYPE IS CHECKED FOR CHARACTER DEVICE TYPE. IF THE ENTRY IS A CHARACTER DEVICE THEN THE NAME IS
;  COMPARED WITH THE NAME OF THE PRINTER TO BE CONFIGURED.  IF THE NAMES MATCH THEN WE KNOW THAT
;  THE PRINTER HAS BEEN PUT ON THE NETWORK, ELSE WE CONTINUE DOWN THE LIST BY INCREMENTING THE
;  INDEX AND MAKING ANOTHER GET ASSIGN LIST ENTRY CALL.  THIS CONTINUES UNTIL THE END OF THE NRL
;  IS REACHED OR THE PRINTER IS FOUND.
;  GET ASSIGN LIST ENTRY CALL WORKS AS FOLLOWS: 5F02H IS PUT IN AX, THE INDEX IS PUT IN BX. ON
;  RETURN DS:SI POINTS TO THE LOCAL NAME, ES:DI POINTS TO REMOTE NAME, CX HAS THE DEVICE TYPE.  IF
;  CARRY IS SET AX HAS ERROR CODE: INVALID FUNCTION (NETWORK SUPPORT IS NOT PRESENT) OR, NO MORE
;  FILES (THE INDEX IS GREATER THAN THE NUMBER OF NETWORK ASSIGNMENTS).
;
;**************************************************************************************************
;
SEE_IF_REDIRECTED:

PUBLIC SEE_IF_REDIRECTED

        MOV     CTRL_ST_LEN,DI          ;SAVE LENGTH OF CONTROL STRING
        DEC     INDEX                   ;SET INDEX TO -1 SO IT WILL BEGIN AT ZERO
SRCH_NRL:
        MOV     AX,GET_LIST_ENTRY       ;SET UP FOR GET ASSIGN LIST ENTRY FUNCTION CALL
        INC     INDEX
        MOV     BX,INDEX                ;BX GETS THE INDEX OF THE ENTRY TO BE FECTHED
        MOV     SI,OFFSET LOCAL_NAME    ;DS:SI POINTS TO HOLDING AREA FOR LOCAL DEVICE NAME
        PUSH    DS
        POP     ES                      ;ES GETS THE SEGMENT OF REMOTE DEVICE NAME HOLDING AREA
        MOV     DI,OFFSET REMOTE_DEV    ;ES:DI POINTS TO (USELESS) REMOTE DEVICE NAME HOLDING AREA
        INT     21H
        JNC     CHK_DEV_TYPE
        JMP     NOT_RED                 ;IF CARRY IS SET THEN THE PRINTER IS NOT REDIRECTED
CHK_DEV_TYPE:
          CMP   BL,CHAR_DEVICE          ;ELSE CHECK THE DEVICE TYPE
          JE    CHECK_NAME              ;IF THE DEVICE TYPE IS CHAR DEVICE THEN COMPARE NAMES
            JMP SRCH_NRL                ;ELSE CONTINUE SEARCH
CHECK_NAME:                     ;SEE IF THE LOCAL NAME IS THE PRINTER TO BE CONFIGURED
;  Count the number of characters in the local name
  MOV   SI,00                           ;INITIALIZE THE CHARACTER COUNTER
COUNT:
  CMP   LOCAL_NAME[SI],NULL             ;WHILE (char<>null) AND (char<>space) DO
  JE    CHECK_1st_3                     ;END OF NAME
    CMP   LOCAL_NAME[SI],SPACE
    JE    CHECK_1st_3                   ;  END OF NAME
      INC   SI                          ;    INCREMENT NUMBER OF CHARS IN THE NAME
  JMP   COUNT                           ;END WHILE non-termination char
CHECK_1st_3:                            ;SEE IF 1st 3 CHARS ARE LPT OR PRN
  CMP   SI,4                            ;IF the name is 4 or less chars THEN
  JG    CONTINUE_SEARCH                 ;NAME IS TOO LONG
    COMPARE   LOCAL_NAME, L, P, T
    JNE       CHECK_FOR_PRN
      MOV       AL,PT80N                        ;PUT PRINTER NUMBER IN AL
      CMP       LOCAL_NAME[PRN_NO],AL           ;CHECK PRINTER NUMBER
      JE        END_CHECK_NAME                  ;FOUND THE PRINTER IN THE LIST
    CHECK_FOR_PRN:
      COMPARE   LOCAL_NAME, P, R, N
      JNE       CONTINUE_SEARCH                 ;NOT REDIRECTED AS PRN EITHER
        CMP       PT80N,1                         ;IF printer to be configured is lpt1 THEN
        JNE       CONTINUE_SEARCH
      JMP SHORT END_CHECK_NAME                  ;FOUND THE PRINTER IN THE LIST
  CONTINUE_SEARCH:
    JMP   SRCH_NRL
END_CHECK_NAME:

PUBLIC END_CHECK_NAME

MOV     REDIRECTED,TRUE                         ;REDIRECTED:=TRUE.  THE PRINTER WAS FOUND IN NRL
;
;**************************************************************************************************
;  INDICATE TO SERVER THAT THE CONFIGURATION OF A NETWORK PRINTER HAS CHANGED.
;
;  SET DS:SI TO POINT TO THE PRINTER NAME, CHARS_LINE HAS THE CHARACTERS PER LINE, PARM2 HAS THE
;  LINES PER INCH IN CHARACTER FORM.
;**************************************************************************************************
;
      CMP  CTRL_ST_LEN,0                ;IF printer configuration has changed THEN
      JUMP_EQUAL_TO_CHK_FOR_P
        MOV  SI,OFFSET LOCAL_NAME       ;DS:SI POINTS TO PRINTER NAME
        MOV  AH,02
        XOR  AL,AL              ;AL=0
        MOV  CL,8                       ;CL HOLDS SHIFT COUNT.  8 IS NUMBER OF BITS NEEDED FOR 132.
        MOV  DL,CHARS_LINE              ;MOV 80 or 132 or FF INTO ACCUMULATOR REGISTER
        ROR  DL,1                       ;IF (DL=50H) OR (DL=84H) THEN DL[7]=0  ELSE DL=FFH
        SAR  DL,CL                      ;IF (CHARS_LINE = 80) OR (CHARS_LINE = 132) THEN
        MOV  BH,DL                        ;BH=0  ELSE  BH=BL=FFH
        MOV  BL,CHARS_LINE              ;BX= -1 or 80 or 132
        MOV  CL,PARM2
        CMP  PARM2,bogus                ;IF THE LINES PER INCH WASN'T SPECIFIED THEN DON'T CONVERT
        JNE  CONVERT
          MOV  CH,UNCHANGED               ;CX=-1 (FFFF)
;C00      JMP  CALL_SERVER
          JMP  SHORT CALL_SERVER                                        ;C00
        CONVERT:                        ;ELSE
          XOR  CH,CH                      ;CLEAR CH
          SUB  CX,INTCONV                 ;CHANGE LINES PER INCH TO NUMERIC FORM
        CALL_SERVER:                    ;ENDIF
        INT  2AH                        ;CALL SERVER
;
;**************************************************************************************************
;  SET UP REDIRECTOR WITH CONTROL STRING BUFFER VIA A PRINTER SET UP CALL.
;
;  DI HAS LENGTH OF THE CONTROL STRING.  NEED TO SET DS:SI TO THE POINT TO THE CONTROL STRING
;  BUFFER, PUT THE LENGTH IN CX, AND THE NETWORK REDIRECTION LIST INDEX FOR THE PRINTER IN BX.
;**************************************************************************************************
;
      CMP       CTRL_ST_LEN,0         ;IF there is something to send to a printer THEN
      JUMP_EQUAL_TO_CHK_FOR_P
        MOV     AX,PRINTERSETUP         ;SET UP FOR PRINTER SET UP FUNCTION CALL.
        MOV     SI,OFFSET CTRL_ST       ;DS HAS SEG OF CONTROL ST. BUFFER, DS:SI POINTS TO BUFFER
        MOV     CX,CTRL_ST_LEN          ;CX GETS LENGTH OF CONTROL STRING BUFFER.
        MOV     BX,INDEX                ;BX GETS NRL INDEX OF REDIRECTED PRINTER.
        INT     21H                     ;PERFORM PRINTER SET UP.
;       $IF     C                               ;IF CARRY THERE IS A DESCREPENCY BETWEEN
        JNC $$IF1
           DISPLAY   NET_ERROR                  ;GET ASS LIST ENTRY AND PRINTER SET UP
;       $ELSE                                   ;REDIRECTOR IS RESIDENT AND PRINTER SETUP
        JMP SHORT $$EN1
$$IF1:
                                                ;CALL WAS SUCCESSFUL SO TELL USER WHAT
           CMP     EIGHTY_CHARS_LINE_REQ,TRUE      ;HAPPENED
;          $IF     E                               ;IF 80 chars/line requested THEN
           JNE $$IF3
             DISPLAY       PT80                    ;  WRITELN("LPT? set for 80")
;          $ELSE                                   ;ELSE
           JMP SHORT $$EN3
$$IF3:
              CMP   REQ_132_CHARS_LINE,TRUE
;             $IF   E                              ;  IF 132 chars/line requested THEN
              JNE $$IF5
                 DISPLAY     PT132                 ;    WRITELN("LPT? set for 132")
;             $ENDIF                               ;  ENDIF
$$IF5:
;          $ENDIF                                  ;ENDIF
$$EN3:
           CMP     PTLINES_REQ,TRUE                ;IF lines/inch requested THEN
;          $IF   E
           JNE $$IF8
              DISPLAY       PTLINES                ;  WRITELN("Printer lines per inch set")
;          $ENDIF                                  ;ENDIF
$$IF8:
;       $ENDIF                                  ;ENDIF
$$EN1:
        JMP     CHK_FOR_P                                                  ;C06
;C06    JMP     SHORT CHK_FOR_P                                         ;C00
;******************************************************************************************
; SEND THE CONTROL STRING TO THE LOCAL PRINTER

; EACH CHARACTER OF THE CONTROL STRING IS TAKEN OUT OF THE BUFFER "CTRL_ST" AND SENT TO THE
; PRINTER STARTING WITH THE FIRST CHARACTER.  ON ENTRY DI HAS THE NUMBER OF CHARACTERS IN
; THE CONTROL STRING.  SI IS USED TO INDEX INTO THE CONTROL STRING.
;******************************************************************************************
;
NOT_RED:
      CMP       IS_LOCAL,TRUE   ;IF the device is local THEN
      JE        ELSE02
        JMP     ELSE01    ;or on the network
ELSE02:
      CMP       CTRL_ST_LEN,0         ;IF there is something to send to a printer THEN
;C06     JE     CHK_FOR_P
        JNE     NO_P_CHK                                                   ;C06
        JMP     CHK_FOR_P                                                  ;C06
NO_P_CHK:                                                                  ;C06
        MOV     SI,0            ;INITIALIZE CHARACTER POSITION INDEX FOR CONTROL STRING
        PUSH    ES                                                         ;C06
        PUSH    BX                                                         ;C06
        MOV     AX,0040H                ;SETUP LOW MEMORY AREA SEGMENT     ;C06
        MOV     ES,AX                                                      ;C06
        MOV     BX,WORD PTR PT17                                           ;C06
        ADD     BX,0078H                ;GET CORRECT PRINTER TIME-OUT VALUE;C06
        MOV     AL,BYTE PTR ES:[BX]                                        ;C06
        MOV     OLD_PTR_TIMEOUT,0                                          ;C06
        CMP     AL,3                    ;DON'T CHANGE IF ALREADY LOW       ;C06
        JBE     NO_TIMEOUT_CHG                                             ;C06
        MOV     OLD_PTR_TIMEOUT,AL      ;SAVE PRINTER TIME-OUT VALUE       ;C06
        MOV     BYTE PTR ES:[BX],3      ;SET IT TO A LOW VALUE             ;C06
NO_TIMEOUT_CHG:                                                            ;C06
        POP     BX                                                         ;C06
        POP     ES                                                         ;C06
FOR:                            ;FOR each_char_in_control_string DO.  FOR DI=no_chars DOWN TO 0 DO
        DEC     CTRL_ST_LEN     ;DECREMENT LOOP COUNTER
        MOV     AH,NULL                         ;CLEAR ERROR CODE FROM AH
        MOV     AL,BYTE PTR CTRL_ST[SI]         ;MOVE NEXT CONTROL CHAR TO AL
        CALL    OUTCHR                          ;SEND THE CHARACTER TO THE PRINTER, HANDLING ERRORS
        CMP     PRINTER_ERROR,FALSE     ;EXIT LOOP IF PRINTER ERROR        ;C06
        JNE     END_OUTCHR                                                 ;C06
        INC     SI                      ;GET TO NEXT CHAR POSITION IN CONTROL STRING
        CMP     CTRL_ST_LEN,0           ;CHECK IF ALL CHARACTERS HAVE BEEN SENT
        JNE     FOR                     ;LOOP UNTIL ALL CONTROL CHARACTERS HAVE BEEN SENT
;
END_OUTCHR:                                                                ;C06
        CMP     OLD_PTR_TIMEOUT,0       ;IF TIME-OUT VALUE CHANGED         ;C06
        JZ      NO_TIMEOUT_REST                                            ;C06
        PUSH    ES                                                         ;C06
        PUSH    BX                                                         ;C06
        MOV     AX,0040H                                                   ;C06
        MOV     ES,AX                                                      ;C06
        MOV     BX,WORD PTR PT17                                           ;C06
        ADD     BX,0078H                                                   ;C06
        MOV     AL,OLD_PTR_TIMEOUT      ;RESTORE SAVED PRINTER TIMEOUT     ;C06
        MOV     BYTE PTR ES:[BX],AL                                        ;C06
        POP     BX                                                         ;C06
        POP     ES                                                         ;C06
NO_TIMEOUT_REST:                                                           ;C06
      IF_NO_PRINTER_ERROR_THEN
        CMP     EIGHTY_CHARS_LINE_REQ,TRUE      ;IF 80 chars/line requested THEN
        JNE     WAS_132_CHARS_LINE_REQ
          DISPLAY       PT80                    ;  WRITELN("LPT? set for 80")
          JMP SHORT     WAS_LINES_INCH_SPEC
        WAS_132_CHARS_LINE_REQ:                 ;ELSE
          CMP   REQ_132_CHARS_LINE,TRUE         ;  IF 132 chars/line requested THEN
          JNE   WAS_LINES_INCH_SPEC
            DISPLAY     PT132                   ;    WRITELN("LPT? set for 132")  ENDIF
        WAS_LINES_INCH_SPEC:                    ;ENDIF
        CMP     PTLINES_REQ,TRUE                ;IF lines/inch requested THEN
        JNE     LINES_NOT_REQ
          DISPLAY       PTLINES                 ;  WRITELN("Printer lines per inch set")
        LINES_NOT_REQ:                          ;ENDIF
;
;****************************************************************
;   CALL PROCEDURE TO SET THE RETRY FLAG
;****************************************************************
;
CHK_FOR_P:

public CHK_FOR_P

        CALL set_retry_type
        CALL modify_resident_code


     JMP    SHORT ENDIF01
ELSE01:

PUBLIC ELSE01

       MOV   DI,0                      ;the device name is always the first parm                                         ;AN000;
       MOV   BP,OFFSET parm_lst   ;address the parm list via parm_list which is [BP]                                  ;AN000;
       MOV   BX,parm_list[DI].value1                                                     ;AN000;
       MOV  illegal_device_ptr,BX
       MOV  BYTE PTR [BX][4],0       ;AN000;chop off the string at 4, so "LPT1132" will be displayed as "LPT1"
       DISPLAY err1                   ;AN000;"Illegal device name - LPTX"
       MOV  noerror,false              ;AN000;
;    ENDIF COLON IS MISSING
ENDIF01:
;     $ENDIF ;there was a chance that the parameters were valid
;  $ENDIF ;there were enough paramters specified

   RET                        ;RETURN TO MODE MAIN ROUTINE

MODEPRIN ENDP

;******************************************************************************

modify_resident_code PROC  NEAR                                                                                            ;AN000;


        CALL LOADED_YET      ;on return ES:DI points to res copy of "modeto" if loaded
;    : :IF RESIDENT CODE IS ALREADY LOADED
        .IF Z THEN NEAR
;                    MODIFY LOADED CODE TO REFLECT WHO GETS RETRIED NOW
           MOV  BX,OFFSET lpt1_retry_type    ;BX=> first of 3 retry mask bytes                                             ;AC000;
           XOR  SI,SI    ;clear code modification index                                                     ;AN000;
           .FOR DI = 0 TO 2    ;FOR LPT1 TO LPT3 check the retry mask byte                                                ;AN000;
             .IF <<BYTE PTR ES:[BX][DI]> NE no_retry_flag> THEN    ;IF at least one type of retry on THEN         ;AN000;
                OR SI,8     ;OR in 00001000 which shifts into proper position                                              ;AN000;
             .ENDIF                                                                                                        ;AN000;
             SHR   SI,1                                                                                                    ;AN000;
           .NEXT DI       ;DI=1 or 2, SI=0,1 ,2 ,3 ,4 ,5 ,6 or 7                                             ;AN000;
           SHL  SI,1    ;SI=0, 2, 4, ... , 14, INDEX TO SHOW WHICH LPTns to be retried                                    ;AC000;
           MOV  BX,OFFRETRY     ;OFFSET TO TEST INSTR IN RETRY CODE
           CLI          ;DISABLE INTERRUPTS
           MOV  BYTE PTR ES:[BX]+4,5    ;SET JMP TARGET TO +5
           JMP  CASE[SI]        ;CALL BRANCH TABLE
;
CASE    DW      P0
        DW      P1
        DW      P2
        DW      P3
        DW      P4
        DW      P5
        DW      P6
        DW      P7
;
P0:
;                               SINCE NO PRINTER IS TO BE RETRIED
             MOV        WORD PTR ES:[BX]+3,00EBH        ;MAKE JUMP INTO NOP
;                               TO CAUSE FALL THRU TO JMP PRINTER_IO INSTR
             JMP        SHORT ENDC
;
P1:
;       RETRY LPT1 ONLY
             MOV        WORD PTR ES:[BX]+2,7403H        ;TEST 3 : JZ RT
             JMP        SHORT ENDC
;
P2:
;       RETRY LPT2 ONLY
             MOV        WORD PTR ES:[BX]+2,7501H        ;TEST 1 : JNZ RT
             JMP        SHORT ENDC
;
P3:
;       RETRY LPT1 AND LPT2 ONLY
             MOV        WORD PTR ES:[BX]+2,7402H        ;TEST 2 : JZ RT
             JMP        SHORT ENDC
;
P4:
;       REDIRECT LPT3 ONLY
             MOV        WORD PTR ES:[BX]+2,7502H        ;TEST 2 : JNZ RT
             JMP        SHORT ENDC
;
P5:
;       REDIRECT LPT1 AND LPT3 ONLY
             MOV        WORD PTR ES:[BX]+2,7401H        ;TEST 1 : JZ RT
             JMP        SHORT ENDC
;
P6:
;       REDIRECT LPT2 AND LPT3 ONLY
             MOV        WORD PTR ES:[BX]+2,7503H        ;TEST 3 : JNZ RT
             JMP        SHORT ENDC
;
P7:
;       REDIRECT ALL THREE: LPT1, LPT2, AND LPT3
             MOV        WORD PTR ES:[BX]+2,0EB00H       ;TEST 0 : JMP SHORT RT
;
ENDC:
             STI                ;REENABLE INTERRUPTS
;    : : : ENDIF RESIDENT CODE IS ALREADY LOADED
        .ENDIF

RET                                                                                                                         ;AN000;

modify_resident_code ENDP                                                                                                   ;AN000;


;**********************************************************
;FIRST_INSTR EQU 0C2F6H          ;THE FIRST INSTRUCTION OF THE LOADED CODE
;                       SEE THE RESIDENT CODE MODULE FOR ENTRY SYMBOL MODETO
;                       THE FIRST INSTRUCTION THERE IS:
;                               TEST DL,1
;                       WHICH ASSEMBLES AS:
;                               F6 C2 01


LOADED_YET PROC NEAR


;       ON EXIT, THE ZERO FLAG REFLECTS THE LOADED STATE
;       ES:DI=ADDR OF MODETO, OR ZERO
;       Z=ON, LOADED
;       Z=OFF,NOT LOADED YET
;
        PUSH    AX              ;SAVE REG
;
        SUB     AX,AX           ;ZERO A REG
        MOV     ES,AX           ;SET SEGREG TO VECTORS AT 0
        LES     DI,ES:RESSEG    ;GET ADDR OF RESIDENT CODE, IF THERE, ES:DI points to 'modeto' or is 0
        CMP     DI,0            ;see if something at 50:30
;       $IF     NE              ;IF code loaded THEN
        JE $$IF11
           CMP  AX,AX              ;SET THE ZERO FLAG TO RELFECT IT IS LOADED
;       $ELSE
        JMP SHORT $$EN11
$$IF11:
           CMP  AX,0FFH            ;RESET THE ZERO FLAG TO REFLECT IT IS NOT LOADED
;       $ENDIF
$$EN11:
;
        POP     AX              ;RESTORE CALLER'S REGS
        RET
LOADED_YET ENDP

;*******************************************************************
OUTCHR PROC NEAR
     PRINT                      ;OUTPUT CHARACTER TO PRINTER
     AND    AH,0A9H             ;MASK OFF ERROR BITS
;    IF WE GOT AN ERROR RETURN CODE
     CMP    AH,NULL
     JE    ENDIF02
;
       CMP      PRINTER_ERROR,TRUE
       JE       ALREADY_YELLED
         DISPLAY ERR2           ;DISPLAY ERROR MESSAGE
         MOV    PRINTER_ERROR,TRUE
       ALREADY_YELLED:
;    ENDIF    ,END GOT AN ERROR RETURN CODE? TEST
ENDIF02:
     RET                        ;RETURN TO MAIN PROC
OUTCHR    ENDP
;�����������������������������������������������������������������������������Ŀ
;�                                                                             �
;� SET_retry_type                                                              �
;� --------------                                                              �
;�                                                                             �
;�  Set the resident retry mask to for all LPTs
;�                                                                             �
;�  INPUT:  device - holds '1', '2', or '3' (ascii) for x of lptx.             �
;�          retry_index - holds index value for the parsed retry parameter.    �
;�          redirected - holds true/false value for redirected status of lptx. �
;�          parm_list_holder - holds offset of parameter list.                 �
;�          resseg - holds offset of resident code in memory                   �
;�                                                                             �
;�                                                                             �
;�  RETURN:
;�
;�                                                                             �
;�                                                                             �
;�  MESSAGES: none
;�                                                                             �
;�                                                                             �
;�  REGISTER                                                                   �
;�  USAGE:      AL -
;�
;�              CL -
;�              ES -
;�              BP -
;�              DI -
;�              DL -
;�                                                                             �
;�                                                                             �
;�  PSUEDO CODE:                                                               �
;�
;�                                                                             �
;�      SAVE REGISTERS                                                         �
;�      SET UP SEGMENT REGISTER
;�      IF <RETRY REQUESTED>                                                   �
;�         IF <PRINTER IS REDIRECTED>                                          �
;�            PRINT ERROR MESSAGE - not supported on network printer.          �
;�         ELSE                                                                �
;�            SET UP PARAMETER LIST STRUCTURE                                  �
;�            SET BIT MASK FOR TYPE OF RETRY AND SET pparm TO PROPER LETTER    �
;�            LOAD RESIDENT CODE IF NEEDED                                     �
;�            SET AND STORE NEW lpt retry mask
;�         ENDIF                                                               �
;�      ELSEIF <RESIDENT CODE ALREADY LOADED>                                  �
;�         IF <POSITIONAL PARAMETER SPECIFIED>                                 �
;�            SET FLAG TO ZERO, SET pparm TO PROPER LETTER                     �
;�         ELSE                                                                �
;�            SET pparm TO PROPER LETTER FOR CURRENT SETTING                   �
;�         ENDIF                                                               �
;�      ELSE                                                                   �
;�         SET pparm TO '-'                                                    �
;�      ENDIF                                                                  �
;�      RESTORE REGISTERS                                                      �
;�      RETURN                                                                 �
;�                                                                             �
;�                                                                             �
;�  SIDE EFFECT:
;�                                                                             �
;�������������������������������������������������������������������������������
;
set_retry_type PROC NEAR                                                                                                    ;AN663;
                                                                                                                            ;AN663;

PUSH ES                   ;save registers                                                                             ;AN663;
PUSH DI                                                                                                               ;AN663;
PUSH AX                                                                                                               ;AN663;
PUSH BX
PUSH DX                                                                                                               ;AN663;
                                                                                                                      ;AN663;
XOR  BX,BX                ;clear a reg                                                                                ;AN663;
MOV  ES,BX                ;set to segment at 0                                                                        ;AN663;
MOV  BL,device                                                                                                        ;AN663;
AND  BL,07H                                                                                                           ;AN663;
DEC  BL                    ;BX=zero based binary printer number                                           ;AN663;
                                                                                                                      ;AN663;
.IF <retry_index NE 0> THEN NEAR    ;IF retry requested on this invokation THEN                                     ;AN663;
                                                                                                                      ;AN663;
   .IF <redirected EQ true> THEN                                                                                      ;AN663;
                                                                                                                      ;AN663;
      display not_supported ;infinite retry not supported on network printer                                          ;AN663;
      jmp     srt_no_display              ;M003; Do not display retry if network printer
                                                                                                                      ;AN663;
   .ELSE NEAR                             ;not a network printer                                                      ;AN663;
                                                                                                                      ;AN663;
      MOV  DI,retry_index                                                                                             ;AN663;
      MOV  BP,parm_list_holder            ;set up addressability to the list of parsed parms,set "parm_list"          ;AN663;

      .SELECT                                                                                                         ;AN663;

         .WHEN <parm_list[DI].item_tag EQ E_item_tag>                                                                 ;AN663;
            MOV  AL,error_status         ;set mask byte to horrible status                           ;AN663;

         .WHEN <parm_list[DI].item_tag EQ P_item_tag> OR                                                              ;AN663;
         .WHEN <parm_list[DI].item_tag EQ B_item_tag>                                                         ;AN663;
            MOV  AL,busy_status          ;set mask byte to actual status                              ;AN663;

         .WHEN <parm_list[DI].item_tag EQ R_item_tag>                                             ;AN663;
            MOV  AL,ready_status         ;set mask byte to rosy status         ;AN663;

         .WHEN <parm_list[DI].item_tag EQ NONE_item_tag>                                             ;AN663;;AN000;
            MOV  AL,no_retry_flag      ;when there is no retry the mask will not be used, so this is just a flag

      .ENDSELECT

      PUSH  AX                                    ;save the retry setting ;AN001;
      .IF <AL EQ no_retry_flag> THEN
;AC001;  MOV      INF_OR_NO_PTR,OFFSET NORETRY    ;modify message to indicate no retry
         set_submessage_ptr   noretry,retparto    ;modify message to indicate no retry         ;AC001;
      .ELSE                                                                                    ;AN663;
;AC001;  MOV    INF_OR_NO_PTR,OFFSET INFINITE   ;modify message to indicate retry              ;AN663;
         set_submessage_ptr   infinite,retparto    ;modify message to indicate retry            ;AC001;
      .ENDIF                                                                                   ;AN663;
      POP   AX                                    ;restore the retry setting ;AN001;

      .IF <<WORD PTR ES:resseg> EQ 0000H> THEN    ;IF code not resident THEN                      ;AN663;
         .IF <AL NE no_retry_flag> THEN              ;need to turn on retry
            CALL modeload                            ;load resident code                          ;AN663;
            MOV  ES,ES:WORD PTR resseg[2]                                                            ;AN663;
            MOV  BYTE PTR ES:lpt1_retry_type[BX],AL      ;store new setting                    ;AN663;
         .ENDIF
      .ELSE                                       ;ELSE code is already resident
         MOV  ES,ES:WORD PTR resseg[2]                                                            ;AN663;
         MOV  BYTE PTR ES:lpt1_retry_type[BX],AL      ;store new setting                     ;AN663;
      .ENDIF                                                                                   ;AN663;
                                                                                               ;AN663;
   .ENDIF                                                                                      ;AN663;
                                                                                               ;AN663;
.ELSEIF <<WORD PTR ES:resseg> NE 0000H> THEN      ;if code is loaded but no                    ;AN663;
                                                  ;  retry is specified then                   ;AN663;
   MOV  ES,ES:WORD PTR resseg[2]                ;ES=segment of the resident code             ;AN663;
                                                                                               ;AN663;
   .IF <parms_form NE keyword>                    ;no retry specified with                     ;AN663;
                                                  ;positional parameters, so turn off retry            ;AN663;
      MOV  BYTE PTR ES:lpt1_retry_type[BX],no_retry_flag         ;set flag for get retry routine;AN663;
;AC001;      MOV   INF_OR_NO_PTR,OFFSET NORETRY          ;modify message to indicate no retry         ;AN663;
         set_submessage_ptr   noretry,retparto    ;modify message to indicate no retry         ;AC001;
                                                                                               ;AN663;
   .ELSE                                          ;else, no retry specified with keywords
                                                  ;  update pparm with current retry type      ;AN663;
      .IF <<BYTE PTR ES:lpt1_retry_type[BX]> EQ no_retry_flag> THEN                     ;AN663;
;AC001;  MOV      INF_OR_NO_PTR,OFFSET NORETRY       ;modify message to indicate no retry      ;AN663;
         set_submessage_ptr   noretry,retparto    ;modify message to indicate no retry         ;AC001;
      .ELSE                                                                                    ;AN663;
;AC001;  MOV    INF_OR_NO_PTR,OFFSET INFINITE   ;modify message to indicate retry              ;AN663;
         set_submessage_ptr   infinite,retparto    ;modify message to indicate retry            ;AC001;
      .ENDIF                                                                                   ;AN663;
                                                                                               ;AN663;
   .ENDIF                                                                                      ;AN663;
                                                                                               ;AN663;
.ELSE                                          ;no retry, no code resident                  ;AN663;
                                                                                               ;AN663;
;AC001;   MOV      INF_OR_NO_PTR,OFFSET NORETRY       ;modify message to indicate no retry            ;AN663;
   set_submessage_ptr   noretry,retparto    ;modify message to indicate no retry         ;AC001;
                                                                                               ;AN663;
.ENDIF                                                                                         ;AN663;
                         ;'Infinite retry on parallel printer timeout' OR                      ;AN663;
DISPLAY  RETPARTO        ;'No retry on parallel printer timeout'                               ;AN663;

srt_no_display:          ;M003; Do not display retry if network printer
                                                                                               ;AN663;
POP  DX                                           ;restore registers                           ;AN663;
POP  BX
POP  AX                                                                                        ;AN663;
POP  DI                                                                                        ;AN663;
POP  ES                                                                                        ;AN663;
                                                                                               ;AN663;
RET                                                                                            ;AN663;
                                                                                                     ;AN663;
set_retry_type ENDP                                                                                  ;AN663;

PRINTF_CODE   ENDS
     END

