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
;  05/22/90  MKS  C05  MODE was hanging when preparing code pages.  This is
;                      because someone was using PSP:6 to see how many free
;                      bytes were in the segment.  This old CALL5 from CPM is
;                      not officially supported and apparently changed in 5.0.
;                      We are now doing basically the same thing, but using a
;                      DOS call to see how much memory is left in the segment.
;
;******************************************************************************                                            ;AN000;
.XLIST
INCLUDE STRUC.INC
.LIST


;���������������������������������  M A C R O S  ����������������������������������������ͻ
;�                                                                                        �

set_submessage_ptr   MACRO submessage,message ;PUT pointer to "subMESSAGE" into submessage pointer field of "message".

MOV   AX,submessage                          ;AX=message number                 ;AN001;
MOV   DH,utility_msg_class                   ;DH=message class=utility class    ;AN001;
CALL  SYSGETMSG                              ;DS:SI=>message piece                                ;AN001;
MOV   BP,OFFSET sublist_&message             ;address the sublist control block ;AN001;
MOV   [BP].sublist_off,SI                    ;the sublist now points to the desired message piece ;AN001;
ENDM                                                                                              ;AN001;

;�                                                                                        �
;���������������������������������  M A C R O S  ����������������������������������������ͼ

      PAGE    ,132                    ;
        TITLE   MODECP.SAL - CODEPAGE SUPPORT
        INCLUDE MODECPRO.INC            ;MODULE PROLOGUE
;THE FOLLOWING "INCLUDE MODECPEQ.INC" CONTAINS THE FOLLOWING DEFINITIONS:
; MACROS: HEADER, DOSCALL
; DOS FUNCTION CALLS EQUATES
; MAJOR AND MINOR CODES FOR "GENERIC IOCTL" DOS FUNCTION CALL
; ERROR RETURN CODES FROM SEVERAL SUBFUNCTIONS OF THE GENERIC IOCTL
; OPEN MODE EQUATES
; DEFINITIONS OF STRUC:
;      "FON" - THE HEADER OF THE CODEPAGE FONT FILE
;      "CODEPAGE_PARMS" - INPUT PARM LIST FROM CALLER
;      "PACKET" AND "DES_STRT_PACKET" - BUFFERS USED
;         BY THE SEVERAL CODEPAGE DOS IOCTL CALLS
        INCLUDE MODECPEQ.INC            ;MACROS,DOS EQUATES,STRUCS,OTHER EQUATES
; = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
        HEADER  <DESCRIPTIONS OF ALL MESSAGES USED BY MODECP.SAL>
        INCLUDE MODECPMS.INC            ;DESCRIPTIONS OF MESSAGES
; = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
        HEADER  <EXTRNS, LOCAL DATA AND OTHER WORKAREAS>
;              $SALUT CP(4,16,22,38) ;(set preprocessor columns for indenting)
PRINTF_CODE    SEGMENT WORD PUBLIC
               ASSUME CS:PRINTF_CODE ;"MODE" IS A ".COM" FILE;
               ASSUME DS:PRINTF_CODE ; AS SUCH, ALL FOUR SEG REGS
               ASSUME ES:PRINTF_CODE ; POINT TO THE ONE COMMON
               ASSUME SS:PRINTF_CODE ; SEGMENT, "PRINTF_CODE"

MODECP00       EQU   $               ;UNREFERENCED, LOCATES REL ZERO IN LINK MAP
               PUBLIC MODECP00


;��������������������������������  P U B L I C S  ���������������������������������������ͻ
;�                                                                                        �

PUBLIC   CLOSE                      ;EQU  3EH   ;CLOSE A FILE HANDLE,make available to analyze_and_invoke, see modecpeq.inc
PUBLIC   DES_STRT_FL_CART           ;EQU  0001H, means CARTRIDGE prepare
PUBLIC   DEV_OPEN_MODE              ;make available to analyze_and_invoke, see modecpeq.inc
PUBLIC   OPEN                       ;make available to analyze_and_invoke, see modecpeq.inc

;�                                                                                        �
;��������������������������������  P U B L I C S  ���������������������������������������ͼ

;��������������������������������  E X T R N S  �����������������������������������������ͻ
;�                                                                                        �

EXTRN    cp_cb:WORD        ;AN000;codepage_parms <>, see invoke

               EXTRN NOERROR:BYTE    ;ERRORLEVEL RETURN CODE FLAG BYTE
                                     ; NORMAL VALUE IS "TRUE" (-1)
                                     ; IF ERROR OCCURS, SET IT TO "FALSE" (0)
               EXTRN PRINTF:NEAR     ;SENDS MESSAGES TO STDOUT OR STDERR

EXTRN    SYSGETMSG:NEAR    ;AN001;message services routine to get the address of a message.  Used to get address of a submessage.
EXTRN    Utility_Msg_Class:ABS    ;AN001;
EXTRN    sublist_cpmsg8:BYTE    ;AN001;
EXTRN    sublist_cpmsg10:BYTE    ;AN001;
EXTRN    sublist_cpmsg17:BYTE    ;AN001;

;�                                                                                        �
;��������������������������������  E X T R N S  �����������������������������������������ͼ


INCLUDE  common.stc        ;AN001;includes the following structures


;bogus    EQU   88H      ;totally invalid value
;
;codepage_parms STRUC
;   cp_device      DW    ?
;   des_pack_ptr   DW    ?
;   font_filespec  DW    ?
;   request_typ    DW    ?
;codepage_parms ENDS
;
;
;parm_list_entry   STRUC                   ;used by parse_parameters and invoke
;
;parm_type            DB       bogus
;item_tag             DB       0FFH
;value1               DW       bogus       ;used only for filespecs and code page numbers
;value2               DW       bogus       ;used only for filespecs and code page numbers
;keyword_switch_ptr   DW    0
;
;parm_list_entry   ENDS
;
;
;sublist_def  STRUC         ;used by initialize_sublists
;
;             db  ?  ;Sublist Length, fixed
;             db  ?  ;Reserved, not used yet                   ;AN000;
;sublist_off  dw  ?  ;offset
;sublist_seg  dw  ?  ;segment part of pointer to piece of message
;             db  ?  ;ID, special end of message format ;AN000;
;             db  ?  ;flags
;             db  ?
;             db  ?
;             db  ?
;
;sublist_def  ENDS

;������������������������������������  D A T A  �����������������������������������������ͻ
;�                                                                                        �


                                     ;ALL MESSAGES ARE IN THE MODEMES.ASM MODULE
current_request   DB bogus             ;see 'do_refresh' and 'des_end'
dgroup         group zseg,printf_code
zseg           segment para public
;               LOCAL WORKAREA
;C05 BUF            DB    512 DUP(0)      ;DEFAULT BUFFER AREA
BUF            DB 0                  ;DEFAULT BUFFER AREA               ;C05
                                     ;Buffer allocated from here later  ;C05
               public buf            ;not referenced, just shows up on mapf buf
END_OF_BUF     EQU   $
zseg           ends

RESPONSE_LIST  LABEL WORD            ;WHERE TO PUT THE RESULTS OF QUERY prepareS
RES_NUM_ENTRIES EQU  12              ;NUMBER ENTRIES IN EACH HARDWARE AND prepare LISTS
RES_LEN        DW    RES_END - RESPONSE_LIST - 2 ;BYTE SIZE OF RESPONSE AREA
RES_HWCP       DW    RES_NUM_ENTRIES ;NUMBER OF HARDWARE CODE PAGES IN FOLLOWING LIST
               DW    RES_NUM_ENTRIES DUP(-1)
RES_DSCP       DW    RES_NUM_ENTRIES ;NUMBER OF prepare CODE PAGES IN FOLLOWING LIST
               DW    RES_NUM_ENTRIES DUP(-1)
RES_END        EQU   $               ;END OF QUERY prepare RESPONSE LIST


OLDINT24       DD    ?               ;ORIGINAL CONTENTS OF INT 24H VECTOR
CRITERROR      DW    0               ;ERROR REPORTED IN DI TO INT 24H HANDLER

DEVICE_STATUS  DW    ?               ;FLAGS SET BY IOCTL (GET DEVICE INFO)
DEV_HANDLE     DW    ?               ;VALUE OF HANDLE RETURNED BY
                                     ; OPEN TO DEVICE FOR IOCTL I/O
FILE_HANDLE    DW    ?               ;VALUE OF HANDLE RETURNED BY
                                     ; OPEN TO FILESPEC.
DEV_TYPE       DW    prepare_STRT    ;CX=xxx_DEV_TYPE * 256 + "prepare_STRT"
                                     ;THE HIGH BYTE IS JUST ZERO HERE, BUT
                                     ; THAT WILL BE OR'ED IN LATER
PK             PACKET <>             ;SELECT, QUERY SELECTED, prepare END

PUBLIC         PK

DBCS_headr     DBCS_head <>

PUBLIC         DBCS_headr

;       THESE NEXT TWO WORDS MUST BE KEPT TOGETHER
;       THEY ARE REFERENCED AS A DWORD
DBUF           LABEL DWORD
BUF_OFF        DW    0               ;OFFSET OF ALLOCATED BUFFER
BUF_SEG        DW    ?               ;SEGID OF ALLOCATED BUFFER

BUF_SIZ        DW    1000H           ;REMEMBER HOW MUCH BUF IS AVAILABLE
                                     ; (IN PARAGRAPHS, NOT BYTES)
BUF_BYTES      DW    ?               ;NUMBER OF BYTES ACTUALLY IN THE BUFFER
PREPED         DW    0               ;COUNT OF CODEPAGES KNOWN TO DEVICE
MINOR_VERSION  DB    0,0             ;MINOR VERSION OF DOS
STATUS_BREAK   DB    0               ;SAVES THE CURRENT STATUS OF "BREAK"
DEV_TABLE      LABEL BYTE            ;TABLE OF SUPPORTED DEVICE NAMES
CN             DB    "CON",0
LP             DB    "PRN",0
L1             DB    "LPT1",0
L2             DB    "LPT2",0
L3             DB    "LPT3",0
C1             DB    "COM1",0
C2             DB    "COM2",0
C3             DB    "COM3",0
C4             DB    "COM4",0
G1             DB    "*",0
END_DEV_TABLE  LABEL BYTE
DEV_TAB_PTRS   DW    CN,LP,L1,L2,L3,C1,C2,C3,C4,G1,END_DEV_TABLE
NUM_TABL_ENTRIES EQU (($-DEV_TAB_PTRS)-2)/2 ;NUMBER OF DEVICE POINTERS
;THE ENTRIES IN THE NEXT TABLE MUST BE KEPT IN THE SAME ORDER AS THE
; DEVICE NAMES IN THE ABOVE LIST.  THERE MUST BE A ONE-TO-ONE CORRESPONDENCE
; BETWEEN THE DEVICE NAMES AND THIS TABLE OF DEVICE TYPES.
DEV_TYPES      DB    CON_DEV_TYPE    ;CN
               DB    LPT_DEV_TYPE    ;LP
               DB    LPT_DEV_TYPE    ;L1
               DB    LPT_DEV_TYPE    ;L2
               DB    LPT_DEV_TYPE    ;L3
               DB    COM_DEV_TYPE    ;C1
               DB    COM_DEV_TYPE    ;C2
               DB    COM_DEV_TYPE    ;C3
               DB    COM_DEV_TYPE    ;C4
               DB    GLOBAL_CP       ;G1


;�                                                                                        �
;������������������������������������  D A T A  �����������������������������������������ͼ



MODECP PROC NEAR               ;SUBROUTINE ENTRY POINT


 PUBLIC MODECP
                               ;REMEMBER WHAT THE CURRENT SETTING IS
                               ; OF "BREAK"
 DOSCALL BREAK_CHECK,REQUEST_BREAK_STATE ;CURRENT STATE RETURNED IN DL

 MOV   STATUS_BREAK,DL         ;REMEMBER WHAT THE CURRENT BREAK STATUS IS

 MOV   DL,BREAK_OFF            ;AVOID UNWANTED CTRL-BREAK DURING OPERATION
 DOSCALL BREAK_CHECK,SET_BREAK_STATE

;DOSCALL DOS_VERSION           ;DETERMINE VERSION OF DOS
                               ;OUTPUT: AL=MAJOR, AH=MINOR VERSION NUMBER
;MOV   MINOR_VERSION,AH        ;        BX AND CX SET TO ZERO

;               REMEMBER ORIGINAL OWNER OF INT 24H
;                  THE CRITICAL ERROR HANDLER
 CALL  SAVE_VECTOR24           ;SET DWORD AT "OLDINT24" WITH ORIGINAL POINTERS

;                       SET UP THE DEVICE TYPE
;                       FOR THE prepare START FUNCTION
 MOV   BX,CP_CB.cp_device     ;SET A BASE REG TO POINT TO DEV NAME
 CALL  SET_DEV_TYPE            ;INTERROGATE THE DEVICE NAME,
                               ; SET "DEV_TYPE" ACCORDINGLY
;CMP   BYTE PTR DEV_TYPE+BYTE,GLOBAL_CP ;WAS THE DEVICE SPECIFIED AS "*"?
;$IF   E
;   CALL  SET_GLOBAL_CODEPAGE
;$ELSE                         ;SINCE DEVICE WAS NOT "*"


;                      OPEN DEVICE
    MOV   DX,BX                ;DS:DX=POINTER TO ASCIIZ DEVICE NAME
    DOSCALL OPEN,DEV_OPEN_MODE ;OPEN DEVICE WITH READ/WRITE ACCESS

;   $IF   NC                   ;IF OPEN OK,
    JC $$IF1
       MOV   DEV_HANDLE,AX     ;REMEMBER HANDLE TO DEVICE

       MOV   BX,AX             ;PASS DEVICE HANDLE TO IOCTL
       DOSCALL IOCTL,IOCTL_FUN_GET_INFO

       MOV   DEVICE_STATUS,DX  ;SAVE THE DEVICE STATUS
;      $IF   NC,AND            ;IF OK
       JC $$IF2
       TEST  DX,ISDEVICE       ;IS THIS A DEVICE OR FILE?
;      $IF   NZ                ;IF IS A DEVICE
       JZ $$IF2
          CALL  FUNCTION_SELECT ;CHECK THE "request_typ", AND
                               ; CALL THE APPROPRIATE FUNCTION HANDLER
;      $ELSE                   ;SINCE NOT A DEVICE, MUST BE A FILE
       JMP SHORT $$EN2
$$IF2:
          MOV   DX,OFFSET CPMSG15     ;PASS POINTER TO MSG PARM LIST
          CALL  SEND_MSG       ;"DEVICE NOT SUPPORTED FOR CODEPAGE"

;      $ENDIF                  ;OK FROM IOCTL, AND DEVICE OR FILE?
$$EN2:
       MOV   BX,DEV_HANDLE
       DOSCALL CLOSE           ;FINISHED WITH DEVICE

;   $ELSE                      ;SINCE DEVICE OPEN NOT OK
    JMP SHORT $$EN1
$$IF1:
       MOV   AX,CP_CB.cp_device   ;GET OFFSET TO ASCIIZ DEVICE NAME
       MOV   CPMSGLST2DEV,AX   ; TO MSG PARM LIST
       MOV   DX,OFFSET CPMSG2     ;PASS POINTER TO MSG PARM LIST
       CALL  SEND_MSG          ;"FAILURE TO OPEN DEVICE"

;   $ENDIF                     ;DEVICE OPEN OK?
$$EN1:
;$ENDIF                        ;DEVICE "*"?
 MOV   DL,STATUS_BREAK         ;GET WHAT THE BREAK STATUS USED TO BE
 DOSCALL BREAK_CHECK,SET_BREAK_STATE ;RETURN TO USER DEFINED BREAK CONDITION

 RET                           ;RETURN TO CALLER
MODECP ENDP
; = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =


SET_DEV_TYPE PROC NEAR



;INPUT: BX = OFFSET TO ASCIIZ OF DEVICE NAME

 MOV   CX,NUM_TABL_ENTRIES     ;SET LOOP COUNTER TO NUMBER OF DEVICE NAMES IN LIST
 MOV   SI,OFFSET DEV_TAB_PTRS  ;SET INDEX TO FIRST ENTRY IN TABLE OF POINTERS
 MOV   DI,OFFSET DEV_TYPES     ;SET INDEX TO FIRST ENTRY OF DEVICE TYPES
;$SEARCH                       ;LOOK THRU TABLE FOR MATCHING ENTRY
$$DO7:
    PUSH  DS                   ;AN002;
    POP   ES                   ;need ES and DS the same for the CMPSB    ;AN002;
    PUSH  CX                   ;SAVE COUNTER OF DEVICE NAMES
    PUSH  SI                   ;SAVE POINTER TO TABLE OF POINTERS TO DEVICE NAMES
    PUSH  DI                   ;SAVE POINTER TO TABLE OF DEVICE TYPES

    MOV   CX,[SI]+WORD         ;GET OFFSET TO NEXT DEVICE NAME
    SUB   CX,[SI]              ;SET COUNT TO SIZE OF THIS DEVICE NAME
    MOV   DI,[SI]              ;POINT TO DEVICE NAME FROM ENTRY IN TABLE
    MOV   SI,BX                ;POINT TO DEVICE NAME FROM COMMAND LINE
    REP   CMPSB                ;IS THIS THE ONE?

    POP   DI                   ;RESTORE POINTER TO DEVICE TYPE LIST
    POP   SI                   ;RESTORE POINTER TO TABLE OF DEVICE NAME POINTERS
    POP   CX                   ;RESTORE COUNTER OF DEVICES
;$EXITIF E
 JNE $$IF7
    MOV   AL,BYTE PTR[DI]      ;GET TYPE OF THIS DEVICE
;$ORELSE
 JMP SHORT $$SR7
$$IF7:
    ADD   SI,WORD              ;BUMP INDEX TO NEXT ENTRY IN TABLE
    ADD   DI,BYTE              ;BUMP INDEX TO NEXT ENTRY IN TABLE
;$ENDLOOP LOOP
 LOOP $$DO7
    MOV   AL,UNK_DEV_TYPE      ;DEVICE NAME IS NOT IN THE ABOVE LIST
;$ENDSRCH
$$SR7:
 MOV   BYTE PTR DEV_TYPE+BYTE,AL ;ADD DESCRIPTOR OF DEVICE TO DEV_TYPE WORD

RET
SET_DEV_TYPE ENDP
; = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

;SET_GLOBAL_CODEPAGE PROC NEAR
;
;;DEVICE WAS SPECIFIED AS "*", INDICATING THE GLOBAL CODEPAGE IS THE DEVICE
;
; MOV   AX,CP_CB.request_typ
; CMP   AX,SELECT
; $IF   E
;    MOV   BX,CP_CB.DES_PACK_PTR ;SET BASE REG TO POINT TO PACKET AREA
;    MOV   BX,[BX].DES_STRT_PKCP1 ;GET CODEPAGE ID FROM PACKET
;    MOV   AX,SET_GLOBAL_CP     ;SET GLOBAL CODEPAGE
;    DOSCALL
;
;    $IF   C                    ;IF ERROR TRYING TO SET GLOBAL CODEPAGE
;       MOV   CPMSGLST11FUN,OFFSET CPMSG11_SET ;PUT "SETTING" INTO MESSAGE
;       MOV   DX,OFFSET CPMSG11     ;PASS OFFSET TO MSG PARM LIST
;       CALL  SEND_MSG
;
;    $ELSE
;       MOV   CPMSGLST10FUN,OFFSET CPMSG10_GLOBAL ;SET MSG TO SAY "GLOBAL"
;       MOV   DX,OFFSET CPMSG10     ;PASS OFFSET TO MSG PARM LIST
;       CALL  QUEUE             ;"MODE GLOBAL CODEPAGE FUNCTION COMPLETED"
;
;    $ENDIF                     ;ERROR IN SETTING GLOBAL CODEPAGE?
; $ELSE                         ;SINCE NOT "SELECT" ASSUME IT IS "STATUS"
;    MOV   AX,GET_GLOBAL_CP
;    DOSCALL
;
;    $IF   C                    ;IF ERROR
;       MOV   CPMSGLST11FUN,OFFSET CPMSG11_GET ;PUT "GETTING" INTO MESSAGE
;       MOV   DX,OFFSET CPMSG11     ;PASS OFFSET TO MSG PARM LIST
;       CALL  SEND_MSG
;
;    $ELSE                      ;SINCE NO ERROR
;       PUSH  DX                ; DX=SYSTEM CODE PAGE
;       PUSH  BX                ; BX=ACTIVE CODE PAGE
;       MOV   DX,OFFSET CPMSG12     ;PASS OFFSET TO MSG PARM LIST
;       CALL  QUEUE             ;"CURRENT CODEPAGE SETTINGS:"
;
;       MOV   CPMSGLST13TYP,OFFSET CPMSG13_ACT ;PUT "ACTIVE" INTO MESSAGE
;       POP   CPMSGLST13CP      ;PASS ACTIVE CODEPAGE ID TO MSG PARM LIST (PUSHED FROM BX)
;       MOV   DX,OFFSET CPMSG13     ;PASS OFFSET TO MSG PARM LIST
;       CALL  QUEUE             ;"   XXX - ACTIVE CODEPAGE"
;
;       POP   CPMSGLST13CP      ;PASS SYSTEM CODE PAGE (PUSHED FROM DX)
;       MOV   CPMSGLST13TYP,OFFSET CPMSG13_SYS ;PUT "SYSTEM" INTO MESSAGE
;       MOV   DX,OFFSET CPMSG13     ;PASS OFFSET TO MSG PARM LIST
;       CALL  QUEUE             ;"   XXX - SYSTEM CODEPAGE"
;
;    $ENDIF                     ;ERROR IN GETTING GLOBAL CODEPAGE STATUS?
; $ENDIF                        ;"SELECT" OR "STATUS"?
;
; RET
;SET_GLOBAL_CODEPAGE ENDP
; = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
 HEADER <SELECT FUNCTION HANDLING ROUTINE ACCORDING TO 'request_typ'>

FUNCTION_SELECT PROC NEAR

 MOV   AX,CP_CB.request_typ
 CMP   AX,PREPARE              ;("prepare" IS INTERNAL USAGE ONLY; "PREPARE" IS WHAT CUSTOMER SEES)
;$IF   E                       ;IF "prepare" IS REQUESTED FUNCTION
 JNE $$IF12
    CALL  DO_prepare           ;DEFINE FONTS TO DEVICE

;$ELSE                         ;SINCE NOT A "prepare"
 JMP SHORT $$EN12
$$IF12:
    CMP   AX,SELECT
;   $IF   E                    ;IF "SELECT" IS REQUESTED FUNCTION
    JNE $$IF14
       CALL  DO_SELECT

;   $ELSE                      ;SINCE NOT "SELECT" EITHER
    JMP SHORT $$EN14
$$IF14:
       CMP   AX,REFRESH
;      $IF   E                 ;IF "REFRESH IS REQUSTED FUNCTION
       JNE $$IF16
          CALL  DO_REFRESH

;      $ELSE                   ;SINCE NONE OF THE ABOVE
       JMP SHORT $$EN16
$$IF16:
          CALL  DO_STATUS

;      $ENDIF                  ;REFRESH?
$$EN16:
;   $ENDIF                     ;SELECT?
$$EN14:
;$ENDIF                        ;prepare?
$$EN12:

 RET
FUNCTION_SELECT ENDP
; = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
 HEADER <'prepare' -  CODEPAGE FUNCTION>

DO_prepare PROC NEAR

PUBLIC DO_prepare

 MOV   BX,CP_CB.DES_PACK_PTR
 TEST  [BX].DES_STRT_PKFL,DES_STRT_FL_CART ;CARTRIDGE prepare?
;$IF   Z                       ;NO, LOOK FOR FONT FILE
 JNZ $$IF21
    MOV   AX,CS                ;USE PREDEFINED AREA
    MOV   BUF_SEG,AX           ;REMEMBER WHERE BUFFER IS
    lea   dx,dgroup:BUF        ;buf is at end of allocated memory
    mov   BUF_OFF,dx
;C05mov   ax,ds:[6]            ;get number bytes in this segment
;C05sub   ax,dx                ;get size of buf to segment end
;C05mov   cl,4                 ;shift count
;C05shr   ax,cl                ;convert buf size to para size
    mov   bx,1000H              ;use BX for MODIFY ALLOCATED MEMORY     ;C05
    mov   ah,4Ah                ;int 21h function for shrinking memory  ;C05
    int   21h                   ;do it                                  ;C05
;
; BX = gives the available size 
; remember that our stack is also in the same segment and at the very end!
; THIS IS A HACK: We know that since mode < 64K in size, our SS == CS
; and also that SP = TOP of available memory; So we just leave out 
; the current stack usage + 5 paragraphs and use the remaining memory
; for reading the code page font file.
; 
    mov   cl,4                  ;shift count                            ;C05
    shr   dx,cl                 ;Convert buf size to paragraphs         ;C05
    sub   bx,dx                 ;Free bytes after BUF to end of memory  ;C05
                                ;Still accessible from CS               ;C05
    mov	dx, -1			; end of memory
    sub   dx,sp			; amout of stack already used
    add	dx,50h			; just some magic no; some extra room for
				; stack
    shr  dx,cl			; convert to paras
    sub	 bx,dx			; bx = size of buffer we can safely use
    jle	 Outofmem
	
    MOV   BUF_SIZ,bx           ;SIZE IN PARAGRAPHS
;                      OPEN FILE FOR INPUT

    MOV   DX,CP_CB.FONT_FILESPEC ;DS:DX = POINTER TO ASCIIZ PATH NAME
    DOSCALL OPEN,FILE_OPEN_MODE ;OPEN CODEPAGE FILE IN READ ONLY MODE

;   $IF   NC                   ;IF OPEN OK,
    JC $$IF22
       MOV   FILE_HANDLE,AX    ;REMEMBER THE HANDLE TO THE FILE
       CALL  DES_START         ;PERFORM THE prepare START

;      $IF   NC                ;IF prepare START WAS OK
       JC $$IF23
          CALL  DEVICE_TO_BINARY ;SET DEVICE TO "BINARY MODE"

          CALL  MOVE_FILE      ;READ IN THE CODEPAGE FILE, SEND TO DEVICE

          CALL  DES_END        ;PERFORM prepare END FUNCTION

;      $ELSE                   ;SINCE prepare START HAD A PROBLEM
       JMP SHORT $$EN23
$$IF23:
          CALL  DES_START_ERROR ;DISPLAY MSG SAYING WHY DES_START FAILED

;      $ENDIF                  ;prepare START OK?
$$EN23:

;                      CLOSE A FILE HANDLE
       MOV   BX,FILE_HANDLE    ;BX = HANDLE RETURNED BY OPEN OR CREATE
       DOSCALL CLOSE

;   $ELSE                      ;SINCE FILE OPEN NOT OK
    JMP SHORT $$EN22
$$IF22:
       MOV   DX,OFFSET CPMSG1     ;PASS OFFSET TO MSG PARM LIST
       CALL  SEND_MSG          ; "FAILURE TO OPEN CODEPAGE FONT FILE"

;   $ENDIF                     ;FILE OPEN OK?
$$EN22:
;$ELSE                         ;SINCE NO FONT FILE SPEC INDICATED FOR CARTRIDGE,
 JMP SHORT $$EN21

Outofmem:
	mov	dx,offset msgOutOfMemory
	call	SEND_MSG
	jmp	short $$EN21


$$IF21:

    CALL  DES_START            ;PERFORM THE prepare START

;   $IF   C
    JNC $$IF29
       CALL  DES_START_ERROR   ;DISPLAY MSG SAYING WHY DES_START FAILED

;   $ELSE                      ;SINCE DES_START WAS OK,
    JMP SHORT $$EN29
$$IF29:
       CALL  DES_END           ;PERFORM prepare END FUNCTION

;   $ENDIF                     ;DES_START ON CARTRIDGE OK?
$$EN29:
;$ENDIF                        ;CARTRIDGE?
$$EN21:
 RET
DO_prepare ENDP
; = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
DES_START PROC NEAR

PUBLIC DES_START

 MOV   CX,DEV_TYPE             ;CX=xxx_DEV_TYPE * 256 + "prepare_STRT"
 MOV   DX,CP_CB.DES_PACK_PTR   ;DS:DX=DATA BUFFER "DES_STRT_PACKET"
 CALL  DO_GENERIC_IOCTL

 RET
DES_START ENDP
; = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
DES_START_ERROR PROC NEAR
;AFTER DES_START, CARRY WAS SET ON, INDICATING AN ERROR
;               prepare START ERROR CODES:
                               ;DS_ERR0 EQU 0     PREVIOUS prepare DELETED
                               ;DS_ERR8 EQU 8     CODE PAGE CONFLICT (USED FOR KEYB XX MISMATCH)
                               ;DS_ERRA EQU 0AH  DEVICE ERROR
                               ;DS_ERR16 EQU 016H UNKNOWN COMMAND

 CALL  EX_ERR                  ;GET EXTENDED ERROR
                               ;AX=EXTENDED ERROR
                               ;BH=ERROR CLASS
                               ;BL=SUGGESTED ACTION
                               ;CH=LOCUS
 CMP   AX,DS_ERRA+MAPERR       ;DEVICE ERROR?
;$IF   E
 JNE $$IF33
;AC001;    MOV   CPMSGLST17FUN,OFFSET CPMSG17_PREP ;MOVE "PREPARE" INTO MESSAGE
    set_submessage_ptr CPMSGxx_PREP,cpmsg17 ;MOVE "PREPARE" INTO MESSAGE ;AN002;
    MOV   DX,OFFSET CPMSG17     ;PASS OFFSET TO MSG PARM LIST
                               ;"DEVICE ERROR DURING PREPARE"

;$ELSE                         ;SINCE NOT ERROR CODE 0AH,
 JMP SHORT $$EN33
$$IF33:
    CMP   AX,DS_ERR0+MAPERR    ;PREVIOUS prepare DELETED?
;   $IF   E
    JNE $$IF35
       MOV   DX,OFFSET CPMSG5     ;PASS OFFSET TO MSG PARM LIST
                               ;"PREVIOUSLY PREPARED CODEPAGE DELETED"
;   $ELSE
    JMP SHORT $$EN35
$$IF35:
       CMP   AX,DS_ERR16       ;DOES THE DEVICE DRIVER SUPPORT DESIGNATE START FUNCTION?
;      $IF   E,OR
       JE $$LL37
       CMP   AX,DS_ERR1        ;16=unknown command, 1=Invalid function number
;      $IF   E
       JNE $$IF37
$$LL37:
          MOV   DX,OFFSET CPMSG15     ;PASS OFFSET TO MSG PARM LIST
                               ;"Codepage operation not supported on this device",BEEP,CR,LF,EOM

;      $ELSE                   ;SINCE NOT THAT EITHER, ASSUME MUST BE CODE 8
       JMP SHORT $$EN37
$$IF37:
          MOV   DX,OFFSET CPMSG19     ;PASS OFFSET TO MSG PARM LIST
                               ;"Current keyboard does not support this Codepage",BEEP,CR,LF,EOM
;      $ENDIF
$$EN37:

;   $ENDIF                     ;PREV prepare DELETED?
$$EN35:
;$ENDIF                        ;WHICH ERROR CODE?
$$EN33:
 CALL  SEND_MSG                ;DISPLAY INDICATED MESSAGE


 RET
DES_START_ERROR ENDP
; = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

MOVE_FILE PROC NEAR

;CODEPAGE FILE IS OPEN, THE prepare START IS COMPLETED OK...

dummy8:
public   dummy8


;$SEARCH                       ;REPEAT READS UNTIL FILE COMPLETELY READ
$$DO42:
;                      READ FROM A FILE OR DEVICE
    MOV   AX,BUF_SIZ           ;NUMBER OF PARAS TO READ
    MOV   CL,4
    SHL   AX,CL                ;CONVERT PARA COUNT TO BYTE COUNT
    MOV   CX,AX                ;PASS BYTE COUNT TO CX
    MOV   BX,FILE_HANDLE       ;BX = FILE HANDLE
    PUSH  DS
    LDS   DX,DBUF              ;DS:DX = BUFFER ADDRESS
    DOSCALL READ

    POP   DS                   ;RESTORE DATA ADDRESSABILITY
;$LEAVE C                      ;ERROR DURING READ?
 JC $$EN42
    MOV   BUF_BYTES,AX         ;REMEMBER HOW MANY BYTES READ
    CMP   AX,0                 ;WAS ANYTHING READ?
;$LEAVE E                      ;NOTHING READ, QUIT
 JE $$EN42
                               ;SOMETHING WAS READ, PROCESS THE BUFFER
    CALL  HOOK_IN_MY_INT24     ;DO MY OWN ERROR HANDLING

;                     WRITE TO A FILE OR DEVICE
    MOV   BX,DEV_HANDLE        ;BX = FILE HANDLE
    MOV   CX,BUF_BYTES         ;CX = NUMBER OF BYTES TO WRITE
    PUSH  DS
    LDS   DX,DBUF              ;DS:DX = ADDRESS OF DATA TO WRITE
    DOSCALL IOCTL,IOCTL_WRITE  ;WRITE THE DATA TO THE DEVICE

    POP   DS
    PUSHF                      ;SAVE FLAGS
    CALL  RESTORE_OLD_INT24    ;LET SYSTEM RESUME ERROR HANDLING

    POPF                       ;RESTORE FLAGS

;$EXITIF C                     ;QUIT IF A PROBLEM
 JNC $$IF42
                               ;prepare WRITE ERROR CODES:
                               ;DW_ERR8 EQU 8   DEVICE NOT FOUND IN FILE ,OR
                               ;                 CODE PAGE NOT FOUND IN FILE
                               ;DW_ERRA EQU 0AH DEVICE ERROR
                               ;DW_ERRC EQU 0CH  FILE CONTENTS NOT A FONT FILE,
                               ;                 OR FILE CONTENTS STRUCTURE DAMAGED
    CALL  EX_ERR               ;GET EXTENDED ERROR
                               ;AX=EXTENDED ERROR
                               ;BH=ERROR CLASS
                               ;BL=SUGGESTED ACTION
                               ;CH=LOCUS
    CMP   AX,FAIL24            ;DID ERROR COME FROM THE INT 24 HANDLER?
;   $IF   E
    JNE $$IF46
       MOV   AX,CRITERROR      ;REAL ERROR CAME FROM INT24
       ADD   AX,MAPERR
;   $ENDIF
$$IF46:
    CMP   AX,DW_ERR8+MAPERR    ;DEVICE NOT FOUND IN FILE ,OR
                               ; CODE PAGE NOT FOUND IN FILE
;   $IF   E
    JNE $$IF48
       MOV   DX,OFFSET CPMSG3     ;"Missing from font file is either device or codepage"
;   $ELSE                      ;SINCE NOT ERR 8
    JMP SHORT $$EN48
$$IF48:
       CMP   AX,DW_ERRA+MAPERR ;DEVICE ERROR

;      $IF   E
       JNE $$IF50
;AC001;   MOV   CPMSGLST17FUN,OFFSET CPMSG17_WRIT ;"write of font file to device",EOM
          set_submessage_ptr  CPMSG17_WRIT,cpmsg17  ;"write of font file to device",EOM;AN001;
          MOV   DX,OFFSET CPMSG17     ;"DEVICE ERROR DURING %S"
;      $ELSE                   ;SINCE NOT ERR A NEITHER, MUST BE
       JMP SHORT $$EN50
$$IF50:
                               ; FILE CONTENTS NOT A FONT FILE,
                               ; OR FILE CONTENTS STRUCTURE DAMAGED
          MOV   DX,OFFSET CPMSG4     ;"Font file contents invalid"
;      $ENDIF                  ;ERR "A"?
$$EN50:
;   $ENDIF                     ;ERR 8?
$$EN48:
    CALL  SEND_MSG             ;DISPLAY THE ERROR MSG POINTED TO BY DX

;$ORELSE                       ;SINCE WRITE TO DEVICE WAS OK,
 JMP SHORT $$SR42
$$IF42:
;$ENDLOOP                      ;GO BACK AND READ SOME MORE
 JMP SHORT $$DO42
$$EN42:
;   $IF   C                    ;IF READ ERROR
    JNC $$IF56
       MOV   DX,OFFSET CPMSG20     ;"ERROR DURING READ OF FONT FILE"
       CALL  SEND_MSG          ;DISPLAY THE ERROR MSG POINTED TO BY DX

;   $ENDIF                     ;READ ERROR?
$$IF56:
;$ENDSRCH
$$SR42:

 RET
MOVE_FILE ENDP
; = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

DES_END PROC NEAR

 MOV   CX,DEV_TYPE             ;PICK UP DEVICE TYPE AS MAJOR CODE
 MOV   CL,prepare_END          ;CX=xxx_DEV_TYPE * 256 + "prepare_END"
 MOV   DX,OFFSET PK            ;DS:DX=DATA BUFFER, PROBABLY NOT USED
                               ; THIS IS JUST A DUMMY BUFFER, NO DATA FOR IT
 CALL  DO_GENERIC_IOCTL

;$IF   C                       ;IF THERE WAS AN ERROR
 JNC $$IF59
;AC001;    MOV   CPMSGLST17FUN,OFFSET CPMSG17_PREP ;MOVE "PREPARE" INTO MESSAGE
    set_submessage_ptr  CPMSGxx_PREP,cpmsg17 ;MOVE "PREPARE" INTO MESSAGE
    MOV   DX,OFFSET CPMSG17     ;PASS OFFSET TO MSG PARM LIST
    CALL  SEND_MSG             ;"DEVICE ERROR DURING PREPARE"

;$ELSE                         ;SINCE NO ERROR DURING PREPARE END,
 JMP SHORT $$EN59
$$IF59:
    CMP   NOERROR,TRUE
;   $IF   E                    ;IF no previous errors THEN
    JNE $$IF61
       CMP   current_request,refresh_request
;      $IF   E
       JNE $$IF62
;AC001;   MOV   CPMSGLST10FUN,OFFSET CPMSG10_REFRESH   ;SET MSG TO SAY "REFRESH"
          set_submessage_ptr  CPMSGxx_REFRESH,cpmsg10   ;SET MSG TO SAY "REFRESH";AN001;
;      $ELSE
       JMP SHORT $$EN62
$$IF62:
;AC001;   MOV   CPMSGLST10FUN,OFFSET CPMSG10_DES ;SET MSG TO SAY "PREPARE"
          set_submessage_ptr  CPMSGxx_prep,cpmsg10 ;SET MSG TO SAY "PREPARE";AN001;
;      $ENDIF
$$EN62:
       MOV   DX,OFFSET CPMSG10     ;PASS OFFSET TO MSG PARM LIST
       CALL  printf             ;"MODE PREPARE CODEPAGE FUNCTION COMPLETED"
;   $ENDIF
$$IF61:

;$ENDIF                        ;ERROR DURING PREPARE END?
$$EN59:
 RET
DES_END ENDP
; = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

;        'SELECT' - FUNCTION HANDLER



DO_SELECT PROC NEAR

 CALL  setup_packet_for_DBCS   ;see if DBCS vectors needed, if so move them into the packet
 MOV   BX,CP_CB.DES_PACK_PTR   ;GET PACKET FROM INPUT PARMLIST
 MOV   AX,[BX].DES_STRT_PKCP1  ;GET CODEPAGE FROM HIS PACKET
 MOV   PK.PACKCPID,AX          ; INTO PACKET EXPECTED BY SELECT FUNCTION
 MOV   CX,DEV_TYPE             ;CX=xxx_DEV_TYPE * 256 + "MINOR CODE"
 MOV   CL,SELECT_CP            ;SET MINOR CODE TO "SELECT"
 MOV   DX,OFFSET PK            ;DS:DX=DATA BUFFER "PACKET"
 CALL  DO_GENERIC_IOCTL

dummy10:
public dummy10

;$IF   C                       ;IF SELECT WAS NOT OK
 JNC $$IF67

    CALL  EX_ERR               ;GET EXTENDED ERROR
                               ;AX=EXTENDED ERROR
                               ;BH=ERROR CLASS
                               ;BL=SUGGESTED ACTION
                               ;CH=LOCUS
    CMP   AX,IN_ERR7+MAPERR    ;CODE PAGE NOT PREPARED?
;   $IF   E
    JNE $$IF68
       MOV   DX,OFFSET CPMSG18     ;PASS OFFSET TO MSG PARM LIST
                               ;"SPECIFIED CODEPAGE NOT PREPARED"
;   $ELSE                      ;NOT ERR7, MUST BE SOMETHING ELSE
    JMP SHORT $$EN68
$$IF68:
       CMP   AX,IN_ERR8+MAPERR ;KEYBOARD SUPPORT THIS CODEPAGE?
;      $IF   E
       JNE $$IF70
          MOV   DX,OFFSET CPMSG19     ;"CURRENT KEYBOARD DOES NOT SUPPORT THIS CP"
;      $ELSE                   ;SINCE NOT ERR8 EITHER, MUST BE DEVICE ERROR
       JMP SHORT $$EN70
$$IF70:

;AC001;   MOV   CPMSGLST17FUN,OFFSET CPMSG17_ACT
          set_submessage_ptr  CPMSGxx_select,cpmsg17                       ;AN001;
          MOV   DX,OFFSET CPMSG17     ;"DEVICE ERROR DURING SELECT"
;      $ENDIF                  ;KEYB SUPPORT THIS CODEPAGE?
$$EN70:
;   $ENDIF                     ;CP NOT PREPARED?
$$EN68:
    CALL  SEND_MSG             ;DISPLAY INDICATED ERROR MESSAGE

;$ELSE                         ;SINCE SELECT WAS OK,
 JMP SHORT $$EN67
$$IF67:
    CMP   NOERROR,TRUE
;   $IF   E                    ;IF no previous errors THEN
    JNE $$IF75
;AC001;MOV   CPMSGLST10FUN,OFFSET CPMSG10_SELECT ;SET MSG TO SAY "SELECT"
       set_submessage_ptr  CPMSGxx_SELECT,cpmsg10 ;SET MSG TO SAY "SELECT"
       MOV   DX,OFFSET CPMSG10     ;PASS OFFSET TO MSG PARM LIST
       CALL  printf             ;"MODE SELECT CODEPAGE FUNCTION COMPLETED"
;   $ENDIF
$$IF75:

;$ENDIF                        ;SELECT OK?
$$EN67:

 RET
DO_SELECT ENDP
; = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
;�������������������������������������������������������������������������������
;�
;� DO_REFRESH
;� ----------
;�
;�  THIS EVENT WOULD BE REQUIRED IN CASE THE EXTERNAL DEVICE LOST ITS LOADABLE
;�  FONT DESCRIPTIONS, AS PERHAPS IF THE PRINTER GOT TURNED OFF, FOR EXAMPLE.
;�
;�  The possible return codes for a REFRESH request will be returned only on a
;� designate start and are defined as follows:
;�
;� 8008 - For DISPLAY.SYS this means that there is a keyboard/code page conflict.
;�
;� 800A - Device error.
;�
;� 800C - The device driver does not have a copy of the code page in memory.
;�        This could be due to the DEVICE= command in CONFIG.SYS not setting up
;�        the buffer, so all previous prepares were handled by downloading the
;�        code page directly to the printer.
;�
;�������������������������������������������������������������������������������


 HEADER <'REFRESH' - RELOAD EXISTING FONTS>


DO_REFRESH PROC NEAR


 MOV   current_request,refresh_request

 CALL  DES_START               ;PERFORM THE prepare START

;$IF   NC                      ;IF prepare START WAS OK
 JC $$IF78
    CALL  DEVICE_TO_BINARY     ;SET DEVICE TO "BINARY MODE"

;UNLIKE THE prepare FUNCTION, THERE IS NO FONT FILE TO BE SENT TO THE
;DEVICE DRIVER, WHO SHOULD STILL HAVE THE FONT DESCRIPTOR DATA TO BE
;RETRANSMITTED TO THE EXTERNAL DEVICE ITSELF.



    CALL  DES_END              ;PERFORM prepare END FUNCTION

;$ELSE                         ;SINCE prepare START HAD A PROBLEM
 JMP SHORT $$EN78
$$IF78:

;AFTER DES_START, CARRY WAS SET ON, INDICATING AN ERROR
;               prepare START ERROR CODES for REFRESH request:
;RS_ERR8             EQU  8     ;KEYBOARD/CODE PAGE CONFLICT
;RS_ERRA             EQU  0AH   ;DEVICE ERROR
;RS_ERRC             EQU  0CH   ;DEVICE DRIVER DOES NOT HAVE COPY OF CODE PAGE

    CALL  EX_ERR                  ;GET EXTENDED ERROR
                                  ;AX=EXTENDED ERROR
                                  ;BH=ERROR CLASS
                                  ;BL=SUGGESTED ACTION
                                  ;CH=LOCUS
    CMP   AX,DS_ERRA+MAPERR       ;DEVICE ERROR?
;   $IF   E
    JNE $$IF80
;AC001;MOV   CPMSGLST17FUN,OFFSET CPMSG17_REFRESH   ;MOVE "Refresh" INTO MESSAGE
       set_submessage_ptr  CPMSGxx_REFRESH,cpmsg17   ;MOVE "Refresh" INTO MESSAGE ;AN001;
       MOV   DX,OFFSET CPMSG17     ;PASS OFFSET TO MSG PARM LIST
                                  ;"Device error during REFRESH"

;   $ELSE                         ;SINCE NOT ERROR CODE 0AH,
    JMP SHORT $$EN80
$$IF80:
       CMP   AX,RS_ERRC+MAPERR
;      $IF   E                    ;IF driver unable to download because he has no buffer THEN
       JNE $$IF82
          MOV   DX,OFFSET CPMSG21     ;PASS OFFSET TO MSG PARM LIST
                                  ;"Unable to perform REFRESH"
;      $ELSE
       JMP SHORT $$EN82
$$IF82:
          CMP   AX,DS_ERR16       ;DOES THE DEVICE DRIVER SUPPORT DESIGNATE START FUNCTION?
;         $IF   E,OR
          JE $$LL84
          CMP   AX,DS_ERR1        ;16=unknown command, 1=Invalid function number
;         $IF   E
          JNE $$IF84
$$LL84:
             MOV   DX,OFFSET CPMSG15     ;PASS OFFSET TO MSG PARM LIST
                                  ;"Codepage operation not supported on this device",BEEP,CR,LF,EOM

;         $ELSE                   ;SINCE NOT THAT EITHER, ASSUME MUST BE CODE 8
          JMP SHORT $$EN84
$$IF84:
             MOV   DX,OFFSET CPMSG19     ;PASS OFFSET TO MSG PARM LIST
                                  ;"Current keyboard does not support this Codepage",BEEP,CR,LF,EOM
;         $ENDIF
$$EN84:

;      $ENDIF                     ;PREV prepare DELETED?
$$EN82:
;   $ENDIF                        ;WHICH ERROR CODE?
$$EN80:
    CALL  SEND_MSG                ;DISPLAY INDICATED MESSAGE

;$ENDIF                        ;prepare START OK?
$$EN78:

 RET
DO_REFRESH ENDP
; = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
DEVICE_TO_BINARY PROC NEAR

 MOV   DX,DEVICE_STATUS        ;GET BITS DEFINING DEVICE
 OR    DX,BINARY_DEV           ;TURN ON THE "BINARY" FLAG BIT
 XOR   DH,DH                   ;BE SURE DH IS CLEAR
 MOV   BX,DEV_HANDLE           ;PASS THE DEVICE HANDLE TO IOCTL
 DOSCALL IOCTL,IOCTL_FUN_SET_INFO ;SET DEVICE TO "BINARY"

 RET
DEVICE_TO_BINARY ENDP
; = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
 HEADER <'STATUS' - QUERY FUNCTION HANDLER>

PUBLIC   do_status

DO_STATUS PROC NEAR

 CALL  setup_packet_for_DBCS
 MOV   CX,DEV_TYPE             ;CX=xxx_DEV_TYPE * 256 + "MINOR CODE"
 MOV   CL,QUERY_SELECTED       ;SET MINOR CODE TO "QUERY SELECT"
 MOV   DX,OFFSET PK            ;DS:DX=DATA BUFFER "PACKET"
 CALL  DO_GENERIC_IOCTL

;$IF   C
 JNC $$IF90
    CALL  QUERY_ERROR          ;DISPLAY CAUSE OF PROBLEM

;$ELSE                         ;SINCE NOT ERROR ON QUERY
 JMP SHORT $$EN90
$$IF90:
    MOV   AX,PK.PACKCPID       ;GET CODEPAGE ID FROM PACKET
    MOV   CPMSGLST6CP,AX       ;PASS CODEPAGE ID TO PRINTF
    MOV   AX,CP_CB.cp_device      ;GET POINTER TO DEVICE NAME
    MOV   CPMSGLST6DEV,AX      ;OFFSET TO DEVICE NAME MOVED TO MSG PARM LST
    MOV   DX,OFFSET CPMSG6     ;PASS OFFSET TO MSG PARM LIST
    CALL  printf                ;DISPLAY THE MESSAGE:
                               ; "CURRENTLY SELECTD CODEPAGE IS %D FOR DEVICE: %S",CR,LF,0

    CALL  QUERY_LISTER         ;DISPLAY LIST OF PREPPED CODEPAGES

;$ENDIF                        ;ERR FROM QUERY ACTIVE?
$$EN90:

 RET
DO_STATUS ENDP
; = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
QUERY_LISTER PROC NEAR

 MOV   CX,DEV_TYPE             ;CX=xxx_DEV_TYPE * 256 + "MINOR CODE"
 MOV   CL,QUERY_DES_LST        ;SET MINOR CODE TO "QUERY prepare LIST"
 MOV   DX,OFFSET RESPONSE_LIST ;DS:DX=DATA BUFFER
 CALL  DO_GENERIC_IOCTL

;$IF   C                       ;IF PROBLEM WITH QUERY LIST
 JNC $$IF93
    CALL  QUERY_ERROR          ;DISPLAY CAUSE OF PROBLEM

;$ELSE                         ;SINCE NO PROBLEM WITH QUERY LIST
 JMP SHORT $$EN93
$$IF93:
    MOV   SI,OFFSET RES_HWCP   ;START WITH THE LIST OF HARDWARE CODEPAGES
    MOV   CX,[SI]              ;GET NUMBER OF HARDWARE CODEPAGES IN LIST IN PACKET
    ADD   SI,WORD              ;START WITH THE FIRST CODEPAGE
;   $IF   NCXZ                 ;IF ANY HARDWARE CODEPAGES ARE PRESENT
    JCXZ $$IF95
;AC001;MOV   CPMSGLST8HD,OFFSET CPMSG8_HW ;GET OFFSET OF "HARDWARE" TO MSG PARM LIST
       PUSH CX                ;protect from SYSGETMSG ;AC001;
       PUSH SI                ;protect from SYSGETMSG ;AC001;
       set_submessage_ptr  CPMSG8_HW,cpmsg8 ;GET OFFSET OF "HARDWARE" TO MSG PARM LIST;AC001;
       POP  SI
       POP  CX
       CALL  DISPLAY_CPID

;   $ENDIF                     ;HARDWARE CODEPAGES PRESENT?
$$IF95:
    MOV   CX,[SI]              ;GET NUMBER OF prepare CODEPAGES IN LIST IN PACKET
    ADD   SI,WORD              ;LOOK AT NEXT CODEPAGE ENTRY IN LIST
;   $IF   NCXZ                 ;IF ANY prepare CODEPAGES ARE PRESENT
    JCXZ $$IF97
;AC001;MOV   CPMSGLST8HD,OFFSET CPMSG8_PR ;GET OFFSET OF "PREPARED" TO MSG PARM LIST
       PUSH  CX               ;protect from SYSGETMSG ;AC001;
       PUSH  SI               ;protect from SYSGETMSG ;AC001;
       set_submessage_ptr  CPMSG8_PR,cpmsg8 ;GET OFFSET OF "PREPARED" TO MSG PARM LIST ;AC001;
       POP   SI
       POP   CX
       CALL  DISPLAY_CPID

;   $ENDIF                     ;PREPARED CODEPAGES PRESENT?
$$IF97:
    CMP   PREPED,0             ;CHECK COUNT OF PREPED CODEPAGES
;   $IF   Z                    ;IF NONE,
    JNZ $$IF99
       MOV   AX,CP_CB.cp_device   ;GET POINTER TO DEVICE NAME
       MOV   CPMSGLST7DEV,AX   ;OFFSET TO DEVICE NAME MOVED TO MSG PARM LST
       MOV   DX,OFFSET CPMSG7     ;PASS OFFSET TO MSG PARM LIST
       CALL  printf             ;DISPLAY THE MESSAGE:
                               ; "NO CODEPAGES PREPARED FOR DEVICE: %S",CR,LF,0
;   $ENDIF                     ;PREPED PAGES?
$$IF99:
;$ENDIF                        ;ERR FROM QUERY LIST?
$$EN93:
 CMP   NOERROR,TRUE
;$IF   E                       ;IF no previous errors THEN
 JNE $$IF102
;AC001;    MOV   CPMSGLST10FUN,OFFSET CPMSG10_QUERY ;SET MSG TO SAY "QUERY"
    set_submessage_ptr  cpmSGxx_QUERY,cpmsg10       ;SET MSG TO SAY "QUERY" ;AN001;
    MOV   DX,OFFSET CPMSG10     ;PASS OFFSET TO MSG PARM LIST
    CALL  printf                ;"MODE QUERRY CODEPAGE FUNCTION COMPLETED"
;$ENDIF
$$IF102:

 RET
QUERY_LISTER ENDP
; = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =


QUERY_ERROR PROC NEAR


;CARRY WAS SET AFTER "QUERY SELECT" OR "QUERY LIST".

 CALL  EX_ERR                  ;GET EXTENDED ERROR
                               ;AX=EXTENDED ERROR
                               ;BH=ERROR CLASS
                               ;BL=SUGGESTED ACTION
                               ;CH=LOCUS
 CMP    AX,QS_ERR7+MAPERR      ;CODE PAGE NOT SELECTD?

 .IF <AX EQ <QS_ERR7+MAPERR>> THEN  ;CODE PAGE NOT SELECTD?     ;AC665;
    MOV   DX,OFFSET CPMSG16     ;PASS OFFSET TO MSG PARM LIST
                               ;"NO CODEPAGE HAS BEEN SELECTED"
    CALL  SEND_MSG             ;DISPLAY INDICATED ERROR MESSAGE

    MOV   NOERROR,TRUE         ;FORGET THE ABOVE WAS FLAGGED AS "ERROR"
    CALL  QUERY_LISTER         ;LIST PREPPED CODEPAGES ANYWAY

 .ELSEIF <AX EQ DS_ERR16> OR   ;016H UNKNOWN COMMAND, i.e. device driver not loaded ;AN665;
 .IF <AX EQ DS_ERR1> THEN      ;;AN665;16=unknown command, 1=Invalid function number
    MOV   DX,OFFSET CPMSG15    ;AN665;PASS OFFSET TO MSG PARM LIST, "Codepage operation not supported on this device"
    CALL  SEND_MSG              ;DISPLAY INDICATED ERROR MESSAGE

 .ELSE                         ;NOT ERR7 or 16, MUST BE DEVICE ERROR ;AC665;
;AC002;    MOV   CPMSGLST17FUN,OFFSET CPMSG17_QUERY
    set_submessage_ptr  CPMSGxx_QUERY,cpmsg17
    MOV   DX,OFFSET CPMSG17     ;"DEVICE ERROR DURING QUERY"

 .ENDIF                        ;CP NOT PREPARED?

 RET
QUERY_ERROR ENDP
; = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
DISPLAY_CPID PROC NEAR
;INPUT: SI=OFFSET TO CODEPAGE ID WORD IN LIST

 MOV   DX,OFFSET CPMSG8        ;PASS OFFSET TO MSG PARM LIST
 CALL  printf                   ;DISPLAY THE MESSAGE:
                               ; "%s CODEPAGES:"
 CLD                           ;REQUEST INCREMENT IF INDEX
;$DO
$$DO104:
    LODSW                      ;GET CODEPAGE ID FROM PACKET
                               ;SI NOW POINTS TO NEXT WORD IN CODEPAGE LIST
    CMP   AL,EMPTY
;   $IF   E
    JNE $$IF105
       MOV   DX,OFFSET CPMSG14     ;PASS OFFSET TO MSG PARM LIST
                               ; "  Codepage unprepared",CR,LF,0
;   $ELSE                      ;SINCE NOT EMPTY,
    JMP SHORT $$EN105
$$IF105:
       INC   PREPED            ;COUNT PREPED CODEPAGES
       MOV   CPMSGLST9CP,AX    ;PASS CODEPAGE ID TO PRINTF
       MOV   DX,OFFSET CPMSG9     ;PASS OFFSET TO MSG PARM LIST
                               ; "  Codepage %d",CR,LF,0
;   $ENDIF                     ;EMPTY?
$$EN105:
    CALL  printf                ;DISPLAY THE MESSAGE

;$ENDDO LOOP
 LOOP $$DO104

 RET
DISPLAY_CPID ENDP
; = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
 HEADER <SUBROUTINES TO HANDLE MESSAGES TO STDOUT OR STDERR>
SEND_MSG PROC NEAR             ;USED TO SEND "ERROR" MESSAGES ONLY
                               ;FOR NON-ERROR MESSAGES, CALL printf DIRECTLY.
;INPUT: DX=OFFSET TO PRINTF MESSAGE PARM LIST

 MOV   NOERROR,FALSE           ;SET ERROR TO ERRORLEVEL
 CALL  printf

 RET
SEND_MSG ENDP
; = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
EX_ERR PROC NEAR
;THE DOSCALL RESPONDED WITH CARRY, SO GET EXTENDED ERROR
 MOV   BX,WORD PTR MINOR_VERSION ;BX = 30 VERSION NUMBER (FOR 3.30)
 DOSCALL EXTERROR              ;EXTENDED ERROR
                               ;OUTPUT:
                               ;AX=EXTENDED ERROR
                               ;BH=ERROR CLASS
                               ;BL=SUGGESTED ACTION
                               ;CH=LOCUS
 RET
EX_ERR ENDP
; = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
DO_GENERIC_IOCTL PROC NEAR

PUBLIC DO_GENERIC_IOCTL

;INPUT CX=xxx_DEV_TYPE * 256 + "MINOR CODE"
;      DS:DX=DATA BUFFER

 MOV   BX,DEV_HANDLE           ;BX=HANDLE
 MOV   AX,GENERIC_IOCTL        ;AX="GENERIC_IOCTL"
 DOSCALL                       ;SELECT DEVICE WITH GENERIC_IOCTL

 RET
DO_GENERIC_IOCTL ENDP
; = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
 HEADER <INT 24H - CRITICAL ERROR HANDLER, AND SUBROUTINES>
HANDLER24 PROC NEAR            ;CRITICAL ERROR HANDLER FOR INT 24H
;WHEN THE CRITICAL ERROR OCCURS, CONTROL IS TRANSFERRED TO INTERRUPT 24H.
;BP:SI POINTS TO THE DEVICE HEADER CONTROL BLOCK.

;ERROR CODE IS IN THE LOWER HALF OF THE DI REG WITH UPPER HALF UNDEFINED.
;ERROR CODES ARE DEFINED IN THE "MODECPEQ.INC" MODULE.

 MOV   CRITERROR,DI            ;SAVE THE ABOVE ERROR CODE

;AT EXIT, WHEN THE IRET IS EXECUTED, DOS RESPONDS ACCORDING TO (AL)

 MOV   AL,CRERR_FAIL           ;REQUEST THE "FAIL" OPTION

 IRET                          ;RETURN FROM INTERRUPT
HANDLER24 ENDP
; = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
SAVE_VECTOR24 PROC NEAR
;GET THE ORIGINAL CONTENTS OF THE CRITICAL ERROR HANDLER VECTOR, 24H
; AND SAVE THEM IN THE DWORD AT "OLDINT24"

 PUSH  ES                      ;SAVE BASE SEGREG
 DOSCALL GET_VECTOR,INT24      ;GET ORIGINAL OWNER OF INT 24H VECTOR

 MOV   WORD PTR OLDINT24,BX    ;OUTPUT: ES:BX = CONTENTS OF VECTOR
 MOV   WORD PTR OLDINT24+WORD,ES
 POP   ES                      ;RESTORE BASE SEGREG

 RET
SAVE_VECTOR24 ENDP
; = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
RESTORE_OLD_INT24 PROC NEAR
;TAKE THE CONTENTS OF THE ORIGINAL INT24 VECTOR, SAVED IN "OLDINT24"
; AND RESTORE THE INT24 ENTRY IN VECTOR BACK TO ITS ORIGINAL CONTENTS.

 PUSH  DS                      ;SAVE MY DATA SEGREG
 LDS   DX,OLDINT24             ;DS:DX = VECTOR TO INT HANDLER
 DOSCALL SET_VECTOR,INT24      ;RESTORE OLD POINTER INTO INT24 VECTOR

 POP   DS                      ;RESTORE MY DATA SEGREG

 RET
RESTORE_OLD_INT24 ENDP
; = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
HOOK_IN_MY_INT24 PROC NEAR
;SET THE CRITICAL ERROR INTERRUPT HANDLER IN
;  VECTOR TABLE TO POINT TO "HANDLER24"

 MOV   DX,OFFSET HANDLER24     ;DS:DX = VECTOR TO INT HANDLER
 DOSCALL SET_VECTOR,INT24      ;SET INT24 VECTOR TO POINT TO NEW HANDLER

 RET
HOOK_IN_MY_INT24 ENDP
; = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
;�������������������������������������������������������������������������������                                           ;AN000;
;�
;�
;�  SETUP_PACKET_FOR_DBCS
;�  ---------------------
;�
;�  Determine if need to include codepage information in the Invoke or Query
;�  packet.
;�
;�
;�
;�  INPUT: Global access to "pk", the Invoke or Query packet.
;�
;�
;�  RETURN: If needed, the DBCS vetcors are added to the packet and the packet
;�          size is incremented.
;�
;�  MESSAGES: none
;�
;�  REGISTER
;�  USAGE:
;�
;�
;�  SIDE EFFECT:
;�
;�
;�  PSEUDO CODE:
;�
;�  set up for and call INT 21 function 6507
;�  IF the length field of the DBCS table is non-zero THEN
;�     add the length of the table to the length field of the packet
;�  ENDIF
;�
;�
;�  DATA STRUCTURES: PK - the vectors are moved into this structure beginning
;�                   at PACKVECTOR1.  The count of the vectors is added to
;�                   PACKLEN.
;�
;�                   DBCS_header - The return from function 6507.
;�
;�                   DBCS_table - the table of vectors pointed to by
;�                                DBCS_header.table_ptr.
;�
;�
;�  ASSUMPTIONS: No more than 12 (0CH) are in the DBCS table.
;�
;�               The double word pointer returned by function 6507
;�               (DBCS_header.table_ptr) always points to the DBCS table.
;�
;�               The first byte of the table (DBCS_table.table_len) is zero if
;�               DBCS support is not necessary.
;�                                                                                                                         ;AN000;
;�               ES=DS.                                                                                                    ;AN000;
;�                                                                                                                         ;AN000;
;�������������������������������������������������������������������������������                                           ;AN000;

setup_packet_for_DBCS   PROC  NEAR

PUBLIC setup_packet_for_DBCS

PUSH  DS                                  ;DS is used to address the DBCS vector table which is somewhere in IBMDOS

;set up for and call INT 21 function 6507
MOV   AX,6507H                            ;get ext country info, DBCS info
MOV   BX,-1                               ;use codepage currently active for CON
MOV   DX,-1                               ;use default contry ID
MOV   CX,TYPE DBCS_head                   ;length of data to be returned
MOV   DI,OFFSET DBCS_headr                ;DI=>buffer to fill with info ID and pointer to table
INT   21H

LDS   SI,DBCS_header.table_ptr            ;DS:SI=>DBCS table,"DBCS_table" EQU DS:[SI]
.IF <DBCS_table.table_len NE 0> THEN      ;IF there are some vectors THEN
   MOV   CX,DBCS_table.table_len          ;set length of the list of vectors
   ADD   CS:pk.packlen,CX
   ADD   SI,vector1                       ;DS:SI=>first DBCS vector
   MOV   DI,OFFSET pk                     ;ES:DI=>the query or invoke packet
   ADD   DI,packvector1                   ;ES:DI=>holder for first DBCS vector in the query/invoke packet
   REP   MOVSB                            ;mov all the vectors and the two bytes of zeros into the packet
.ENDIF

POP   DS                                  ;restore to address stuff in MODE's segment

RET

setup_packet_for_DBCS   ENDP


PRINTF_CODE ENDS               ;END OF MODECP.SAL MODULE
END
