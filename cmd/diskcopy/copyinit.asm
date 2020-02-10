        PAGE    90,132                  ;A2
        TITLE   COPYINIT -- DISKCOPY INITIALIZATION PROGRAM
;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */
;****************** START OF SPECIFICATIONS *****************************
; MODULE NAME: COPYINIT

; DESCRIPTIVE NAME: Initialization for Diskette to diskette copy Utility

;FUNCTION: DISKCOPY is to copy the contents of the diskette in the
;          specified source drive to the diskette in the target
;          drive.  If necessary, the target diskette is also
;          formatted.

;          Multiple copies may be performed with one load of DISKCOPY.
;          A prompt, "Copy another (Y/N)?" permits additional
;          executions, all with the same drive specifications.

; ENTRY POINT: "DISKCOPY" at ORG 100h, jumps to "BEGIN".

; INPUT: (DOS command line parameters)
;        [d:][path]DISKCOPY [d: [D:]][/1]

;        Where

;        [d:][path] before DISKCOPY to specify the drive and path that
;                   contains the DISKCOPY command file.

;        [d:]       to specify the source drive id

;        [D:]       to specify the destination drive id

;        [/1]       to request single sided operations only

; EXIT-NORMAL: Errorlevel = 0
;             Function completed successfully.

; EXIT-ERROR: Errorlevel = 1
;             Abnormal termination due to error, wrong DOS,
;             invalid parameters, unrecoverable I/O errors on
;             the diskette.

; EFFECTS: The entire source diskette is copied, including the unused
;          sectors.  There is no awareness of the separate files
;          involved.  A unique volume serial number is generated
;          for the target diskette.

; INCLUDED FILES:
;       INCLUDE DCPYMACR.INC
;       INCLUDE DISKCOPY.EQU
;       INCLUDE PATHMAC.INC             ;PATHGEN MACRO

; INTERNAL REFERENCES:
;    ROUTINES:
;       INIT - INITIALIZATION ROUTINE, MAIN PROGRAM
;       SOURCE_TARGET_DRV - CONVERT SOURCE/TARGET DRIVE TO BIOS VALUES
;       TEST_DRIVE_VALIDITY - ARE SOURCE/TARGET DRIVES VALID?
;       DOS_DRIVE_VALIDITY - CHECK DOS DRIVE VALIDITY BYTE
;       TEST_REMOVABLE - IS SPECIFIED DRIVE REMOVABLE?
;       CHK_SINGLE_DRIV_OP - IS TARGET DRIVE SAME AS SOURCE?
;       GET_LOGICAL_DRIVE - GET LOG. DRIV NO. WHO OWNS PHYSICAL DRIVE
;       DISKETTE_DRV_TYPE - CHECK COMPATABILITY SOURCE/TARGET DRIVES
;       CHECK_REDIRECTION - IS DEVICE REDIRECTED?
;       BUFFER_SIZE - FINDS START AND END OF BUFFER
;       SETUP_CTRL_BREAK - SETUP THE CTRL-BREAK VECTOR
;       CHECK_SERVER - IS SERVER OR REDIRECTOR LOADED?

;    DATA AREAS:
;       PSP - Contains the DOS command line parameters.
;       WORKAREA - Temporary storage

; EXTERNAL REFERENCES:
;    ROUTINES:
;       SYSDISPMSG - Uses the MSG parm lists to construct the messages
;                on STDOUT.
;       SYSLOADMSG - Loads messages, makes them accessable.
;       PARSER - Processes the DOS Command line, finds parms.

;    DATA AREAS:
;        DCOPYSM.SAL - Defines the control blocks that describe the messages
;        DCOPYPAR.SAL - Defines the control blocks that describe the
;               DOS Command line parameters.

; NOTES:
;        This module should be processed with the SALUT preprocessor
;        with the re-alignment not requested, as:

;               SALUT COPYINIT,NUL

;        To assemble these modules, the alphabetical or sequential
;        ordering of segments may be used.

;        For instructions as to how to LINK, see prolog for DISKCOPY.

;PROGRAM AUTHOR: Original written by: JK
;                4.00 modifications by: EMK
;       ;C02    MKS     Bug#727 against DISKCOMP wasn't allowing two 720k
;                       diskettes to DISKCOMP or DISKCOPY if they were in
;                       drives of differing capacities.  This is fixed by
;                       checking for drive type 7 as well as 2, and permitting
;                       the copy if both diskettes are 720k.  The equate
;                       DRV_OTHER comes from DISKCOPY.EQU.
;       ;C05    MKS     Bug#1061.  Removeable hard disks were causing problems.
;                       Now they are disallowed.  FixedDisk comes from
;                       DISKCOPY.EQU.
;****************** END OF SPECIFICATIONS *****************************
        IF1
            %OUT    COMPONENT=DISKCOPY, MODULE=COPYINIT.SAL
        ENDIF

;DATE:   9-22-83
;TIME:   8:00 PM
;DATE:   10-30-84 - chk_para routine added. many parts are modified to
;        permit DISKCOPY /1, DISKCOPY D: /1 cases. Restore diskbase
;        before return to DOS when invalid DOS version occurs.
;DATE:   3-27-85  MAIN PARTS OF DISKCOPY PROGRAM HAS BEEN REWRITTEN
;        TO USE NEW IOCTL FUNCTION CALLS - READ, WRITE AND FORMAT.

        INCLUDE DCPYMACR.INC
        INCLUDE PATHMAC.INC             ;AN015;PATHGEN MACRO

CSEG    SEGMENT PARA PUBLIC 'CODE'      ;AN000;
        ASSUME  CS:CSEG, DS:CSEG, ES:CSEG, SS:CSEG

        INCLUDE DISKCOPY.EQU
	INCLUDE	VERSION.INC		; M003
;$salut (4,2,9,41)
;****************************************************************************
;                                                                           *
;                        EXTERNAL VARIABLES                                 *
;                                                                           *
;****************************************************************************

 EXTRN  PARSER:NEAR                     ;DCOPYPAR.SAL - DRIVES SYS PARSER       ;AN000;

 EXTRN  RECOMMENDED_BYTES_SECTOR:WORD   ;SOURCE DRIVE DEFAULT BYTES/SECTOR
 EXTRN  S_OWNER_SAVED:BYTE
 EXTRN  T_OWNER_SAVED:BYTE
 EXTRN  ASCII_DRV1_ID:BYTE              ;40H    SOURCE DRIVE ID IN ASCII
 EXTRN  ASCII_DRV2_ID:BYTE              ;40H    TARGET DRIVE ID IN ASCII
 EXTRN  MSGNUM_INVALID_DRV:BYTE         ;"INVALID DRIVE SPECIFICATION"          ;AC000;
 EXTRN  MSGNUM_NOT_COMPATIBLE :BYTE     ;"DEVICE TYPE OF DISKETTE TYPES NOT COMPATIBLE";AC000;
 EXTRN  MSGNUM_DRV_REDIRECTED:BYTE      ;"INVALID, DRIVE REDIRECTED"            ;AC000;
 EXTRN  MSGNUM_OPTIONS       :BYTE      ; /? options message
 EXTRN  MSG_OPTIONS_FIRST    :ABS       ; /? first options message
 EXTRN  MSG_OPTIONS_LAST     :ABS       ; /? last options message
IF	IBMCOPYRIGHT NE TRUE		; M003
 EXTRN  MSGNUM_CMCDD_DRIVE   :BYTE	; M003
ENDIF					; M003

 EXTRN  SENDMSG              :NEAR      ; message sender

 EXTRN  SUBLIST_8            :WORD      ;                                       ;AN000;
 EXTRN  SUBLIST_9            :WORD      ;                                       ;AN000;
 EXTRN  SUBLIST_13           :WORD      ;                                       ;AN000;
 EXTRN  SUBLIST_17A          :WORD      ;                                       ;AN000;
 EXTRN  SUBLIST_17B          :WORD      ;                                       ;AN000;
 EXTRN  SUBLIST_17C          :WORD      ;                                       ;AN000;
 EXTRN  SUBLIST_19C          :WORD      ;                                       ;AN000;
 EXTRN  SUBLIST_19D          :WORD      ;                                       ;AN000;
 EXTRN  SUBLIST_19E          :WORD      ;                                       ;AN000;
 EXTRN  SUBLIST_26A          :WORD      ;                                       ;AN001;
 EXTRN  SUBLIST_26B          :WORD      ;                                       ;AN001;
 EXTRN  SUBLIST_PARSE:WORD              ;PARSE ERROR XX - %0                         ;AN003;

.XLIST
;EXTRN  MSG_INVALID_PARM_PTR:BYTE       ;"INVALID PARAMETER"
;EXTRN  MSG_INVALID_DOS :BYTE           ;"INVALID DOS"
.LIST
 EXTRN  S_DRV_SECT_TRACK :BYTE          ;SECT/TRACK
 EXTRN  S_DRV_HEADS :BYTE               ;# OF HEADS
 EXTRN  S_DRV_TRACKS :WORD              ;# OF TRACKS
 EXTRN  T_DRV_SECT_TRACK :BYTE
 EXTRN  T_DRV_HEADS :BYTE
 EXTRN  T_DRV_TRACKS :WORD
 EXTRN  SOURCE_DRIVE :BYTE              ;SRC DRV LOGICAL NUMBER
 EXTRN  TARGET_DRIVE :BYTE              ;TARGET DRV LOGICAL NUMBER
 EXTRN  COPY_TYPE :BYTE                 ;1 = 1-DRIVE COPY  2 = 2-DRIVE COPY
 EXTRN  USER_OPTION :BYTE               ;NO OPTION (-1)  /1 (1), INVALID (9)
 EXTRN  BUFFER_BEGIN :WORD              ;STARTING BUFFER @ FOR LOADING
 EXTRN  BUFFER_END :WORD                ;ENDING BUFFER @ FOR LOADING
 EXTRN  MAIN_EXIT :WORD                 ;EXIT ADDRESS FOR CONTROL-BREAK
 EXTRN  SHOW_OPTIONS :BYTE              ; show command line options and exit

 EXTRN  IO_ERROR :BYTE

 EXTRN  DS_IOCTL_DRV_PARM :BYTE         ;PLACE HOLDER FOR DEFAULT SOURCE DRV PARM
 EXTRN  DT_IOCTL_DRV_PARM :BYTE         ;PLACE HOLDER FOR DEFAULT TARGET DRV PARM
 EXTRN  DS_specialFunctions :BYTE       ;AND THEIR CONTENTS
 EXTRN  DT_specialFunctions :BYTE
 EXTRN  DS_deviceType:BYTE
 EXTRN  DT_deviceType:BYTE
 EXTRN  DS_deviceAttributes :WORD
 EXTRN  DT_deviceAttributes :WORD
 EXTRN  DS_numberOfCylinders :WORD
 EXTRN  DT_numberOfCylinders :WORD
 EXTRN  DS_mediaType :BYTE
 EXTRN  DT_mediaType :BYTE
 EXTRN  DS_BPB_PTR :BYTE
 EXTRN  DT_BPB_PTR :BYTE

 EXTRN  MS_IOCTL_DRV_PARM :BYTE         ;DRIVE PARM FROM SOURCE MEDIUM
 EXTRN  MT_IOCTL_DRV_PARM :BYTE

 EXTRN  GENERIC_IOCTL :NEAR
 EXTRN  SET_LOGICAL_DRIVE :NEAR

;                  $salut (4,20,24,41)  ;                                       ;AN000;
MY_BPB             STRUC
CBYTE_SECT         DW  0                ; 200H  BYTES / SECTOR
CSECT_CLUSTER      DB  0                ; 2h    SECTORS / CLUSTER
CRESEV_SECT        DW  0                ; 1h    RESERVED SECTORS
CFAT               DB  0                ; 2h    # OF FATS
CROOTENTRY         DW  0                ; 70h   # OF ROOT ENTRIES
CTOTSECT           DW  0                ; 02D0h TOTAL # OF SECTORS
                                        ;   INC. BOOT SECT, DIRECTORIES
MEDIA_DESCRIP      DB  0                ;0FDh   MEDIA DISCRIPTOR
CSECT_FAT          DW  0                ; 2h    SECTORS / FAT
CSECT_TRACK        DW  0                ;
CHEAD              DW  0                ;
CHIDDEN_SECT       DD  0                ;
BIG_TOT_SECT       DD  0                ;
                   DB  6 DUP (0)        ;
MY_BPB             ENDS


;****************************************************************************
;                                                                           *
;                        VARIABLE DECLARATIONS                              *
;                                                                           *
;****************************************************************************
DRIVE_VALID        DW  ?                ;DRIVE VALIDITY INDICATOR
DEFAULT_DRV        DB  ?                ;DEFAULT DRIVE ID (0=A,1=B,ETC)
NUMBER_OF_DRV      DB  ?                ;TOTAL # OF DISKT DRIVES ON THE SYS
                                        ;(NUMBER_OF_DRV = 0 ---> 1 DRIVE)
ASCII_DRIVE_LETTER DB  " :",0
                   PATHLABL COPYINIT    ;AN015;
                   HEADER <INIT - INITIALIZATION ROUTINE, MAIN PROGRAM> ;       ;AN000;
;       $salut (4,9,15,41)              ;                                       ;AN000;
;#############################################################################
;                     INITIALIZATION ROUTINE - MAIN PROGRAM
INIT    PROC  NEAR
        PUBLIC INIT                     ;MAKE ENTRY IN LINK MAP                 ;AN000;

;OUTPUT: DX = EXIT CODE, "FINE"
;#############################################################################

        MOV   DRIVE_VALID,AX            ;SAVE DRIVE VALIDITY BYTE

;               REPLACE THE "FILL_SEG" IN THE SUBLIST MESSAGE CONTROL BLOCKS.

;               BECAUSE THIS IS A .COM STYLE FILE, THESE SEGID VALUES CANNOT
;               BE PROVIDED BY THE DOS SYSTEM LOADER, BUT MUST BE DYNAMICALLY
;               PERFORMED AT EXECUTION TIME AS PART OF A .COM FILE'S OBLIGATION
;               TO BE "SELF-RELOCATING".

        MOV   AX,CS                     ;GET SEGID OF COMMON SEGMENT            ;AN000;
        MOV   SUBLIST_8.SUB_VALUE_SEG,AX ;                                      ;AN000;
        MOV   SUBLIST_9.SUB_VALUE_SEG,AX ;                                      ;AN000;
        MOV   SUBLIST_13.SUB_VALUE_SEG,AX ;                                     ;AN000;
        MOV   SUBLIST_17A.SUB_VALUE_SEG,AX ;                                    ;AN000;
        MOV   SUBLIST_17B.SUB_VALUE_SEG,AX ;                                    ;AN000;
        MOV   SUBLIST_17C.SUB_VALUE_SEG,AX ;                                    ;AN000;
        MOV   SUBLIST_19C.SUB_VALUE_SEG,AX ;                                    ;AN000;
        MOV   SUBLIST_19D.SUB_VALUE_SEG,AX ;                                    ;AN000;
        MOV   SUBLIST_19E.SUB_VALUE_SEG,AX ;                                    ;AN000;
        MOV   SUBLIST_26A.SUB_VALUE_SEG,AX ;                                    ;AN001;
        MOV   SUBLIST_26B.SUB_VALUE_SEG,AX ;                                    ;AN001;
        MOV   SUBLIST_PARSE.SUB_VALUE_SEG,AX ;                                  ;AN003;

        CALL  SETUP_CTRL_BREAK          ;STEALS CTRL_BREAK
        CLD                             ;CLEAR DIRECTION FLAG
        MOV   DX,FINE                   ;ASSUME EVERYTHING IS FINE
.XLIST
;        CALL  SCREENING                 ;CHECK DOS VERSION AND INPUT PARMS
;        CALL  CHK_PARA                  ; GENERAL SYNTAX CHECK
.LIST
        CALL  PARSER                    ;LOOK AT DOS COMMAND LINE               ;AN000;

        CMP   DX,FINE                   ;IF ALL OK                              ;AN000;
;       $IF   E                         ;                                       ;AN000;
        JNE $$IF1

            ; If the user entered /? on the command line,
            ; show them the possible options and then exit.

            cmp   SHOW_OPTIONS, 0       ; does the user want them?
            je    ShowOptionsDone       ; jump if not
            call  DISPLAY_OPTIONS       ;  else display message
            mov   DX, offset MSGNUM_OPTIONS     ; to get back out
            jmp   short EXIT_INIT       ; and bail out with it
ShowOptionsDone:

            CALL  SOURCE_TARGET_DRV     ;SET UP TO USE THE DRIVE LETTERS        ;AN000;

            CALL  TEST_DRIVE_VALIDITY

            CMP   DX,FINE
;           $IF   E                     ;                                       ;AN000;
            JNE $$IF2
                CALL  DISKETTE_DRV_TYPE ;SOURCE & TARGET DRIVE TYPES

                CMP   DX,FINE           ;IF FINE & DANDY
;               $IF   E                 ;                                       ;AN000;
                JNE $$IF3
                    CALL  BUFFER_SIZE   ;GET BUFFER SIZE FOR COPYING

;               $ENDIF                  ;                                       ;AN000;
$$IF3:
;           $ENDIF                      ;                                       ;AN000;
$$IF2:
;       $ENDIF                          ;                                       ;AN000;
$$IF1:
EXIT_INIT:                              ;DX <-- 1  IF INIT OK
        RET                             ;DX <-- ERROR OFFSET IF NOT OK
                                        ;RETURN TO CALLER
INIT    ENDP                            ;END INITIALLIZATION PROGRAM

         HEADER <DISPLAY_OPTIONS - DISPLAY OPTIONS MSG>
;******************************************************************************
; SUBROUTINE NAME :  DISPLAY_OPTIONS                                          *
;                                                                             *
; INPUT           :  NONE                                                     *
;                                                                             *
; OUTPUT          :  NONE                                                     *
;                                                                             *
; FUNCTION        :  Displays all the lines of the user options message on    *
;                    standard output.                                         *
;                                                                             *
;******************************************************************************

        PUBLIC  DISPLAY_OPTIONS
DISPLAY_OPTIONS PROC NEAR

        push    di
        mov     di, offset MSGNUM_OPTIONS       ; get message
DO_LOOP:
        call    SENDMSG                 ; send this line
        cmp     word ptr[MSGNUM_OPTIONS], MSG_OPTIONS_LAST      ; last msg?
        je      DO_DONE                 ; done if so
        inc     word ptr[MSGNUM_OPTIONS]        ; else bump msg number
        jmp     short DO_LOOP           ; and go display it.
DO_DONE:
        pop     di
        ret

DISPLAY_OPTIONS ENDP

.XLIST
;        HEADER <SCREENING - CHECK DOS VERSION, SYNTAX PARMS>
;******************************************************************************
; SUBROUTINE NAME :  SCREENING  - CHECKS THE FOLLOWING:                       *
;                                 - DOS VERSION                               *
;                                 - GENERAL SYNTAX CHECKING FOR PARAMETERS    *
; INPUT           :  NONE                                                     *
; OUTPUT          :  DX : FINE    - NO ERROR                                  *
;                         (OTHERS)- ERROR MSG OFFSET                          *
;******************************************************************************

;SCREENING PROC NEAR
                                        ;CHECK DOS VERSION:
;        MOV   AH,DOSVER_FUNC            ;SEE IF CORRECT DOS VERSION
;        INT   21H                       ;FUNCTION CALL (AL <- DOS VERSION)
                                        ;NOTE: BX IS DESTROYED
;       XCHG    AH,AL                   ;AH=MAJOR VER, AL=MINOR VER
;        CMP   AX,expected_version       ;IF DOS MAJOR VERSION LESS THAN 3.00
;        $IF   NE                        ;THEN ISSUE ERROR MSG
;            MOV   DX,OFFSET MSG_INVALID_DOS
;            MOV   AH,PRINT_FUNC         ;USE PRINT FUNCTION TO TELL USER
;            INT   21H                   ;THAT HE IS USING THE OLD VERSION
;            INT   20H                   ;EXIT TO DOS
;        $ELSE                           ;VERSION OK
;            CALL  CHK_PARA              ;GENERAL SYNTAX CHECK
;        $ENDIF                          ;END VERSION TEST
;        RET
;SCREENING ENDP
;        HEADER <CHK_PARA - SYNTAX PARMS, OPTION /1>
;**************************************************************************

;kiser: this proc is to be deleted

;CHK_PARA PROC NEAR
;       CHECK SYNTAX OF THE ENTERED PARAMETERS                            *
;       ALSO, DETERMINE THE USER OPTION "/1" IS ENTERED OR NOT.           *
;  INPUT: DX = FINE                                                       *
;         IF /1 HAS BEEN ENTERED, THE VARIABLE USER_OPTION = OPTION_1     *
;         ELSE USER_OPTION = NO_OPTION.                                   *
;  OUTPUT: DX = FINE    - NO ERROR                                        *
;          OTHERWISE DX POINTS TO ERROR MSG                               *
;**************************************************************************
;        PUSH  CX
;        MOV   USER_OPTION, NO_OPTION    ;ASSUME NO /1 IS ENTERED.
;        XOR   CX, CX
;        MOV   CL, BYTE PTR DS:BEGIN_UNFORM_AREA ;GET # OF CHR
;        CMP   CL, 0
;        $IF   NZ
;            CLD                         ;CLEAR DIRECTION
;            MOV   DI, BEGIN_UNFORM_AREA+2 ;STARTING POINT OF PARA
;            DEC   CL                    ;TO IGNORE LAST CHR (0DH)
;            CALL  SKIP_BLANKS           ;SKIP BLANKS, IF ANY. THE POINTER
;                                        ; WILL POINT TO THE NEXT NON_BLANK CHR
;
;            $IF   NZ                    ;SOMETHING OTHER THAN BLANKS
;                                        ; ARE ENTERED
;                CALL  CHK_SLASH_ONE     ;IS NEXT WORD /1 ?
;
;                JNC   SLASH_ONE         ;YES
;                CALL  CHK_DRV_SPEC      ;IS IT A DRIVE SPECIFICATION LIKE d: ?
;
;                JC    INVALID_PARA      ;IF NOT, THEN ERROR
;                JZ    CHK_PARA_EXIT     ;NO MORE CHR? THEN, OK. (EX. DISKCOPY D:)
;                CALL  CHK_SLASH_ONE     ;IS NEXT WORD /1 ?
;
;                JNC   SLASH_ONE         ;YES.(EX. DISKCOPY D:/1)
;                CALL  CHK_BLANK         ;IF NOT, NEXT CHR SHOULD BE A BLANK.
;
;                JC    INVALID_PARA      ;OTHERWISE, ERROR.
;                CALL  SKIP_BLANKS       ;SKIP BLANKS, IF ANY.
;
;                JZ    CHK_PARA_EXIT     ;(EX. DISKCOPY D:  )
;                CALL  CHK_SLASH_ONE     ;IS IT A /1 ?
;
;                JNC   SLASH_ONE         ;YES. (EX. DISKCOPY D:  /1)
;                CALL  CHK_DRV_SPEC      ;IF NOT /1, THEN IS IT A DRV SPEC?
;
;                JC    INVALID_PARA      ;OTHERWISE, ERROR.
;                CALL  SKIP_BLANKS       ;SKIP BLANKS, IF ANY.
;
;                JZ    CHK_PARA_EXIT     ;NO MORE CHR. (EX. DISKCOPY D: D:)
;                CALL  CHK_SLASH_ONE     ;OTHERWISE, IT SHOULD BE /1.
;
;                JNC   SLASH_ONE         ;YES, /1. JMP TO SLASH_ONE
;                JMP   INVALID_PARA      ;PARAMETER ERROR.
;SLASH_ONE:
;                MOV   USER_OPTION, OPTION_1 ;YES, /1 HAS BEEN ENTERED.
;                CALL  SKIP_BLANKS       ;/1 SHOULD BE END OF PARAMETERS, OR ONLY BLANKS CAN FOLLOW.
;
;                $IF   NZ
;INVALID_PARA:
;                    MOV   DX,OFFSET MSG_INVALID_PARM_PTR ;WRONG PARM ENTERED MSG
;                $ENDIF
;            $ENDIF
;        $ENDIF
;CHK_PARA_EXIT:
;        POP   CX
;
;        RET
;CHK_PARA ENDP
;        HEADER <SKIP_BLANKS - IGNORE BLANKS/TABS IN PARMS PARSING>
;***************************************************************************
;SKIP_BLANKS PROC NEAR
; ** SKIP BLANKS, OR TABS IF ANY, IN THE PARAMETER STRING.                 *
; INPUT: ES:DI POINTS TO THE CURRENT CHR.                                  *
;        CX - # OF REMAINING CHR IN THE STRING.                            *
; OUTPUT: ES:DI POINT TO THE NEXT NON_BLANK CHR.                           *
;         CX IS ADJUSTED ACCORINGLY.                                       *
;         IF THE CURRENT CHR IS NOT A BLANK, THEN DI, CX VALUE NOT CHANGED.*
;         IF CX = 0, THEN ZERO FLAG WILL BE SET AND EXIT THIS PROC.        *
;***************************************************************************
;        $DO
;            MOV   AL, 20H               ;20H=BLANK
;            CLD                         ;CLEAR DIRECTION
;            REPE  SCASB
;        $LEAVE Z                        ;IF NOT FOUND A NON_BLANK CHR YET, AND CX=0, EXIT THIS ROUTINE.
;            DEC   DI                    ;OTHERWISE, RESTORE DI TO THE NON_BLANK POSITION.
;            INC   CX                    ;  AND RESTORE CX TO WHERE IT WAS AT NON_BLANK CHR
;                                        ;(IF FOUND A NON_BLANK CHR, ZERO FLAG WOULD NOT BE SET)
;            MOV   AL, ES:BYTE PTR [DI]
;            CMP   AL, 09H               ;09H=TAB
;        $LEAVE NZ                       ;IF THE NON_BLANK CHR IS NOT A TAB THEN EXIT
;            INC   DI                    ;ELSE TRY SKIP AGAIN
;            DEC   CX
;        $ENDDO
;        RET
;SKIP_BLANKS ENDP
;        HEADER <CHK_SLASH - IS CURRENT PARM /1>
;***************************************************************************

;kiser: this proc is to be deleted

;CHK_SLASH_ONE PROC NEAR
; ** CHECK CURRENT CHR IS / FOLLOWED BY 1.                                 *
; INPUT: ES:DI POINTS TO THE CURRENT CHR TO BE CHECKED.                    *
;        CX REPRESENTS THE # OF CHR'S IN THE STRING.                       *
; OUTPUT: FOUND - DI POINTS TO THE NEXT CHR.  CX CHANGED ACCORDINGLY.      *
;                 IF THIS HAD BEEN A LAST WORD, ZERO FLAG WILL BE SET.     *
;         NOT FOUND - CARRY IS SET. DI, CX UNCHANGED.                      *
;***************************************************************************
;
;        CLC                             ;CLEAR CARRY FLAG
;        CMP   CX, 2                     ;# OF CHR IN THE STRING.
;        $IF   NL,AND                    ;IF LESS THAN 2, THEN SET CARRY AND EXIT.
;
;        MOV   AX, ES:WORD PTR [DI]      ;GET CURRENT WORD IN AX
;        CMP   AX, '1/'                  ;IS IT /1 ?
;        $IF   Z                         ;IF NOT, THEN SET CARRY AND EXIT
;            INC   DI                    ;ADJUST CX, DI TO THE NEXT CHR
;            INC   DI
;            DEC   CX
;            DEC   CX
;            CMP   CX, 0                 ;IF NO MORE CHR, THEN SET ZERO FLAG.
;        $ELSE
;            STC                         ;NOT FOUND, SET CARRY FLAG.
;        $ENDIF
;        RET
;CHK_SLASH_ONE ENDP
;        HEADER <CHK_DRV - CURRENT PARM CHAR IS DRIVE AND COLON?>
;***************************************************************************

;kiser: this proc is to be deleted

;CHK_DRV_SPEC PROC NEAR
; ** CHECK CURRENT CHR IS ALPHA CHR FOLLOWED BY COLON.                     *
; INPUT: ES:DI POINTS TO THE CURRENT CHR TO BE CHECKED.                    *
;        CX -- # OF CHR IN THE STRING.                                     *
; OUTPUT: FOUND - DI POINTS TO THE NEXT CHR. CX ADJUSTED ACCORDINGLY.      *
;                 IF THIS HAD BEEN A LAST WORD, ZERO FLAG WILL BE SET.     *
;         NOT FOUND - CARRY IS SET. DI, CX UNCHANGED.                      *
;***************************************************************************

;        CLC                             ;CLEAR CARRY
;        CMP   CX, 2                     ;# OF CHR REMAINING IN THE STRING.
;        $IF   NL,AND                    ;IF NOT LESS THAN 2, THEN FOUND
;                                        ;IF LESS THAN 2, THEN NOT FOUND - SET CARRY AND EXIT.
;        MOV   AL, ES:BYTE PTR [DI]      ;GET CURRENT CHR
;        AND   AL, 11011111B             ;CHANGE IT TO UPPER_CASE CHR.
;        CMP   AL, 'A'
;        $IF   NB,AND                    ;NOT BELOW 'A', THEN MAYBE FOUND OK
;
;        CMP   AL, 'Z'
;        $IF   NA,AND                    ;NOT ABOVE 'Z', THEN FOUND
;
;        MOV   AL, ES:BYTE PTR [DI+1]    ;LOOK AHEAD THE FOLLOWING CHR.
;        CMP   AL, ':'                   ;SHOULD BE A COLON.
;        $IF   Z                         ;IF FOUND.
;            INC   DI                    ;FOUND. ADJUST CX, DI TO THE NEXT CHR.
;            INC   DI
;            DEC   CX
;            DEC   CX
;            CMP   CX, 0                 ;IF NO MORE CHR, THAN SET THE ZERO FLAG.
;        $ELSE
;            STC                         ;SET CARRY
;        $ENDIF
;        RET
;CHK_DRV_SPEC ENDP
;        HEADER <CHK_BLANK - IS CURRENT CHAR IN PARM BLANK OR TAB>
;***************************************************************************

;kiser: this proc is to be deleted

;CHK_BLANK PROC NEAR
;; ** CHECK THE CURRENT CHR IS A BLANK OR TAB                               *
;; INPUT: ES:DI POINTS TO THE CURRENT CHR.                                  *
;        CX - # OF CHR IN THE STRING.                                      *
; OUTPUT: FOUND - DI MOVES TO THE NEXT CHR. CX DECREASES BY 1.             *
;         NOT FOUND - CARRY IS SET. DI, CX UNCHANGED.                      *
;***************************************************************************

;        CLC                             ;CLEAR CARRY
;        CMP   CX, 1                     ;IF LESS THAN 1, NOT FOUND.
;        $IF   L,OR                      ;GO SET CARRY AND EXIT
;
;        MOV   AL, ES:BYTE PTR [DI]      ;GET CURRENT CHR
;        CMP   AL, 020H                  ;020H=BLANK CHR
;        $IF   NZ,AND                    ;NOT FOUND
;        CMP   AL, 09H                   ;09H=TAB CHR
;        $IF   NZ                        ;NOT FOUND EITHER
;
;                                        ;THEN NOT FOUND
;            STC                         ;SET CARRY
;        $ELSE                           ;CHAR MUST BE EITHER TAB OR BLANK
;            INC   DI                    ;FOUND. ADJUST DI, CX
;            DEC   CX
;        $ENDIF
;        RET
;CHK_BLANK ENDP
.LIST
        HEADER <SOURCE_TARGET_DRV - CONV. SRC/TARGET DRV TO BIOS VALUES> ;      ;AN000;
;******************************************************************************
; SUBROUTINE NAME :  SOURCE_TARGET_DRV    DETERMINES SOURCE & TARGET DRIVES & *
;                                         CONVERT THEM FROM DOS TO BIOS VALUE *
; INPUT           :  SOURCE_DRIVE & TARGET_DRIVE HAVE DOS DRIVE ID'S:         *
;                         0 = DEFAULT        1 = DRV A                        *
;                         2 = DRV B          3 = DRV C ETC                    *
;                                                                             *
;                                                                             *
; OUTPUT          :  DEFAULT_DRV: CURRENT DEFAULT DRIVE                       *
;                          0 - DRIVE A           1 - DRIVE B                  *
;                          2 - DRIVE C           3 - DRIVE D                  *
;                                                                             *
;                 :  SOURCE_DRIVE          1 = DRIVE A    2 = DRIVE B  ETC.   *
;                 :  TARGET_DRIVE          1 = DRIVE A    2 = DRIVE B  ETC.   *
;                      (UNCHANGED)                                            *
;******************************************************************************
SOURCE_TARGET_DRV PROC NEAR
        PUBLIC SOURCE_TARGET_DRV        ;MAKE ENTRY IN LINK MAP                 ;AN000;
                                        ;GET CURRENT DEFAULT DRIVE
        MOV   AH,CURRENTDRV_FUNC        ;FUNCTION CALL (19H)
                                        ;(AL <- CURRENT DEFAULT DRV
        INT   21H                       ;0 = A, 1 = B, ETC)

        MOV   DEFAULT_DRV,AL            ;SAVE IT
        INC   AL                        ;NOW A=1, B=2, ETC                      ;AN000;
        CMP   SOURCE_DRIVE,ZERO         ;FIRST DRV ENTERED?                     ;AC000;
;       $IF   E                         ;NO DRIVE LETTER ENTERED
        JNE $$IF7
            MOV   SOURCE_DRIVE,AL       ;USE DEFAULT DRIVE AS SOURCE            ;AC000;
            MOV   TARGET_DRIVE,AL       ; AND AS TARGET                         ;AC000;
;       $ELSE
        JMP SHORT $$EN7
$$IF7:
            CMP   TARGET_DRIVE,ZERO     ;WAS THE SECOND DRIVE ID SPECIFIED?     ;AC000;
;           $IF   E                     ;NO, SO TARGET DRV IS DEFAULT           ;AC000;
            JNE $$IF9
                MOV   TARGET_DRIVE,AL   ;USE DEFAULT DRIVE AS TARGET            ;AC000;
;           $ENDIF
$$IF9:
;       $ENDIF
$$EN7:
        MOV   AX,WORD PTR SOURCE_DRIVE  ;SOURCE TO AL, TARGET TO AH             ;AC000;
        ADD   ASCII_DRV1_ID,AL          ;MAKE THE DRIVE ALPHABET READABLE
        ADD   ASCII_DRV2_ID,AH          ;IN THE MESSAGE

        RET

SOURCE_TARGET_DRV ENDP
        HEADER <TEST_DRIVE_VALIDITY - ARE SOURCE/TARGET DRIVES VALID?> ;        ;AN000;
;******************************************************************************
; SUBROUTINE NAME :  TEST_DRIVE_VALIDITY--MAKE SURE SOURCE AND TARGET DRIVES  *
;                    SPECIFIED BY USER ARE VALID FOR DISKCOPY                 *
;                                                                             *
; INPUT           :  SOURCE_DRIVE:BYTE, TARGET_DRIVE:BYTE                     *
;                                                                             *
; OUTPUT          :  DX='FINE' IF DRIVES ARE VALID, ELSE DX CONTAINS MSG PTR  *
;******************************************************************************

TEST_DRIVE_VALIDITY PROC NEAR
        PUBLIC TEST_DRIVE_VALIDITY      ;MAKE ENTRY IN LINK MAP                 ;AN000;

        CALL  DOS_DRIVE_VALIDITY

        CMP   DX,FINE
;       $IF   E,AND                     ;                                       ;AC000;
        JNE $$IF12

        MOV   BL,SOURCE_DRIVE
        CALL  CHECK_REDIRECTION

        CMP   DX,FINE
;       $IF   E,AND                     ;                                       ;AC000;
        JNE $$IF12

        MOV   BL,TARGET_DRIVE
        CALL  CHECK_REDIRECTION

        CMP   DX,FINE
;       $IF   E,AND                     ;                                       ;AC000;
        JNE $$IF12

        MOV   BL,SOURCE_DRIVE
        CALL  CHECK_SERVER

        CMP   DX,FINE
;       $IF   E,AND                     ;                                       ;AC000;
        JNE $$IF12

        MOV   BL,TARGET_DRIVE
        CALL  CHECK_SERVER

        CMP   DX,FINE
;       $IF   E,AND                     ;                                       ;AC000;
        JNE $$IF12

        CALL  TEST_REMOVABLE

        CMP   DX,FINE
;       $IF   E                         ;                                       ;AC000;
        JNE $$IF12

            CALL  CHK_SINGLE_DRV_OP     ;CHECK IF IT IS
                                        ; ONE PHYSICAL DRIVE OPERATION
;       $ENDIF                          ;                                       ;AC000;
$$IF12:
        RET

TEST_DRIVE_VALIDITY ENDP
        HEADER <DOS_DRIVE_VALIDITY - CHECK DOS DRIVE VALIDITY BYTE> ;           ;AN000;
;******************************************************************************
; SUBROUTINE NAME :  DOS_DRIVE_VALIDITY -- CHECK DOS DRIVE VALIDITY BYTE      *
;                                                                             *
; INPUT           :  DRIVE_VALID:BYTE                                         *
;                                                                             *
; OUTPUT          :  DX="FINE" IF DRIVES ARE VALID ELSE DX CONTAINS MESSAGE PTR *
;******************************************************************************

DOS_DRIVE_VALIDITY PROC NEAR

        CMP   DRIVE_VALID,0             ;SEE IF DRIVES ARE VALID DOS DEVICE
;       $IF   NE
        JE $$IF14
            MOV   DX,OFFSET MSGNUM_INVALID_DRV ;                                ;AC000;
;       $ENDIF
$$IF14:
        RET

DOS_DRIVE_VALIDITY ENDP
        HEADER <TEST_REMOVABLE - IS SPECIFIED DRIVE REMOVABLE?> ;               ;AN000;
;******************************************************************************
; SUBROUTINE NAME :  TEST_REMOVABLE -- CHECK IF DRIVES SPECIFED ARE REMOVABLE *
;                                                                             *
; INPUT           :  SOURCE_DRIVE:BYTE, TARGET_DRIVE:BYTE                     *
;                                                                             *
; OUTPUT          :  DX=FILE IF DRIVES ARE VALID ELSE DX CONTAINS MESSAGE PTR *
;******************************************************************************

TEST_REMOVABLE PROC NEAR

        MOV   BL,SOURCE_DRIVE           ;GET PARM 1 DRIVE ID

        MOV   AX,DRIVE_CHECK            ;CHECK FOR REMOVABLE DRIVE = 4408h
        INT   21H                       ;IOCTL CALL
;       $IF   NC                        ;IF DRIVE ID IS WITHIN RANGE
        JC $$IF16
            CMP   AX,REMOVABLE          ;THEN IF SOURCE DRIVE IS FIXED
;           $IF   NE                    ;  THEN
            JE $$IF17
                MOV   DX,OFFSET MSGNUM_INVALID_DRV ;GENERATE HARD               ;AC000;
                                        ; DRIVE ERROR MESSAGE
;           $ELSE                       ;ELSE, SRC IS REMOVABLE;
            JMP SHORT $$EN17
$$IF17:
                MOV   BL,TARGET_DRIVE   ;NOW GO CHECK TARGET

                MOV   AX,DRIVE_CHECK    ;CHECK FOR REMOVABLE DRIVE
                INT   21H               ;IOCTL CALL
;               $IF   NC                ;IF DRV WITHIN RANGE
                JC $$IF19
                    CMP   AX,REMOVABLE  ;THEN TGT DRV IS FIXED
;                   $IF   NE            ;     THEN
                    JE $$IF20
                        MOV   DX,OFFSET MSGNUM_INVALID_DRV ;GENERATE HARD       ;AC000;
                                        ; DRV ERROR MSG
;                   $ENDIF              ;END TEST IF TGT DRV IS FIXED
$$IF20:
;               $ELSE                   ;TGT DRV OUT OF RANGE.  EX. DRIVE X:
                JMP SHORT $$EN19
$$IF19:
                    MOV   DX,OFFSET MSGNUM_INVALID_DRV ;                        ;AC000;
;               $ENDIF                  ;END TEST IF TGT WITHIN RANGE
$$EN19:
;           $ENDIF                      ;END IF SRC IS REMOVABLE
$$EN17:
;       $ELSE                           ;ELSE,  SRC DRV OUT OF RANGE
        JMP SHORT $$EN16
$$IF16:
            MOV   DX,OFFSET MSGNUM_INVALID_DRV ;PRINT ERROR MSG                 ;AC000;
;       $ENDIF                          ;END TEST IF SRC DRV WITHIN RANGE
$$EN16:
        RET

TEST_REMOVABLE ENDP
        HEADER <CHK_SINGLE_DRIV_OP - IS TARGET DRIVE SAME AS SOURCE?> ;         ;AN000;
;******************************************************************************
; SUBROUTINE NAME :  CHK_SINGLE_DRV_OP                                        *
;                                                                             *
; INPUT           :  SOURCE_DRIVE - LOGICAL DRIVE NUMBER                      *
;                    TARGET_DRIVE                                             *
;                                                                             *
; OUTPUT          :  COPY_TYPE WILL BE SET TO ONE OR TWO DEPENDING ON THE     *
;                    TEST RESULT.  IF IT IS A SINGLE DRIVE COPY, THEN         *
;                    TARGET DRIVE LETTER WILL BE CHANGED TO THAT OF SOURCE.   *
;                    THE OWNERSHIP OF THE SOURCE AND TARGET DRIVE LETTER      *
;                    MIGHT HAVE BEEN CHANGED.                                 *
;                    SO, BEFORE EXIT TO DOS, THEY SHOULD BE RESET TO THE SAVED*
;                    ONE USING S_OWNER_SAVED AND T_OWNER_SAVED UNLESS THEY    *
;                    ARE EQUAL TO 0. (0 MEANS ONLY ONE DRIVE LETTER ASSIGNED.)*
;                    ASCII_DRV1_ID, ASCII_DRV2_ID MAY BE CHANGED ACCORDINGLY. *
;******************************************************************************

CHK_SINGLE_DRV_OP PROC NEAR

        PUSH  AX

        MOV   BL,SOURCE_DRIVE
        CALL  GET_LOGICAL_DRIVE

        MOV   S_OWNER_SAVED, AL         ;SAVE CURRENT OWNER DRIVE LETTER.
        MOV   BL, TARGET_DRIVE
        CALL  GET_LOGICAL_DRIVE

        MOV   T_OWNER_SAVED, AL         ;SAVE CURRENT OWNER
        MOV   BL, SOURCE_DRIVE
        CALL  SET_LOGICAL_DRIVE

        MOV   BL, TARGET_DRIVE
        CALL  SET_LOGICAL_DRIVE

        MOV   BL, SOURCE_DRIVE
        CALL  GET_LOGICAL_DRIVE         ;CHECK SOURCE DRV LETTER
                                        ; STILL HAS A OWNERSHIP.

        CMP   AL, SOURCE_DRIVE          ;
;       $IF   NE                        ;IF IT DOES NOT, THEN A
        JE $$IF27
                                        ; SINGLE DRIVE COPY.
            MOV   COPY_TYPE, ONE
            MOV   BL, SOURCE_DRIVE
            MOV   TARGET_DRIVE, BL      ;SET TARGET DRV LETTER
                                        ; TO THAT OF SOURCE
            MOV   BL, ASCII_DRV1_ID
            MOV   ASCII_DRV2_ID, BL
            MOV   BL, SOURCE_DRIVE
            CALL  SET_LOGICAL_DRIVE     ;SET THE OWNER BACK TO
                                        ; SOURCE DRV LETTER

;       $ELSE
        JMP SHORT $$EN27
$$IF27:
            CMP   AL, TARGET_DRIVE      ;SOURCE DRV LETTER = TARGET DRV
                                        ; LETTER CASE, FOR EX. DISKCOPY A: A:
;           $IF   E
            JNE $$IF29
                MOV   COPY_TYPE, ONE
;           $ELSE
            JMP SHORT $$EN29
$$IF29:
                MOV   COPY_TYPE, TWO
;           $ENDIF
$$EN29:
;       $ENDIF
$$EN27:

        POP   AX

        RET
CHK_SINGLE_DRV_OP ENDP
        HEADER <GET_LOGICAL_DRIVE - GET LOG. DRIV NO. WHO OWNS PHYS. DRIVE> ;AN000;
;******************************************************************************
GET_LOGICAL_DRIVE PROC NEAR
;       *** GET THE LOGICAL DRIVE NUMBER WHO HAS THE OWNERSHIP OF THE PHYSICAL
;           DRIVE.
;       INPUT: BL = DRIVE NUMBER (0=DEFAULT, 1=A, 2=B...)
;       OUTPUT: AL = DRIVE NUMBER (0= ONLY ONE DRIVE LETTER ASSIGNED TO THE
;                                     BLOCK DEVICE. OTHERWISE, 1=A, 2=B...)
;
;******************************************************************************

        MOV   AH, 44H
        MOV   AL, 0EH                   ; GET THE OWNER OF LOGICAL DRIVE NUMBER
        INT   21H
        CMP   AL, 0                     ;ONLY ONE DRIVE LETTER ASSIGNED?
;       $IF   E
        JNE $$IF33
            MOV   AL, BL                ;THEN SET THE INPUT DRIVE NUMBER TO AL.
;       $ENDIF
$$IF33:

        RET

GET_LOGICAL_DRIVE ENDP
        HEADER <DISKETTE_DRV_TYPE - CHECK COMPATABILITY SOURCE/TARGET DRIVES> ; ;AN000;
;******************************************************************************
; SUBROUTINE NAME :  DISKETTE_DRV_TYPE DOES THE FOLLOWING:                    *
;                    - GETS SOURCE, TARGET DRIVE INFORMATION                  *
;                    - CHECK REMOVABLE DRIVE                                  *
; *** REMARK: WILL NOT ALLOW DISKCOPY BETWEEN 5.25" AND 3.5" DRIVES.          *
; *** ALSO, IN THE MAIN PROGRAM, SOURCE MEDIA BPB INFORMATIONS (# OF SEC/TRK, *
; *** # OF TRACKS) SHOULD BE CHECKED AGAINST TARGET DEVICE INFORMATIONS.      *
; *** IF # OF SECT/TRACK, # OF TRACKS OF TARGET DEVICE ARE EQUAL TO, OR       *
; *** GREATER THAN THOSE OF THE SOURCE MEDIA BPB, THEN IT IS OK. OTHERWISE    *
; *** DEVICE NOT COMPATIBLE.                                                  *
; *** IF THIS DOES NOT GAURANTEES COMPATIBILITY BETWEEN SOURCE AND TARGET     *
; *** DEVICE OR MEDIA, EVENTUALLY, FAILURE TO FORMAT THE TARGET WILL          *
; *** TELL THAT SOURCE, TARGET DEVICE OR MEDIA ARE NOT COMPATIBLE.            *
;                                                                             *
;******************************************************************************
DISKETTE_DRV_TYPE PROC NEAR
        PUSH  AX

		; M003  Added check for CMCDD drives
IF IBMCOPYRIGHT NE TRUE
	mov	ax,(44h shl 8) or 11h ; Check if function supported
	xor	bx,bx
	mov	bl, SOURCE_DRIVE
	mov	cx,(8 shl 8) or 73h		; Determine if get_system_info
						; exist (only CMCDD has it).
	int	21h				; see if CMCDD
	jc	@f
	mov	dx, OFFSET MSGNUM_CMCDD_DRIVE
	jmp	$$EN35
@@:
ENDIF		; End M003

        xor   bx, bx
        MOV   BL, SOURCE_DRIVE
        MOV   CL, GETDEVPARM            ;=60h
        MOV   DX, OFFSET DS_IOCTL_DRV_PARM ;POINTER TO THE CONTROL STRING
        CALL  GENERIC_IOCTL             ;GET DEFAULT DEVICE PARM.

        TEST  DS_deviceAttributes, 0001h ;CHECK REMOVABLE. 0001 = NOT REMOVABLE
;       $IF   E,AND                     ;NO, CONTINUE                           ;AC000;
;C05    JNE $$IF35
        JE  check_fixed_disk                    ;Removable, hard disk also?;C05
        JMP $$IF35                              ;Not removeable, can't copy;C05
check_fixed_disk:                               ;Here to check for fixed d ;C05
        cmp   DS_DeviceType,FixedDisk           ;Q: Removeable fixed disk? ;C05
        JE  $$IF35                              ; Y: then error out        ;C05

        MOV   AX, DS_numberOfCylinders
        MOV   S_DRV_TRACKS, AX
        MOV   BX, OFFSET DS_BPB_PTR
        MOV   AX, [BX].CHead
        MOV   S_DRV_HEADS, AL
        MOV   AX, [BX].CSECT_TRACK
        MOV   S_DRV_SECT_TRACK, AL
        MOV   AX, [BX].CBYTE_SECT       ;RECOMMENDED BYTES/SECTOR
        MOV   RECOMMENDED_BYTES_SECTOR, AX

        XOR   BX,BX
        MOV   BL, TARGET_DRIVE
        MOV   CL, GETDEVPARM
        MOV   DX, OFFSET DT_IOCTL_DRV_PARM
        CALL  GENERIC_IOCTL             ;GET DEFAULT DEVICE PARM.

        TEST  DT_deviceAttributes, 0001h ;FIXED DISK?
;       $IF   Z                         ;TARGET IS NOT FIXED DISK, OK           ;AC000;
        JNZ $$IF35
        cmp   DT_DeviceType,FixedDisk           ;Q: Removeable fixed disk? ;C05
        JE  $$IF35                              ; Y: then error out        ;C05

            MOV   AX, DT_numberOfCylinders
            MOV   T_DRV_TRACKS, AX
            MOV   BX, OFFSET DT_BPB_PTR
            MOV   AX, [BX].CHead
            MOV   T_DRV_HEADS, AL
            MOV   AX, [BX].CSECT_TRACK
            MOV   T_DRV_SECT_TRACK, AL

;**NOW, CHECK SOURCE, TARGET DEVICE COMPATIBILITY
            MOV   DX, FINE              ;GUESS, ALL WILL BE OK
                                        ; DX MAY BE CHANGED TO REFLECT ERROR
            CMP   DS_deviceType, DRV_720 ;0 - 48 TPI, 5.25", 96 TPI,
                                        ; 5.25", 2 - 720kb, 3.5"
;           $IF   E                     ;WILL ONLY ALLOW DISKCOPY BETWEEN       ;AC000;
            JNE $$IF36
                                        ; 720KB, 3.5 SOURCE, TARGET

                CMP   DT_deviceType, DRV_720 ;target = 720KB also?
;               $IF   NE                ;                                       ;AC000;
                JE $$IF37
                  CMP   DT_deviceType, DRV_OTHER ;target might be 1.44M ;C02
;                 $IF   NE                                              ;C02
                  JE $$IF38
                    MOV   DX, OFFSET MSGNUM_NOT_COMPATIBLE ;AC000;
;                 $ENDIF                                                ;C02
$$IF38:
;               $ENDIF                  ;                                       ;AC000;
$$IF37:
;           $ELSE                       ;SINCE SOURCE NOT 720                   ;AC000;
            JMP SHORT $$EN36
$$IF36:
                CMP   DT_deviceType, DRV_720 ;SOURCE IS NOT 720kb,
                                        ; IS TARGET 720?
;               $IF   E                 ;IF SO, THEN                            ;AC000;
                JNE $$IF42
                                        ;DDT IS NOT COMPATIBLE
                  CMP   DS_deviceType, DRV_OTHER ;source might be 1.44M ;C02
;                 $IF   NE                                              ;C02
                  JE $$IF43
                    MOV   DX, OFFSET MSGNUM_NOT_COMPATIBLE ;                    ;AC000;
;                 $ENDIF                                                ;C02
$$IF43:
;               $ENDIF                  ;                                       ;AC000;
$$IF42:
;           $ENDIF                      ;                                       ;AC000;
$$EN36:
;       $ELSE                           ;SINCE SOURCE IS FIXED DISK, ERROR      ;AC000;
        JMP SHORT $$EN35
$$IF35:
            MOV   DX, OFFSET MSGNUM_INVALID_DRV ;ISSUE BAD DRV MSG              ;AC000;
;       $ENDIF                          ;                                       ;AC000;
$$EN35:
        POP   AX
        RET

DISKETTE_DRV_TYPE ENDP
        HEADER <CHECK_REDIRECTION - IS DEVICE REDIRECTED?> ;                    ;AN000;
;******************************************************************************
; SUBROUTINE NAME  : CHECK_REDIRECTION   FIND OUT IF DEVICE IS REDIRECTED     *
;                                        IF IT IS, GENERATE ERROR MSG & EXIT  *
; INPUT            : BL - DRIVE TO BE TESTED                                  *
;                  : AL : CURRENT DEFAULT DRIV                                *
;                                                                             *
; OUTPUT           : DX                = LOCAL_DRV  (-1)                      *
;                                      = DIRECTED   ( ERROR MSG OFFSET)       *
;                                      = INVALID_DRIVE (ERROR MSG OFFSET)     *
;******************************************************************************
CHECK_REDIRECTION PROC NEAR

        PUSH  AX                        ;SAVE REGISTERS
        PUSH  BX
        PUSH  CX

        MOV   CX,DX                     ;SAVE RET TEMPORARILY
        MOV   AH,IOCTL_FUNC             ;GET IOCTL FUNTION &
        MOV   AL,REDIRECTED_FUNC        ;IOCTL SUB-FUNCTION ******CHECK***

        INT   21H                       ;AND GO FIND OUT IF IT'S LOCAL
;       $IF   C
        JNC $$IF49
            MOV   CX,OFFSET MSGNUM_INVALID_DRV ;REDIR INVALID                   ;AC000;

;       $ELSE
        JMP SHORT $$EN49
$$IF49:
            TEST  DX,REMOTE_DRV         ;IF DRIVE IS REDIRECTED
;           $IF   NZ
            JZ $$IF51

                MOV   CX,OFFSET MSGNUM_DRV_REDIRECTED ;                         ;AC000;
;           $ENDIF
$$IF51:
;       $ENDIF
$$EN49:
        MOV   DX,CX                     ;GET ERROR MSG @

        POP   CX                        ;RESTORE REGISTERS
        POP   BX
        POP   AX
        RET                             ;RETURN TO CALLER
CHECK_REDIRECTION ENDP
        HEADER <BUFFER_SIZE - FINDS START AND END OF BUFFER> ;                  ;AN000;
;******************************************************************************
; SUBROUTINE NAME :  BUFFER_SIZE    DETERMINES WHERE BUFFER STARTS & ENDS     *
; INPUT           :  NONE                                                     *
;                                                                             *
; OUTPUT          :  BUFFER_BEGIN ADDRESS                                     *
;                 :  BUFFER_END   ADDRESS                                     *
;******************************************************************************
BUFFER_SIZE PROC NEAR


        PUSH  AX                        ;SAVE REGISTERS
        PUSH  BX
        PUSH  CX
        MOV   BX, offset init           ;GET ADDR OF INIT + 1024 AS
                                        ; A START OF BUFFER
        add   bx, 1024                  ;(OFFSET FROM CS, IN BYTES)
        MOV   CL,4                      ;CONVERT OFFSET INTO SEGMT BY DIVIDING
        SHR   BX,CL                     ;IT BY 16

        MOV   AX,CS                     ;CS + OFFSET => INIT+1024@ IN SEGMENT
        ADD   BX,AX                     ;WHERE BUFFER CAN START

                                        ;NEED TO START AT A NEW SECTOR ==>
        AND   BL,CLEAR_SEGMENT          ;TRUNCATE TO PREVIOUS 512 BYTE BOUNDRY
                                        ;(GET PREVIOUS SECTOR NUMBER)
        ADD   BX,20H                    ;THEN, ADVANCE TO THE BEGINNING OF
                                        ;NEXT SECTOR (SINCE PART OF PREVIOUS
                                        ;SECTOR WAS USED)

        MOV   BUFFER_BEGIN,BX           ;SAVE OUR BUFFER START SEGMENT ADDR
                                        ;(AT THE BEGINNING OF A SECTOR WITH
                                        ;SEGMENT BITS CLEARED)

        MOV   BX,DS:TWO                 ;GET ADDR WHERE BUFFER ENDS
        MOV   BUFFER_END,BX             ;(TOP OF MEMORY, OFFSET 2 IN PSP)

        POP   CX                        ;RESTORE REGISTERS
        POP   BX
        POP   AX
        RET                             ;RETURN TO CALLER
BUFFER_SIZE ENDP
        HEADER <SETUP_CTRL_BREAK - SETUP THE CTRL-BREAK VECTOR> ;               ;AN000;
;******************************************************************************
SETUP_CTRL_BREAK PROC NEAR              ;SETUP CTRL-BREAK VECTOR
;******************************************************************************
        PUSH  AX
        PUSH  BX
        PUSH  DX
        PUSH  ES

        MOV   AX,SET_CTL_BREAK_VECT     ;SET THE CTRL-BREAK VECTOR
        MOV   DX,OFFSET MAIN_EXIT
        INT   21H

        POP   ES
        POP   DX
        POP   BX
        POP   AX
        RET

SETUP_CTRL_BREAK ENDP
        HEADER <CHECK_SERVER - IS SERVER OR REDIRECTOR LOADED?> ;               ;AN000;
;******************************************************************************
CHECK_SERVER PROC NEAR                  ;SEE IF SERVER OR REDIRECTOR IS IN++
;
; INPUT: BL = DRIVE NUMBER (1=A,2=B ETC....)
;******************************************************************************
        MOV   AH,0                      ;SEE IF SERVER LOADED
        INT   SERVER
        CMP   AH,0
;       $IF   E
        JNE $$IF54
            MOV   DX,FINE
;       $ELSE
        JMP SHORT $$EN54
$$IF54:
            DEC   BL
            ADD   BL,"A"                ;CONVERT TO ASCII DRIVE LETTER
            MOV   ASCII_DRIVE_LETTER,BL ;PUT IN ASCIIZ STRING
            MOV   SI,OFFSET ASCII_DRIVE_LETTER
            MOV   AH,SHARED
            CLC
            INT   SERVER
;           $IF   C
            JNC $$IF56
                MOV   DX,OFFSET MSGNUM_DRV_REDIRECTED ;                         ;AC000;
;           $ELSE
            JMP SHORT $$EN56
$$IF56:
                MOV   DX,FINE
;           $ENDIF
$$EN56:
;       $ENDIF
$$EN54:
        RET
CHECK_SERVER ENDP

COPYINIT_END LABEL NEAR
        PUBLIC COPYINIT_END

        PATHLABL COPYINIT               ;AN015;

CSEG    ENDS
        END

