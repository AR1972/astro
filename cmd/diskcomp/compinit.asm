        PAGE    90,132                  ;A2
        TITLE   COMPINIT -- DISKCOMP INITIALIZATION PROGRAM
;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */
;****************** START OF SPECIFICATIONS *****************************
; MODULE NAME: COMPINIT

; DESCRIPTIVE NAME: Initialization for Diskette to diskette copy Utility

;FUNCTION: DISKCOMP is to compare the contents of the diskette in the
;          specified source drive to the diskette in the target
;          drive.  If necessary for the diskettes to use volume serial
;          numbers, the actual value of those number is ignored.

;          Multiple compares may be performed with one load of DISKCOMP.
;          A prompt, "Compare another (Y/N)?" permits additional
;          executions, all with the same drive specifications.

; ENTRY POINT: "DISKCOMP" at ORG 100h, jumps to "BEGIN".

; INPUT: (DOS command line parameters)

;             [d:][path] DISKCOMP  [d: [d:]] [/1] [/8]

;        WHERE
;             [d:][path] - Path where the DISKCOMP command resides.

;             [d:] - To specify the Source drive
;
;             [d:] - To specify the Target drive
;
;             [/1] - To compare only the first side of the diskette,
;                    regardless of the diskette or drive type.

;             [/8] - To compare only the first 8 sectors per track,
;                    even if the first diskette contains 9/15 sectors
;                    per track.
;
; EXIT-NORMAL: Errorlevel = 0
;             Function completed successfully.

; EXIT-ERROR: Errorlevel = 1
;             Abnormal termination due to error, wrong DOS,
;             invalid parameters, unrecoverable I/O errors on
;             the diskette.

; EFFECTS: The entire source diskette is compared, including the unused
;          sectors.  There is no awareness of the separate files
;          involved.  A unique volume serial number, if present,
;          is ignored in the comparison process.

; INCLUDED FILES:
;          PATHMAC.INC - PATHGEN MACRO
;          INCLUDE DCMPMACR.INC            ;(FORMERLY CALLED MACRO.DEF)

; INTERNAL REFERENCES:
;    ROUTINES:
;         INIT - initialization main routine
;         SOURCE_TARGET_DRV - convert source/target drive to bios values
;         TEST_DRIVE_VALIDITY - are source/target drives valid?
;         DOS_DRIVE_VALIDITY -- check dos drive validity byte
;         TEST_REMOVABLE - is specified drive removable?
;         CHK_SINGLE_DRIV_OP - is target drive same as source?
;         GET_LOGICAL_DRIVE - get logical drive who owns the physical drive
;         DISKETTE_DRV_TYPE - check compatability source/target drives
;         CHECK_REDIRECTION - is device redirected?
;         BUFFER_SIZE - finds start and end of buffer
;         SETUP_CTRL_BREAK - setup the ctrl-break vector
;         CHECK_SERVER - is server or redirector loaded?

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
;        DCOMPSM.SAL - Defines the control blocks that describe the messages
;        DCOMPPAR.SAL - Defines the control blocks that describe the
;               DOS Command line parameters.

; NOTES:
;        This module should be processed with the SALUT preprocessor
;        with the re-alignment not requested, as:

;               SALUT COMPINIT,NUL

;        To assemble these modules, the alphabetical or sequential
;        ordering of segments may be used.

;        For instructions as to how to LINK, see prolog for DISKCOMP.

;PROGRAM AUTHOR: Original written by: Jin K.
;                4.00 modifications by: Edwin M. K.
;       ;C02    MKS     Bug#727 against DISKCOMP wasn't allowing two 720k
;                       diskettes to DISKCOMP or DISKCOPY if they were in
;                       drives of differing capacities.  This is fixed by
;                       checking for drive type 7 as well as 2, and permitting
;                       the copy if both diskettes are 720k.  The equate
;                       DRV_OTHER comes from DISKCOMP.EQU.
;       ;C05    MKS     Bug#1061.  Removeable hard disks were causing problems.
;                       Now they are disallowed.  FixedDisk comes from
;                       DISKCOMP.EQU.
;****************** END OF SPECIFICATIONS *****************************

;REVISION HISTORY:
;DATE:   10-30-84 - chk_para routine added. Many parts are modified to
;        permit DISKCOMP /1, DISKCOMP D: /1 cases. Restore diskbase
;        before return to DOS when invalid DOS version occurs.
;DATE:   3-27-85  MAIN PARTS OF DISKCOMP PROGRAM HAS BEEN REWRITTEN
;        TO USE NEW IOCTL FUNCTIONS 'READ', 'GET_DEVICE_PARAMETERS'.
;       A000 - Change spelling of "LOCAL" to "LOCALX" to make MASM 3 happy.

        INCLUDE PATHMAC.INC             ;AN013;
        INCLUDE DCMPMACR.INC            ;(FORMERLY CALLED MACRO.DEF)

CSEG    SEGMENT PARA PUBLIC 'CODE'      ;                                            ;AC000;
        ASSUME  CS:CSEG, DS:CSEG, ES:CSEG, SS:CSEG

        INCLUDE DISKCOMP.EQU
        include version.inc             ; MSKK02 07/14/89

NOOP    EQU     90H                     ;NO-OPERTION INSTRUCTION, USED TO DELETE     ;AN001;
                                        ; 386 SUPPORT                           ;AN001;
;$salut (4,2,9,41)
;****************************************************************************
;                                                                           *
;                        EXTERNAL VARIABLES                                 *
;                                                                           *
;****************************************************************************
 EXTRN  PARSER:NEAR                     ;DCOPYPAR.SAL - DRIVES SYS PARSER       ;AN000;

 EXTRN  RECOMMENDED_BYTES_SECTOR:WORD
 EXTRN  S_OWNER_SAVED:BYTE
 EXTRN  T_OWNER_SAVED:BYTE
 EXTRN  ASCII_DRV1_ID:BYTE              ;40H    SOURCE DRIVE ID IN ASCII
 EXTRN  ASCII_DRV2_ID:BYTE              ;40H    TARGET DRIVE ID IN ASCII

 EXTRN  SUBLIST_78          :WORD       ;                                       ;AN000;
 EXTRN  SUBLIST_11          :WORD       ;                                       ;AN000;
 EXTRN  SUBLIST_15A         :WORD       ;                                       ;AN000;
 EXTRN  SUBLIST_15B         :WORD       ;                                       ;AN000;
 EXTRN  SUBLIST_15C         :WORD       ;                                       ;AN000;
 EXTRN  SUBLIST_17B         :WORD       ;                                       ;AN000;
 EXTRN  SUBLIST_17C         :WORD       ;                                       ;AN000;
 EXTRN  SUBLIST_17D         :WORD       ;                                       ;AN000;
 EXTRN  SUBLIST_PARSE:WORD              ;PARSE ERROR XX - %0                         ;AN004;

 EXTRN  MSGNUM_INVALID_PARM:BYTE        ;"INVALID PARAMETER"                    ;AC000;
 EXTRN  MSGNUM_INVALID_DRV:BYTE         ;"INVALID DRIVE SPECIFICATION"          ;AC000;
 EXTRN  MSGNUM_DRV_REDIRECTED:BYTE      ;"INVALID, DRIVE REDIRECTED"            ;AC000;
 EXTRN  MSGNUM_NOT_COMPATIBLE:BYTE      ;"DEVICE OR DISKETTE TYPES NOT COMPATIBLE";AC000;
.XLIST
;EXTRN  MSG_INVALID_DOS :BYTE           ;MSG FOR DOS1.0 AND 1.1
.LIST

 EXTRN  TRACK_TO_READ :WORD
 EXTRN  SIDE:BYTE

 EXTRN  S_DRV_SECT_TRACK :BYTE          ;SECT/TRACK
 EXTRN  S_DRV_HEADS :BYTE               ;# OF HEADS
 EXTRN  S_DRV_TRACKS :WORD              ;# OF TRACKS
 EXTRN  T_DRV_SECT_TRACK :BYTE
 EXTRN  T_DRV_HEADS :BYTE
 EXTRN  T_DRV_TRACKS :WORD

 EXTRN  COPY_TYPE :BYTE                 ;1 = 1-DRIVE COPY  2 = 2-DRIVE COPY
 EXTRN  USER_OPTION :BYTE               ;NO OPTION (-1)  /1 (1), INVALID (9)
 EXTRN  USER_OPTION_8 :BYTE             ;NO OPTION (-1)  /8 (1), INVALID (9)
 EXTRN  BUFFER_BEGIN :WORD              ;STARTING BUFFER @ FOR LOADING
 EXTRN  BUFFER_END :WORD                ;ENDING BUFFER @ FOR LOADING
 EXTRN  START_BUFFER :WORD              ;START OF BUFFER SPACE
 EXTRN  MAIN_EXIT :WORD                 ;EXIT ADDRESS FOR CONTROL-BREAK

 EXTRN  ORG_SOURCE_DRIVE:BYTE           ;LOGICAL SOURCE DRIVE NUMBER
 EXTRN  ORG_TARGET_DRIVE:BYTE           ;        TARGET
 EXTRN  SOURCE_DRIVE:BYTE               ;AS SPECIFIED BY USER PARMS, DR NUM     ;AN000;
 EXTRN  TARGET_DRIVE:BYTE               ;AS SPECIFIED BY USER PARMS, DR NUM     ;AN000;

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

 EXTRN  PATCH_386:BYTE                  ;PATCH AREA, CHANGED TO NOOP IF NOT 386 ;AN001;

 EXTRN  Q_BREAK : BYTE                  ; dcomppar.asm

 EXTRN  GENERIC_IOCTL :NEAR
 EXTRN  SET_LOGICAL_DRIVE :NEAR
;                      $salut (4,24,28,41)
MY_BPB                 STRUC
CBYTE_SECT             DW  0            ; 200H  BYTES / SECTOR
CSECT_CLUSTER          DB  0            ; 2h    SECTORS / CLUSTER
CRESEV_SECT            DW  0            ; 1h    RESERVED SECTORS
CFAT                   DB  0            ; 2h    # OF FATS
CROOTENTRY             DW  0            ; 70h   # OF ROOT ENTRIES
CTOTSECT               DW  0            ; 02D0h TOT. # OF SECT.
                                        ; INCL BOOT SECT, DIRS
MEDIA_DESCRIP          DB  0            ;0FDh   MEDIA DISCRIPTOR
CSECT_FAT              DW  0            ; 2h    SECTORS / FAT
CSECT_TRACK            DW  0            ;
CHEAD                  DW  0            ;
CHIDDEN_SECT           DD  0            ;
BIG_TOT_SECT           DD  0            ;
                       DB  6 DUP (0)    ;
MY_BPB                 ENDS

;USED TO CHECK FOR PRESENCE OF 386 MACHINE:
BIOS_SYSTEM_DESCRIPTOR struc            ;SYSTEM TYPE STRUC                      ;AN001;
bios_SD_leng           dw  ?            ;VECTOR LENGTH                          ;AN001;
bios_SD_modelbyte      db  ?            ;SYSTEM MODEL TYPE                      ;AN001;
bios_SD_scnd_modelbyte db  ?            ;                                       ;AN001;
                       db  ?            ;                                       ;AN001;
bios_SD_featurebyte1   db  ?            ;                                       ;AN001;
                       db  4 dup (?)    ;                                       ;AN001;
BIOS_SYSTEM_DESCRIPTOR ends             ;END OF STRUC                           ;AN001;


;****************************************************************************
;                                                                           *
;                        VARIABLE DECLARATIONS                              *
;                                                                           *
;****************************************************************************
DRIVE_VALID            DW  ?            ;DRIVE VALIDITY BYTE
DEFAULT_DRV            DB  ?            ;DEFAULT DRIVE ID
NUMBER_OF_DRV          DB  ?            ;TOTAL # OF DISKT DRIVES ON THE SYS
                                        ;(NUMBER_OF_DRV = 0 ---> 1 DRIVE)
ASCII_DRIVE_LETTER     DB  " :",0
                       PATHLABL COMPINIT ;AN013;
                       HEADER <INIT - INITIALIZATION ROUTINE, MAIN PROGRAM> ;       ;AN000;
;       $salut (4,9,15,41)              ;                                       ;AN000;
;#############################################################################
;                     INITIALIZATION ROUTINE - MAIN PROGRAM
INIT    PROC  NEAR
        PUBLIC INIT                     ;MAKE ENTRY IN LINK MAP                 ;AN000;

;OUTPUT: DX = EXIT CODE, "FINE"
;#############################################################################

        MOV   DRIVE_VALID,AX            ;SAVE DRIVE VALIDITY BYTE
        CALL  PC_386_CHK                ;SEE IF THIS IS A 386 MACHINE           ;AN001;

;               REPLACE THE "FILL_SEG" IN THE SUBLIST MESSAGE CONTROL BLOCKS.

;               BECAUSE THIS IS A .COM STYLE FILE, THESE SEGID VALUES CANNOT
;               BE PROVIDED BY THE DOS SYSTEM LOADER, BUT MUST BE DYNAMICALLY
;               PERFORMED AT EXECUTION TIME AS PART OF A .COM FILE'S OBLIGATION
;               TO BE "SELF-RELOCATING".

        MOV   AX,CS                     ;GET SEGID OF COMMON SEGMENT
        MOV   SUBLIST_78.SUB_VALUE_SEG,AX ;                                     ;AN000;
        MOV   SUBLIST_11.SUB_VALUE_SEG,AX ;                                     ;AN000;
        MOV   SUBLIST_15A.SUB_VALUE_SEG,AX ;                                    ;AN000;
        MOV   SUBLIST_15B.SUB_VALUE_SEG,AX ;                                    ;AN000;
        MOV   SUBLIST_15C.SUB_VALUE_SEG,AX ;                                    ;AN000;
        MOV   SUBLIST_17B.SUB_VALUE_SEG,AX ;                                    ;AN000;
        MOV   SUBLIST_17C.SUB_VALUE_SEG,AX ;                                    ;AN000;
        MOV   SUBLIST_17D.SUB_VALUE_SEG,AX ;                                    ;AN000;
        MOV   SUBLIST_PARSE.SUB_VALUE_SEG,AX ;                                  ;AN004;

        CALL  SETUP_CTRL_BREAK          ;STEALS CTRL_BREAK
        CLD                             ;CLEAR DIRECTION FLAG
        MOV   DX,FINE                   ;ASSUME EVERYTHING IS FINE
.XLIST
;       CALL  SCREENING                 ;CHECK DOS VERSION AND INPUT PARMS
;       CMP   DX,FINE                   ;IF FINE & DANDY
;       JNE   EXIT_INIT
.LIST
        CALL  PARSER                    ;LOOK AT DOS COMMAND LINE               ;AN000;
        CMP   [Q_BREAK], 0              ; exit after options msg?
        JNE   INIT_EXIT                 ;  jump if so

        CMP   DX,FINE                   ;IF ALL OK                              ;AN000;
        JNE $$IF1
	CALL  SOURCE_TARGET_DRV     ;DETERMINE SOURCE AND TARGET DRV

$$IF1:
        CMP   DX,FINE                   ;IF STILL FINE AND DANDY                ;AN000;
	JNE   INIT_EXIT
	CALL  TEST_DRIVE_VALIDITY

	CMP   DX,FINE
	JNE   INIT_EXIT
	CALL  DISKETTE_DRV_TYPE ;SOURCE & TARGET DRIVE TYPES

	CMP   DX,FINE		;IF FINE & DANDY
	JNE   INIT_EXIT
	CALL  BUFFER_SIZE   ;GET BUFFER SIZE FOR COPYING

INIT_EXIT:
        RET                             ;RETURN TO CALLER

INIT    ENDP                            ;END INITIALLIZATION PROGRAM

;#############################################################################
.XLIST
;       HEADER <SCREENING - CHECK DOS VERSION, SYNTAX PARMS>
;******************************************************************************
; SUBROUTINE NAME :  SCREENING  - CHECKS THE FOLLOWING:                       *
;                                 - DOS VERSION                               *
;                                 - DRIVE ID VALIDITY                         *
;                                 - FILE NAME ENTERED BY MISTAKE?             *
; INPUT           :  BL : DRIVE VALIDITY BYTE                                 *
; OUTPUT          :  DX : FINE    - NO ERROR                                  *
;                         (OTHERS)- ERROR MSG OFFSET                          *
;******************************************************************************
;SCREENING PROC NEAR

                                        ;CHECK DOS VERSION:
;        MOV   AH,DOSVER_FUNC            ;SEE IF CORRECT DOS VERSION
;        INT   21H                       ;FUNCTION CALL (AL <- DOS VERSION)
;;        XCHG    AH,AL                   ;AH=MAJOR VER, AL=MINOR VER
;        CMP   AX,expected_version       ;IF DOS MAJOR VERSION LESS THAN 2.0
;        $IF   NE                        ;THEN ISSUE ERROR MSG
;            MOV   DX,OFFSET MSG_INVALID_DOS
;            MOV   AH,PRINT_FUNC         ;USE PRINT FUNCTION TO TELL USER
;            INT   21H                   ;THAT HE IS USING THE OLD VERSION
;            INT   20H
;        $ELSE                           ;VERSION OK
;            CALL  CHK_PARA              ;GENERAL SYNTAX CHECK
;        $ENDIF                          ;END VERSION TEST
;        RET
;SCREENING ENDP
;        HEADER <CHK_PARA - SYNTAX PARMS, OPTION /1 /8>
;;**************************************************************************
;CHK_PARA PROC NEAR
;;       CHECK SYNTAX OF THE ENTERED PARAMETERS                            *
;;       ALSO, DETERMINE THE USER OPTION "/1" AND/OR "/8" IS ENTERED.      *
;;  INPUT: DX = FINE                                                       *
;;         IF /1 HAS BEEN ENTERED, THE VARIABLE USER_OPTION = OPTION_1     *
;;         ELSE USER_OPTION = NO_OPTION.                                   *
;;         IF /8 HAS BEEN ENTERED, THE USER_OPTION_8 WILL BE ON            *
;;  OUTPUT: DX = FINE    - NO ERROR                                        *
;;          OTHERWISE DX POINTS TO ERROR MSG                               *
;;**************************************************************************
;        PUSH  CX
;        MOV   USER_OPTION, NO_OPTION    ;ASSUME NO /1 IS ENTERED.
;        MOV   USER_OPTION_8, OFF        ;ASSUME /8 IS NOT ENTERED.
;        XOR   CX, CX
;        MOV   CL, BYTE PTR DS:BEGIN_UNFORM_AREA ;GET # OF CHR
;        CMP   CL, 0
;        JZ    CHK_PARA_EXIT
;        CLD                             ;CLEAR DIRECTION
;        MOV   DI, BEGIN_UNFORM_AREA+2   ;STARTING POINT OF PARA
;        DEC   CL                        ;TO IGNORE LAST CHR (0DH)
;        CALL  SKIP_BLANKS               ;SKIP BLANKS, IF ANY. THE POINTER WILL POINT TO THE NEXT NON_BLANK CHR
;        JZ    CHK_PARA_EXIT             ;ONLY BLANKS ARE ENTERED.
;        CALL  CHK_SLASH_ONE             ;IS NEXT WORD /1 OR /8 ?
;        JNC   SLASH_ONE                 ;YES
;        CALL  CHK_DRV_SPEC              ;IS IT A DRIVE SPECIFICATION LIKE d: ?
;        JC    INVALID_PARA              ;IF NOT, THEN ERROR
;        JZ    CHK_PARA_EXIT             ;NO MORE CHR? THEN, OK. (EX. DISKCOMP D:)
;        CALL  CHK_SLASH_ONE             ;IS NEXT WORD /1 OR /8 ?
;        JNC   SLASH_ONE                 ;YES.(EX. DISKCOMP D:/1)
;        CALL  CHK_BLANK                 ;IF NOT, NEXT CHR SHOULD BE A BLANK.
;        JC    INVALID_PARA              ;OTHERWISE, ERROR.
;        CALL  SKIP_BLANKS               ;SKIP BLANKS, IF ANY.
;        JZ    CHK_PARA_EXIT             ;(EX. DISKCOMP D:  )
;        CALL  CHK_SLASH_ONE             ;IS IT /1 OR /8 ?
;        JNC   SLASH_ONE                 ;YES. (EX. DISKCOMP D:  /1)
;        CALL  CHK_DRV_SPEC              ;IF NOT /1 OR /8, THEN IS IT A DRV SPEC?
;        JC    INVALID_PARA              ;OTHERWISE, ERROR.
;        CALL  SKIP_BLANKS               ;SKIP BLANKS, IF ANY.
;        JZ    CHK_PARA_EXIT             ;NO MORE CHR. (EX. DISKCOMP D: D:)
;        CALL  CHK_SLASH_ONE             ;OTHERWISE, /1 AND/OR /8 SHOULD BE FOLLOWED.
;        JNC   SLASH_ONE                 ;YES, /1 OR /8. JMP TO SLASH_ONE
;        JMP   INVALID_PARA              ;PARAMETER ERROR.
;SLASH_ONE:                              ;YES, FOUND EITHER OF /1 OR /8.
;        CALL  SKIP_BLANKS               ;/1 SHOULD BE END OF PARAMETERS, OR ONLY BLANKS CAN FOLLOW.
;        JZ    CHK_PARA_EXIT             ;NO MORE CHR? THEN OK.
;        CMP   USER_OPTION, OPTION_1     ;WAS IT /1?
;        JZ    SLASH_8                   ;THEN CHECK WHETHER NEXT IS /8.
;        CALL  CHK_SLASH_ONE             ;OTHERWISE, IT WAS /8. NOW CHECK /1.
;        JC    INVALID_PARA              ;NOT FOUND, ERROR
;        CMP   USER_OPTION, OPTION_1
;        JZ    CHK_PRE_EXIT              ;YES, IT IS /1
;        JMP   INVALID_PARA              ;OTHERWISE, FOUND /8 AGAIN. ERROR
;SLASH_8:
;        CALL  CHK_SLASH_ONE             ;CHECK IT IS /8
;        JC    INVALID_PARA              ;NOT FOUND? ERROR
;        CMP   USER_OPTION_8, ON
;        JZ    CHK_PRE_EXIT              ;YES. IT IS /8
;        JMP   INVALID_PARA              ;OTHERWISE, FOUND /1 AGAIN. ERROR
;CHK_PRE_EXIT:
;        CALL  SKIP_BLANKS               ;SKIP BLANKS IF ANY.
;        JZ    CHK_PARA_EXIT             ;THERE SHOULD NOT BE ANY MORE PARAMETER.
;INVALID_PARA:
;        MOV   DX,OFFSET MSG_INVALID_PARM_PTR ;WRONG PARM ENTERED MSG
;CHK_PARA_EXIT:
;        POP   CX
;
;        RET
;CHK_PARA ENDP
;        HEADER <SKIP_BLANKS - IGNORE BLANKS/TABS IN PARMS PARSING>
;***************************************************************************
;SKIP_BLANKS PROC NEAR
; ** SKIP BLANKS OR TABS, IF ANY, IN THE PARAMETER STRING.                 *
; INPUT: ES:DI POINTS TO THE CURRENT CHR.                                  *
;        CX - # OF REMAINING CHR IN THE STRING.                            *
; OUTPUT: ES:DI POINT TO THE NEXT NON_BLANK CHR.                           *
;         CX IS ADJUSTED ACCORINGLY.                                       *
;         IF THE CURRENT CHR IS NOT A BLANK, THEN DI, CX VALUE NOT CHANGED.*
;         IF CX = 0, THEN ZERO FLAG WILL BE SET AND EXIT THIS PROC.        *
;***************************************************************************
;SKIP_AGAIN:
;        MOV   AL, 20H                   ;20H=BLANK
;        CLD                             ;CLEAR DIRECTION
;        REPE  SCASB
;        JZ    SK_BL_1                   ;IF NOT FOUND A NON_BLANK CHR YET, AND CX=0, EXIT THIS ROUTINE.
;        DEC   DI                        ;OTHERWISE, RESTORE DI TO THE NON_BLANK POSITION.
;        INC   CX                        ;  AND RESTORE CX TO WHERE IT WAS AT NON_BLANK CHR
;                                        ;(IF FOUND A NON_BLANK CHR, ZERO FLAG WOULD NOT BE SET)
;        MOV   AL, ES:BYTE PTR [DI]
;        CMP   AL, 09H                   ;09H=TAB
;        JNZ   SK_BL_1                   ;IF THE NON_BLANK CHR IS NOT A TAB THEN EXIT
;        INC   DI                        ;ELSE TRY SKIP AGAIN.
;        DEC   CX
;        JMP   SKIP_AGAIN
;SK_BL_1:
;        RET
;SKIP_BLANKS ENDP
;        HEADER <CHK_SLASH - IS CURRENT PARM /1 OR /8>
;;***************************************************************************
;CHK_SLASH_ONE PROC NEAR
; ** CHECK CURRENT CHR IS / FOLLOWED BY 1.                                 *
; INPUT: ES:DI POINTS TO THE CURRENT CHR TO BE CHECKED.                    *
;        CX REPRESENTS THE # OF CHR'S IN THE STRING.                       *
; OUTPUT: FOUND - DI POINTS TO THE NEXT CHR.  CX CHANGED ACCORDINGLY.      *
;                 IF THIS HAD BEEN A LAST WORD, ZERO FLAG WILL BE SET.     *
;         NOT FOUND - CARRY IS SET. DI, CX UNCHANGED.                      *
;***************************************************************************

;        CLC                             ;CLEAR CARRY FLAG
;        CMP   CX, 2                     ;# OF CHR IN THE STRING.
;        JL    CK_SL_0                   ;IF LESS THAN 2, THEN SET CARRY AND EXIT.
;        MOV   AX, ES:WORD PTR [DI]      ;GET CURRENT WORD IN AX
;        CMP   AX, '1/'                  ;IS IT /1 ?
;        JZ    CK_SL_1                   ;YES. SET USER_OPTION
;        CMP   AX, '8/'                  ;IS IT /8 THEN ?
;        JZ    CK_SL_2                   ;YES. SET USER_OPTION_8
;CK_SL_0:
;        STC                             ;OTHERWISE, NOT FOUND. SET CARRY
;        JMP   CK_SL_4                   ; AND RETURN
;CK_SL_1:
;        MOV   USER_OPTION, OPTION_1
;        JMP   CK_SL_3
;CK_SL_2:
;        MOV   USER_OPTION_8, ON
;CK_SL_3:                                ;ADJUST CX, DI TO THE NEXT CHR.
;        INC   DI
;        INC   DI
;        DEC   CX
;        DEC   CX
;        CMP   CX, 0                     ;SET ZERO FLAG IF NO MORE CHR.
;CK_SL_4:
;        RET
;CHK_SLASH_ONE ENDP
;        HEADER <CHK_DRV - CURRENT PARM CHAR IS DRIVE AND COLON?>
;;***************************************************************************
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
;        JL    CK_DR_1                   ;IF LESS THAN 2, THEN NOT FOUND - SET CARRY AND EXIT.
;        MOV   AL, ES:BYTE PTR [DI]      ;GET CURRENT CHR
;        AND   AL, 11011111B             ;CHANGE IT TO UPPER_CASE CHR.
;        CMP   AL, 'A'
;        JB    CK_DR_1                   ;LESS THAN 'A', THEN NOT FOUND.
;        CMP   AL, 'Z'
;        JA    CK_DR_1                   ;ABOVE 'Z', THEN NOT FOUND.
;        MOV   AL, ES:BYTE PTR [DI+1]    ;LOOK AHEAD THE FOLLOWING CHR.
;        CMP   AL, ':'                   ;SHOULD BE A COLON.
;        JNZ   CK_DR_1                   ;NOT FOUND.
;        INC   DI                        ;FOUND. ADJUST CX, DI TO THE NEXT CHR.
;        INC   DI
;        DEC   CX
;        DEC   CX
;        CMP   CX, 0                     ;IF NO MORE CHR, THAN SET THE ZERO FLAG.
;        JMP   CK_DR_2
;CK_DR_1:
;        STC                             ;SET CARRY
;CK_DR_2:
;        RET
;CHK_DRV_SPEC ENDP
;        HEADER <CHK_BLANK - IS CURRENT CHAR IN PARM BLANK OR TAB>
;;***************************************************************************
;CHK_BLANK PROC NEAR
;; ** CHECK THE CURRENT CHR IS A BLANK OR A TAB.                            *
;; INPUT: ES:DI POINTS TO THE CURRENT CHR.                                  *
;;        CX - # OF CHR IN THE STRING.                                      *
;; OUTPUT: FOUND - DI MOVES TO THE NEXT CHR. CX DECREASES BY 1.             *
;;         NOT FOUND - CARRY IS SET. DI, CX UNCHANGED.                      *
;;***************************************************************************
;
;        CLC                             ;CLEAR CARRY
;        CMP   CX, 1                     ;IF LESS THAN 1, NOT FOUND.
;        JL    CK_BL_0                   ;SET CARRY AND EXIT
;        MOV   AL, ES:BYTE PTR [DI]      ;GET CURRENT CHR
;        CMP   AL, 020H                  ;020H=BLANK CHR
;        JZ    CK_BL_1                   ;FOUND
;        CMP   AL, 09H                   ;09H=TAB CHR
;        JZ    CK_BL_1                   ;FOUND
;CK_BL_0:
;        STC                             ;NOT FOUND. SET CARRY
;        JMP   CK_BL_2
;CK_BL_1:
;        INC   DI                        ;FOUND. ADJUST DI, CX
;        DEC   CX
;CK_BL_2:
;        RET
;CHK_BLANK ENDP
.LIST
        HEADER <PC_386_CHK - SEE IF THIS IS A 386 MACHINE>
; QUERIES THE BIOS TO DETERMINE WHAT TYPE OF
; MACHINE WE ARE ON.  WE ARE LOOKING FOR A 386.
; THIS WILL BE USED TO DETERMINE IF A DOUBLE WORD MOVE
; IS TO BE PERFORMED.
;
;       INPUTS          : NONE
;
;       OUTPUTS         : IF A 386 NOT PRESENT, CODE IS
;                            *** P A T C H E D ***
;                         TO NO-OP THE SUPPORT FOR THE DOUBLE WORD MOVE
;=========================================================================

PC_386_CHK PROC NEAR			;DETERMINE MACHINE TYPE

	PUSH  AX			;SAVE AFFECTED REGS
	PUSH  BX			;
	PUSH  ES			;

	xor	ax,ax			; determine CPU type
	push	ax
	popf				; try to put that in the flags
	pushf
	pop	ax			; look at what really went into flags
	test	ax,8000h		; was high bit set?
	jnz	not_a_80386		; jump if so - it's 8086/88
	mov	ax,7000h		; try to set the NT/IOPL bits
	push	ax
	popf				; ...in the flags
	pushf
	pop	ax			; look at actual flags
	test	ax,7000h		; any high bit set?
	jnz	its_a_80386		; jump if so - it's 80386

not_a_80386:
	MOV   AL,NOOP			;WITH A NO-OP INSTRUCTION
	MOV   PATCH_386,AL		;
	MOV   PATCH_386+1,AL		;
	MOV   PATCH_386+2,AL		;
its_a_80386:

	POP   ES			;RESTORE REGS.
	ASSUME ES:CSEG			;BACK TO USUAL

	POP   BX			;
	POP   AX			;

	RET				;

PC_386_CHK ENDP 			;
	HEADER <SOURCE_TARGET_DRV - CONVERT SRC/TARGET DR TO BIOS VALS> ;
;******************************************************************************
; SUBROUTINE NAME :  SOURCE_TARGET_DRV    DETERMINES SOURCE & TARGET DRIVES & *
;                                         CONVERT THEM FROM DOS TO BIOS VALUE *
; INPUT           :  SOURCE_DRIVE & TARGET_DRIVE HAVE DOS DRIVE ID'S:         *
;                         0 = DEFAULT        1 = DRV A   ETC.                 *
;                                                                             *
; OUTPUT          :  ORG_SOURCE_DRIVE        1 = DRIVE A    2 = DRIVE B  ETC. *
;                 :  ORG_TARGET_DRIVE        1 = DRIVE A    2 = DRIVE B  ETC. *
;                                                                             *
;                 :  COPY_TYPE      1 = SINGLE DRV COPY, 2 = 2 DRIVE COPY     *
;                 :  DX : FINE    - NO ERROR                                  *
;******************************************************************************
SOURCE_TARGET_DRV PROC NEAR
	PUBLIC SOURCE_TARGET_DRV	;MAKE ENTRY IN LINK MAP
                                        ;GET CURRENT DEFAULT DRIVE
        MOV   AH,CURRENTDRV_FUNC        ;FUNCTION CALL (19H)
                                        ;(AL <- CURRENT DEFAULT DRV
        INT   21H                       ;0 = A, 1 = B, ETC)

        MOV   DEFAULT_DRV,AL            ;SAVE IT

	CMP   SOURCE_DRIVE,ZERO 	;FIRST DRV ENTERED?
	JNE $$IF9			;NO DRIVE LETTER ENTERED
	MOV   CH, DEFAULT_DRV		;SET SOURCE, TARGET DRIVE TO
	INC   CH
	MOV   ORG_SOURCE_DRIVE, CH	;DEFAULT DRIVE
	MOV   CL, CH
	MOV   ORG_TARGET_DRIVE, CL
	JMP SHORT $$EN11

$$IF9:
	MOV   CH,SOURCE_DRIVE	    ;GET SOURCE DRIVE FROM SPECIFIED PARM
	MOV   ORG_SOURCE_DRIVE, CH
	CMP   TARGET_DRIVE,ZERO     ;WAS A SECOND DRIVE SPECIFIED
	JNE $$IF11		    ;TARGET DRIVE IS DEFAULT
	MOV   CL, DEFAULT_DRV
	INC   CL		;MAKE IT A LOGICAL DRIVE NUMBER
	MOV   ORG_TARGET_DRIVE, CL
	JMP SHORT $$EN11

$$IF11:
	MOV   CL, TARGET_DRIVE	;USE USER SPECIFIED TARGET DRIVE
	MOV   ORG_TARGET_DRIVE, CL
$$EN11:

        ADD   ASCII_DRV1_ID,CH          ;SETUP DRIVE ID ALPHABET IN THE
        ADD   ASCII_DRV2_ID,CL          ;MESSAGES

        RET
SOURCE_TARGET_DRV ENDP
	HEADER <TEST_DRIVE_VALIDITY - ARE SOURCE/TARGET DRIVES VALID?> ;
;******************************************************************************
; SUBROUTINE NAME :  TEST_DRIVE_VALIDITY--MAKE SURE SOURCE AND TARGET DRIVES  *
;                    SPECIFIED BY USER ARE VALID FOR DISKCOPY                 *
;                                                                             *
; INPUT           :  ORG_SOURCE_DRIVE:BYTE, ORG_TARGET_DRIVE:BYTE             *
;                                                                             *
; OUTPUT          :  DX=FILE IF DRIVES ARE VALID ELSE DX CONTAINS MESSAGE PTR *
;******************************************************************************

TEST_DRIVE_VALIDITY PROC NEAR
        PUBLIC TEST_DRIVE_VALIDITY      ;MAKE ENTRY IN LINK MAP                 ;AN000;

        CALL  DOS_DRIVE_VALIDITY

        CMP   DX,FINE
        JNE $$IF15

        MOV   BL,ORG_SOURCE_DRIVE
        CALL  CHECK_REDIRECTION

        CMP   DX,FINE
        JNE $$IF15

        MOV   BL,ORG_TARGET_DRIVE
        CALL  CHECK_REDIRECTION

        CMP   DX,FINE
        JNE $$IF15

        MOV   BL,ORG_SOURCE_DRIVE
        CALL  CHECK_SERVER

        CMP   DX,FINE
        JNE $$IF15

        MOV   BL,ORG_TARGET_DRIVE
        CALL  CHECK_SERVER

        CMP   DX,FINE
        JNE $$IF15

        CALL  TEST_REMOVABLE

        CMP   DX,FINE
        JNE $$IF15

	CALL  CHK_SINGLE_DRV_OP     ;CHECK IF IT IS
                                        ; ONE PHYSICAL DRIVE OPERATION
$$IF15:
        RET

TEST_DRIVE_VALIDITY ENDP
        HEADER <DOS_DRIVE_VALIDITY - CHECK DOS DRIVE VALIDITY BYTE>
;******************************************************************************
; SUBROUTINE NAME :  DOS_DRIVE_VALIDITY -- CHEKC DOS DRIVE VALIDITY BYTE      *
;                                                                             *
; INPUT           :  DRIVE_VALID:BYTE                                         *
;                    THIS IS THE ORIGINAL VALUE PRESENTED IN AX BY DOS LOADER *
;                                                                             *
; OUTPUT          :  DX=FILE IF DRIVES ARE VALID ELSE DX CONTAINS MESSAGE PTR *
;******************************************************************************

DOS_DRIVE_VALIDITY PROC NEAR
        PUBLIC DOS_DRIVE_VALIDITY       ;MAKE ENTRY IN LINK MAP                 ;AN000;

        CMP   DRIVE_VALID,0             ;SEE IF DRIVES ARE VALID DOS DEVICE
        JE $$IF17
	MOV   DX,OFFSET MSGNUM_INVALID_DRV

$$IF17:
        RET

DOS_DRIVE_VALIDITY ENDP
        HEADER <TEST_REMOVABLE - IS SPECIFIED DRIVE REMOVABLE?>
;******************************************************************************
; SUBROUTINE NAME :  TEST_REMOVABLE -- CHECK IF DRIVES SPECIFED ARE REMOVABLE *
;                                                                             *
; INPUT           :  SOURCE_DRIVE:BYTE, TARGET_DRIVE:BYTE                     *
;                                                                             *
; OUTPUT          :  DX=FILE IF DRIVES ARE VALID ELSE DX CONTAINS MESSAGE PTR *
;******************************************************************************

TEST_REMOVABLE PROC NEAR
        PUBLIC TEST_REMOVABLE           ;MAKE ENTRY IN LINK MAP                 ;AN000;

        MOV   BL,ORG_SOURCE_DRIVE       ;GET PARM 1 DRIVE ID

        MOV   AX,DRIVE_CHECK            ;CHECK FOR REMOVABLE DRIVE
        INT   21H                       ;IOCTL CALL

;       $IF   NC                        ;IF DRIVE ID IS WITHIN RANGE
        JC $$IF19
            CMP   AX,REMOVABLE          ;THEN IF SOURCE DRIVE IS FIXED
;           $IF   NE                    ;  THEN
            JE $$IF20
                MOV   DX,OFFSET MSGNUM_INVALID_DRV ;GENERATE                    ;AC000;
                                        ; HARD DRIVE ERROR MESSAGE
;           $ELSE                       ;ELSE, SRC IS REMOVABLE;
            JMP SHORT $$EN20
$$IF20:
                MOV   BL,ORG_TARGET_DRIVE ;NOW GO CHECK TARGET
                MOV   AX,DRIVE_CHECK    ;CHECK FOR REMOVABLE DRIVE
                INT   21H               ;IOCTL CALL

;               $IF   NC                ;IF DRV WITHIN RANGE
                JC $$IF22
                    CMP   AX,REMOVABLE  ;THEN TGT DRV IS FIXED
;                   $IF   NE            ;     THEN
                    JE $$IF23
                        MOV   DX,OFFSET MSGNUM_INVALID_DRV ;GENERATE            ;AC000;
                                        ; HARD DRV ERROR MSG
;                   $ENDIF              ;END TEST IF TGT DRV IS FIXED
$$IF23:
;               $ELSE                   ;TGT DRV OUT OF RANGE.  EX. DRIVE X:
                JMP SHORT $$EN22
$$IF22:
                    MOV   DX,OFFSET MSGNUM_INVALID_DRV ;GENERATE                ;AC000;
                                        ; HARD DRV ERROR MSG
;               $ENDIF                  ;END TEST IF TGT WITHIN RANGE
$$EN22:
;           $ENDIF                      ;END IF SRC IS REMOVABLE
$$EN20:
;       $ELSE                           ;ELSE,  SRC DRV OUT OF RANGE
        JMP SHORT $$EN19
$$IF19:
            MOV   DX,OFFSET MSGNUM_INVALID_DRV ;PRINT ERROR MSG                 ;AC000;
;       $ENDIF                          ;END TEST IF SRC DRV WITHIN RANGE
$$EN19:
        RET

TEST_REMOVABLE ENDP
        HEADER <CHK_SINGLE_DRIV_OP - IS TARGET DRIVE SAME AS SOURCE?>
;******************************************************************************
; SUBROUTINE NAME :  CHK_SINGLE_DRV_OP                                        *
;                                                                             *
; INPUT           :  ORG_SOURCE_DRIVE - LOGICAL DRIVE NUMBER                  *
;                    ORG_TARGET_DRIVE                                         *
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
        PUBLIC CHK_SINGLE_DRV_OP        ;MAKE ENTRY IN LINK MAP                 ;AN000;

        PUSH  AX

        MOV   BL,ORG_SOURCE_DRIVE
        CALL  GET_LOGICAL_DRIVE

        MOV   S_OWNER_SAVED, AL         ;SAVE CURRENT OWNER DRIVE LETTER.
        MOV   BL, ORG_TARGET_DRIVE
        CALL  GET_LOGICAL_DRIVE

        MOV   T_OWNER_SAVED, AL         ;SAVE CURRENT OWNER
        MOV   BL, ORG_SOURCE_DRIVE
        CALL  SET_LOGICAL_DRIVE

        MOV   BL, ORG_TARGET_DRIVE
        CALL  SET_LOGICAL_DRIVE

        MOV   BL, ORG_SOURCE_DRIVE
        CALL  GET_LOGICAL_DRIVE         ;CHECK IF SOURCE DRIVE OWNERSHIP HAS NOT BEEN CHAGNED?

        CMP   AL, ORG_SOURCE_DRIVE
;       $IF   NE                        ;IF IT HAS BEEN CHANGED TO TARGET, THEN A SINGLE DRIVE COMPARE.
        JE $$IF30
            MOV   COPY_TYPE, ONE
            MOV   BL, ORG_SOURCE_DRIVE
            MOV   ORG_TARGET_DRIVE, BL  ;SET TARGET DRV LETTER TO THE SOURCE.
            MOV   BL, ASCII_DRV1_ID
            MOV   ASCII_DRV2_ID, BL
            MOV   BL, ORG_SOURCE_DRIVE
            CALL  SET_LOGICAL_DRIVE     ;SET THE OWNER BACK TO SOURCE DRV LETTER

;       $ELSE
        JMP SHORT $$EN30
$$IF30:
            CMP   AL, ORG_TARGET_DRIVE  ; SOURCE DRV LETTER = TARGET DRV LETTER CASE, FOR EX. DISKCOMP A: A:
;           $IF   E
            JNE $$IF32
                MOV   COPY_TYPE, ONE
;           $ELSE
            JMP SHORT $$EN32
$$IF32:
                MOV   COPY_TYPE, TWO
;           $ENDIF
$$EN32:
;       $ENDIF
$$EN30:

        POP   AX

        RET
CHK_SINGLE_DRV_OP ENDP
        HEADER <GET_LOGICAL_DRIVE - GET LOG. DRIV NO. WHO OWNS PHYSICAL DRIVE>
;******************************************************************************
GET_LOGICAL_DRIVE PROC NEAR
        PUBLIC GET_LOGICAL_DRIVE        ;MAKE ENTRY IN LINK MAP                 ;AN000;
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
        JNE $$IF36
            MOV   AL, BL                ;THEN SET IT TO THE INPUT DRIVE LETTER
;       $ENDIF
$$IF36:

        RET

GET_LOGICAL_DRIVE ENDP
        HEADER <DISKETTE_DRV_TYPE - CHECK COMPATABILITY SOURCE/TARGET DRIVES>
;******************************************************************************
; SUBROUTINE NAME :  DISKETTE_DRV_TYPE DOES THE FOLLOWING:                    *
;                    - GETS SOURCE, TARGET DRIVE INFORMATIONS                 *
;                    - CHECK IF IT IS A REMOVABLE DRIVE.                      *
; INPUT           :  SOURCE_DRIVE  &  TARGET_DRIVE                            *
;                          1 = DRIVE A          2 = DRIVE B, ETC.             *
;                                                                             *
; OUTPUT          :  DX : FINE    - NO ERROR                                  *
;                         (OTHERS)- ERROR MSG OFFSET                          *
;******************************************************************************
DISKETTE_DRV_TYPE PROC NEAR
        PUBLIC DISKETTE_DRV_TYPE        ;MAKE ENTRY IN LINK MAP                 ;AN000;

        PUSH  AX
        XOR   BX,BX
        MOV   BL, ORG_SOURCE_DRIVE
        MOV   CL, GETDEVPARM            ;=60h
        MOV   DX, OFFSET DS_IOCTL_DRV_PARM ;POINTER TO THE CONTROL STRING
        CALL  GENERIC_IOCTL             ;GET DEVICE PARM.

        TEST  DS_deviceAttributes, 0001h ;CHECK REMOVABLE. 0001 = NOT REMOVABLE
	JZ  check_fixed_disk			;Removable, hard disk also?;C05
        JMP $$IF38                              ;Not removeable, can't copy;C05
check_fixed_disk:                               ;Here to check for fixed d ;C05
        cmp   DS_DeviceType,FixedDisk           ;Q: Removeable fixed disk? ;C05
        JE  $$IF38                              ; Y: then error out        ;C05

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
        MOV   BL, ORG_TARGET_DRIVE
        MOV   CL, GETDEVPARM
        MOV   DX, OFFSET DT_IOCTL_DRV_PARM
        CALL  GENERIC_IOCTL

        TEST  DT_deviceAttributes, 0001h
	JNZ $$IF38				;TARGET IS NOT FIXED DISK, OK		    ;AC000;
        cmp   DS_DeviceType,FixedDisk           ;Q: Removeable fixed disk? ;C05
        JE  $$IF38                              ; Y: then error out        ;C05
	MOV   AX, DT_numberOfCylinders
	MOV   T_DRV_TRACKS, AX
	MOV   BX, OFFSET DT_BPB_PTR
	MOV   AX, [BX].CHead
	MOV   T_DRV_HEADS, AL
	MOV   AX, [BX].CSECT_TRACK
	MOV   T_DRV_SECT_TRACK, AL

;*** CHECK DEVICE COMPATIBILITY
	MOV   DX, FINE		    ;GUESS, ALL WILL BE OK
				    ; DX MAY BE CHANGED TO REFLECT ERROR
	CMP   DS_deviceType, DRV_720 ;0 - 48 TPI, 5.25", 96 TPI,
				    ; 5.25", 2 - 720kb, 3.5"
	JNE $$IF39		    ;WILL ONLY ALLOW DISKCOPY BETWEEN
				    ; 720KB, 3.5 SOURCE, TARGET

	CMP   DT_deviceType, DRV_720 ;target = 720KB also?
	JE    $$EN38
	CMP   DT_deviceType, DRV_OTHER ;target might be 1.44M ;C02
	JE    $$EN38
	MOV   DX, OFFSET MSGNUM_NOT_COMPATIBLE ;
	jmp   short $$EN38

$$IF39:
	CMP   DT_deviceType, DRV_720 ;SOURCE IS NOT 720kb,
				    ; IS TARGET 720?
	JNE   $$EN38		    ;IF SO, THEN
				    ;DDT IS NOT COMPATIBLE
	CMP   DS_deviceType, DRV_OTHER ;source might be 1.44M ;C02
	JE    $$EN38
	MOV   DX, OFFSET MSGNUM_NOT_COMPATIBLE ;
	JMP   SHORT $$EN38	    ;SINCE SOURCE IS FIXED DISK, ERROR	    ;AC000;

$$IF38:
	MOV   DX, OFFSET MSGNUM_INVALID_DRV ;ISSUE INVALID DRV MSG	    ;AC000;

$$EN38:
        POP   AX
        RET

DISKETTE_DRV_TYPE ENDP
        HEADER <CHECK_REDIRECTION - IS DEVICE REDIRECTED?>
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
        PUBLIC CHECK_REDIRECTION        ;MAKE ENTRY IN LINK MAP                 ;AN000;

        PUSH  AX                        ;SAVE REGISTERS
        PUSH  BX
        PUSH  CX

        MOV   CX,DX                     ;SAVE RET TEMPORARILY
        MOV   AH,IOCTL_FUNC             ;GET IOCTL FUNTION &
        MOV   AL,REDIRECTED_FUNC        ;IOCTL SUB-FUNCTION ******CHECK***

        INT   21H                       ;AND GO FIND OUT IF IT'S LOCAL
;       $IF   C
        JNC $$IF52
            MOV   CX,OFFSET MSGNUM_INVALID_DRV ;REDIR INVALID                   ;AC000;

;       $ELSE
        JMP SHORT $$EN52
$$IF52:
            TEST  DX,REMOTE_DRV         ;IF DRIVE IS REDIRECTED
;           $IF   NZ
            JZ $$IF54

                MOV   CX,OFFSET MSGNUM_DRV_REDIRECTED ;                         ;AC000;
;           $ENDIF
$$IF54:
;       $ENDIF
$$EN52:
        MOV   DX,CX                     ;GET ERROR MSG @

        POP   CX                        ;RESTORE REGISTERS
        POP   BX
        POP   AX
        RET                             ;RETURN TO CALLER
CHECK_REDIRECTION ENDP
        HEADER <BUFFER_SIZE - FINDS START AND END OF BUFFER>
;******************************************************************************
; SUBROUTINE NAME :  BUFFER_SIZE    DETERMINES WHERE BUFFER STARTS & ENDS     *
; INPUT           :  NONE                                                     *
;                                                                             *
; OUTPUT          :  BUFFER_BEGIN ADDRESS                                     *
;                 :  BUFFER_END   ADDRESS                                     *
;                 :  START_BUFFER ADDRESS
;******************************************************************************
BUFFER_SIZE PROC NEAR
        PUBLIC BUFFER_SIZE              ;MAKE ENTRY IN LINK MAP                 ;AN000;

        PUSH  AX                        ;SAVE REGISTERS
        PUSH  BX
        PUSH  CX
        MOV   BX,OFFSET init            ;GET ADDR OF INIT+1024 AS A FREE MEMORY
        add   bx, 1024                  ;(OFFSET FROM CS, IN BYTES)
        MOV   CL,4                      ;CONVERT OFFSET INTO SEGMT BY DIVIDING
        SHR   BX,CL                     ;IT BY 16

        MOV   AX,CS                     ;CS + OFFSET => INIT @ IN SEGMENT
        ADD   BX,AX                     ;WHERE BUFFER CAN START

                                        ;NEED TO START AT A NEW SECTOR ==>
        AND   BL,CLEAR_SEGMENT          ;TRUNCATE TO PREVIOUS 512 BYTE BOUNDRY
                                        ;(GET PREVIOUS SECTOR NUMBER)
        ADD   BX,20H                    ;THEN, ADVANCE TO THE BEGINNING OF
                                        ;NEXT SECTOR (SINCE PART OF PREVIOUS
                                        ;SECTOR WAS USED)

        MOV   BUFFER_BEGIN,BX           ;SAVE OUR BUFFER START SEGMENT ADDR
        MOV   START_BUFFER,BX           ;SAVE IT AGAIN ELSEWHERE
                                        ;(AT THE BEGINNING OF A SECTOR WITH
                                        ;SEGMENT BITS CLEARED)

        MOV   BX,DS:TWO                 ;GET ADDR WHERE BUFFER ENDS
        MOV   BUFFER_END,BX             ;(TOP OF MEMORY, OFFSET 2 IN PSP)

        POP   CX                        ;RESTORE REGISTERS
        POP   BX
        POP   AX
        RET                             ;RETURN TO CALLER
BUFFER_SIZE ENDP
        HEADER <SETUP_CTRL_BREAK - SETUP THE CTRL-BREAK VECTOR>
;******************************************************************************
SETUP_CTRL_BREAK PROC NEAR              ;SETUP CTRL-BREAK VECTOR
        PUBLIC SETUP_CTRL_BREAK         ;MAKE ENTRY IN LINK MAP                 ;AN000;
;******************************************************************************
        PUSH  AX
        PUSH  BX
        PUSH  DX
        PUSH  ES

        MOV   AX,2523H                  ;SET THE CTRL-BREAK VECTOR
        MOV   DX,OFFSET MAIN_EXIT
        INT   21H

        POP   ES
        POP   DX
        POP   BX
        POP   AX
        RET

SETUP_CTRL_BREAK ENDP
        HEADER <CHECK_SERVER - IS SERVER OR REDIRECTOR LOADED?>
;******************************************************************************
CHECK_SERVER PROC NEAR                  ;SEE IF SERVER OR REDIRECTOR IS IN++
        PUBLIC CHECK_SERVER             ;MAKE ENTRY IN LINK MAP                 ;AN000;
;
; INPUT: BL = DRIVE NUMBER (1=A,2=B ETC....)
;******************************************************************************
        MOV   AH,0                      ;SEE IF SERVER LOADED
        INT   SERVER
        CMP   AH,0
;       $IF   E
        JNE $$IF57
            MOV   DX,FINE
;       $ELSE
        JMP SHORT $$EN57
$$IF57:
            DEC   BL
            ADD   BL,"A"                ;CONVERT TO ASCII DRIVE LETTER
            MOV   ASCII_DRIVE_LETTER,BL ;PUT IN ASCIIZ STRING
            MOV   SI,OFFSET ASCII_DRIVE_LETTER
            MOV   AH,SHARED
            CLC
            INT   SERVER
;           $IF   C
            JNC $$IF59
                MOV   DX,OFFSET MSGNUM_DRV_REDIRECTED ;AC000;
;           $ELSE
            JMP SHORT $$EN59
$$IF59:
                MOV   DX,FINE
;           $ENDIF
$$EN59:
;       $ENDIF
$$EN57:
        RET
CHECK_SERVER ENDP

        Public INIT_END
INIT_END LABEL BYTE
        PATHLABL COMPINIT               ;AN013;
CSEG    ENDS
        END
