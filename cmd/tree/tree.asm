	PAGE    90,132                  ;AN000
	TITLE   TREE.SAL - DISPLAY THE SUBDIRECTORY TREE ;AN000;
LISTPARM =      1                       ;AN000;0=SUPPRESS LIST; 1=ALLOW LIST

;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

;       .XLIST
;****************** START OF SPECIFICATIONS *****************************
; MODULE NAME: TREE

; DESCRIPTIVE NAME: Tree structure of subdirectories is displayed.

; FUNCTION: Displays to standard output a graphic representation
;           of the subdirectory tree structure, beginning
;           with the specified subdirectory, and optionally
;           displaying all filenames in that tree.

; ENTRY POINT: START

; INPUT: (DOS COMMAND LINE PARAMETERS)

;             [d:][path] TREE [D:][path] [/F] [/A]

;        WHERE
;             [d:][path] - Path where the TREE command resides.

;             [D:][path] - Display of subdirectories starts with this
;                          specified subdirectory.  If this is not
;                          specified, the default is the drive root directory.

;             [/F]       - This requests the files in each subdirectory
;                          in addition to the subdirectories themselves
;                          are to be listed.

;             [/A]       - This requests use of alternate graphic chars

; EXIT-NORMAL:  ERRORLEVEL 0 - Normal completion

; EXIT-ERROR:   ERRORLEVEL 1 - I/O error

;               ERRORLEVEL 2 - Incorrect DOS version

;               ERRORLEVEL 3 - Control Break termination

; EFFECTS: The result is a display of the Tree of subdirectories.
;          No changes are made to the system, to the current subdirectory,
;          nor to the current DOS default drive.

;                   1. NO FILES, JUST SUBDIRECTORIES

;                D:\ROOT
;                ÃÄÄÄSUBDIR1
;                ÃÄÄÄSUBDIR2
;                ³   ÃÄÄÄSUBDIR21
;                ³   ÀÄÄÄSUBDIR22
;                ÀÄÄÄSUBDIR3
;                    ÃÄÄÄSUBDIR31
;                    ÀÄÄÄSUBDIR32

;                   2. FILES AND SUBDIRECTORIES

;                D:\ROOT
;                ³   MAINFIL1
;                ³   MAINFIL2
;                ³
;                ÃÄÄÄSUBDIR1
;                ³       FILE1
;                ³       FILE2
;                ³
;                ÃÄÄÄSUBDIR2
;                ³   ³   FILE2A
;                ³   ³   FILE2B
;                ³   ³
;                ³   ÃÄÄÄSUBDIR21
;                ³   ³       FILEA
;                ³   ³       FILEB
;                ³   ³
;                ³   ÀÄÄÄSUBDIR22
;                ³           FILEC
;                ³           FILED
;                ³           FILEF
;                ³
;                ÀÄÄÄSUBDIR3
;                    ÃÄÄÄSUBDIR31
;                    ÀÄÄÄSUBDIR32
;                            FILE32A
;                            FILE32B

; INCLUDED FILES: TREEQU.INC - EQUATES
;                 PATHMAC.INC - PATHGEN MACRO

; INTERNAL REFERENCES:
;    ROUTINES:

;     BEGIN - VERSION CHECK, SYSMSG INIT, EXIT TO DOS
;     DEFINE_GRAPHICS - GET GRAPHIC CHARS FROM MSG
;     PARSE - TOKENIZE THE DOS COMMAND LINE PARMS
;     VERIFY_DRIVE - CHECK IF USER DRIVE ID IS OK
;     INIT_CONDITIONS - GET INITIAL SUBDIR, APPEND,CTL_BREAK
;     GET_VOL_LABEL - GET VOLUME LABEL ON SPECIFIED DRIVE
;     VOLSER - DISPLAY VOLUME SERIAL NUMBER, IF ANY AN001
;     LEN_ASCIIZ - GET LENGTH OF ASCIIZ STRING
;     EXECUTE - LOOK THRU DIRECTORY LIST FOR SUBDIRS
;     ANY_MORE_SUBDIR - LOOK AHEAD,SEE IF MORE SUBDIR
;     FIND_TYPE_NORMAL - PROCESS NORMAL, NON-DIR, FILES
;     FIND_TYPE_DIR - PROCESS THE DIRECTORY
;     NEXT_LEVEL - SET UP TO LOOK AT LOWER LEVEL SUBDIR
;     BEGIN_FIND - DO FIND FIRST FILE
;     FIND_NEXT - LOOK FOR NEXT ENTRY IN DIRECTORY
;     SHOW_FN - DISPLAY THE FILENAME FOUND
;     FLN_TO_BUF - MOVE FILENAME TO BUFFER
;     GRAF_TO_BUF - SELECT LEADING GRAPHIC CHAR FOR BUF
;     BLANK_DASH - PUT BLANKS OR DASHES BEFORE FILENAME
;     FIX_GRAF - CHANGE CURRENT GRAPHIC FOR NEXT LINE
;     ANY_SUBDIRS - DISPLAY MSG IF NO SUBDIRS PRINTED
;     DO_WRITE - SEND STRING TO STDOUT
;     IF_NOMOREFILES - ASK EXTENDED ERROR FOR WHY IS ERROR
;     GET_EXTERR - CALL EXTENDED ERROR
;     SENDMSG - PASS IN REGS DATA FROM MSG DESCRIPTOR TO DISP MSG
;     BREAK_HANDLER - CONTROL BREAK VECTOR POINTS HERE
;     RESTORE - RETURN TO INITIAL DOS DEFAULT DRIVE
;     MYERRORHANDLER - SERVICE CRITICAL ERROR HANDLER
;     CHK_DBCS -SEE IF SPECIFIED BYTE IS A DBCS LEAD BYTE

;    DATA AREAS:
;       PSP - Contains the DOS command line parameters.
;       STACK - Dynamic allocation of workareas.

; EXTERNAL REFERENCES:
;    ROUTINES:
;       SYSDISPMSG (FAR)  - MESSAGE DISPLAY ROUTINE
;       SYSLOADMSG (FAR)  - SYSTEM MESSAGE LOADER
;       PARSER     (NEAR) - INTERROGATE DOS COMMAND LINE PARMS

;    DATA AREAS:
;       DTA - defined by the DOS FINDFIRST function.

; NOTES:
;        This module should be processed with the SALUT pre-processor
;        with the re-alignment not requested, as:

;               SALUT TREE,NUL

;        To assemble these modules, the sequential or alphabetical
;        ordering of segments may be used.

;        Sample LINK command:

; LINK @TREE.ARF

; Where the TREE.ARF is defined as:
;              TREE+
;              TREEPAR+
;              TREESYSP+
;              TREESYSM
;              TREE

;        These modules should be linked in this order.  The load module is
;        a COM file.  It should be converted via EXE2BIN to a .COM file.

; REVISION HISTORY: A000 Version 4.00: add PARSER, System Message Handler,
;                   Display graphically the subdirectories and their files.
;                   A001 DCR 27, display vol serial number, if present.
;                   A002 Add support for /A switch for alternate graphics.
;                   A003 PTM 471 Avoid duplicate switches
;                   A004 PTM 537 Display parm in error
;                   A005 PTM 692 Remove period from vol label field
;                   A006 PTR1044 Append interface change
;                   A007 PTM1082 Critical error handler
;                   A008 PTM1199 DEFAULT DIR OF TARGET ALTERED
;                   A009 PTM1416 INT24 CLOBBERED USER'S RESPONSE
;                   A010 PTM1406 GET MEDIA ID WITH 69H, NOT IOCTL
;                   A011 PTM1821 COPYRIGH.INC moved to within msgserv.asm
;                   A012 PTM2352 DBCS ENABLING, CHECKING FOR "\"
;                   A013 PTM3512 PATHGEN
;                   A014 PTM3560 INVALID PATH DOES NOT DISPLAY PATHNAME
;

;****************** END OF SPECIFICATIONS *****************************
	IF1                             ;AN000;
	    ;%out    COMPONENT=TREE, MODULE=TREE.SAL... ;AN000;
	ENDIF                           ;AN000;
	HEADER  <MACRO DEFINITIONS>     ;AN000;
	INCLUDE PATHMAC.INC             ;AN013;
	include cpmfcb.inc              ; leaf
; =  =  =  =  =  =  =  =  =  =  =  =
FIXLIST MACRO   LP,DOIT                 ;;AN000;
	IF      LP                      ;;AN000;
	    DOIT                        ;;AN000;
	ENDIF                           ;;AN000;
	ENDM                            ;;AN000;
; =  =  =  =  =  =  =  =  =  =  =  =
HEADER  MACRO   TEXT                    ;;AN000;
	FIXLIST LISTPARM,.XLIST         ;;AN000;
	SUBTTL  TEXT                    ;;AN000;
	FIXLIST LISTPARM,.LIST          ;;AN000;
	PAGE                            ;;AN000;
	ENDM                            ;;AN000;
; =  =  =  =  =  =  =  =  =  =  =  =
;                                  $SALUT (0,36,41,52) ;AN000;
DOSCALL                            MACRO FN,SF     ;;AN000;
				   IFNB <FN>       ;;AN000;ARE THERE ANY PARMS AT ALL?
				   IFNB <SF>       ;;AN000;
				   MOV  AX,(FN SHL 8)+SF ;;AN000;AH=FN;AH=SF
				   ELSE            ;;AN000;SINCE THERE IS NO SUB FUNC
				   MOV  AH,FN      ;;AN000;
				   ENDIF           ;;AN000;
				   ENDIF           ;;AN000;
				   INT  21H        ;;AN000;
				   ENDM            ;;AN000;

	     HEADER <EQUATES - DOS FUNCTION CALLS> ;AN000;
	     INCLUDE TREEQU.INC    ;AN000;EQUATES, CONTROL BLOCKS

; =  =  =  =  =  =  =  =  =  =  =  =

;
; Set F_Carry based on current carry value in flags register.
; Leave carry in flags register unchanged.
;
Set_F_Carry MACRO
   LOCAL C_High, C_Low, SFC_Exit

   jc   C_High

C_Low:
   and  Flags, NOT F_Carry
   clc
   jmp  short SFC_Exit

C_High:
   or   Flags, F_Carry
   stc

SFC_Exit:

ENDM

; =  =  =  =  =  =  =  =  =  =  =  =

Copy_DTA MACRO Source_DTA, Dest_DTA

  mov  si, offset Source_DTA
  mov  di, offset Dest_DTA
  mov  cx, DTA_Length
  rep  movsb

ENDM

; =  =  =  =  =  =  =  =  =  =  =  =

;
; Copy all information (which isn't already there) pertinent to the current
; directory level from global variables on to the stack frame.
;
Push_Frame MACRO

  ; We don't need to copy Fram_Char or Fram_Curr_Path since we have already
  ; been using them on the stack.

  ; Copy Next_DTA on to stack.
  mov  si, offset Next_DTA
  mov  di, bp
  add  di, Fram_Next_DTA
  mov  cx, DTA_Length
  rep  movsb

  ; Copy Flags.
  mov  al, Flags
  mov  [bp].Fram_Flags, al

ENDM

; =  =  =  =  =  =  =  =  =  =  =  =

;
; Copy all information (which isn't already there) pertinent to the previous
; directory level from the stack frame into global variables.
;
Pop_Frame MACRO

  ; Copy Next_DTA off stack.
  mov  si, bp
  add  si, Fram_Next_DTA
  mov  di, offset Next_DTA
  mov  cx, DTA_Length
  rep  movsb

  ; Copy Flags.
  mov  al, [bp].Fram_Flags
  mov  Flags, al

ENDM

; =  =  =  =  =  =  =  =  =  =  =  =

LISTPARM     =    1                ;AN000;PERMIT LISTING
.LIST                              ;AN000;
	     HEADER <STATIC DATA AREA> ;AN000;


CSEG         SEGMENT PARA PUBLIC 'CODE' ;AN000;


	     ASSUME CS:CSEG,DS:CSEG,ES:CSEG,SS:CSEG ;AN000;AS SET BY DOS LOADER

; =  =  =  =  =  =  =  =  =  =  =  =
; $SALUT (4,3,8,36)                ;AN000;
  EXTRN SUBLIST_PARSE:WORD         ;AN004;PARSE ERROR XX - %0

  EXTRN MSGNUM_VOL:WORD            ;AN000;"Directory PATH listing for Volume %1"
  EXTRN SUBLIST_VOL:WORD           ;AN000;SUBLIST TO VOL LABEL IN Current_DTA_Filename

  EXTRN MSGNUM_LIST:WORD           ;AN000;"Directory PATH listing"

  EXTRN MSGNUM_INVPATH:WORD        ;AN000;"INVALID PATH"
  EXTRN SUBLIST_INVPATH:WORD       ;AN014;THE ASCIIZ PATH CONSIDERED INVALID

  EXTRN MSGNUM_EXTERR:WORD         ;AN000;ALL EXTENDED ERRORS
  EXTRN MSGNUM_NOSUB:WORD          ;AN000;"No subdirectories exists"

  EXTRN MSGNUM_SERNO:WORD          ;AN001;"Volume Serial Number is %1-%2"
  EXTRN SUBLIST_6A:WORD            ;AN001;FIRST PART OF SERIAL NUMBER
  EXTRN SUBLIST_6B:WORD            ;AN001;SECOND PART OF SERIAL NUMBER

  EXTRN CURRENT_PARM:WORD          ;AN000;POINT TO NEXT PARM TO PARSE
  EXTRN ORDINAL:WORD               ;AN000;NUMBER OF CURRENT PARM
  EXTRN LAST_BYTE:BYTE             ;AN000;TAG AT END OF USED MEMORY, BEFORE STACK

  EXTRN SYSDISPMSG:NEAR            ;AN000;MESSAGE DISPLAY ROUTINE
  EXTRN SYSLOADMSG:NEAR            ;AN000;SYSTEM MESSAGE LOADER
  EXTRN SYSGETMSG:NEAR             ;AN002;SYSTEM MESSAGE LOCATER ROUTINE
  EXTRN PARSER:NEAR                ;AN000;INTERROGATE DOS COMMAND LINE PARMS
; =  =  =  =  =  =  =  =  =  =  =  =
;            $SALUT (0,14,19,36)   ;AN000;

	     ORG  80H              ;AN000;

	     PUBLIC COMMAND        ;AN000;
COMMAND      DB   128 DUP (?)      ;AN000;DOS INPUT COMMAND LINE
; =  =  =  =  =  =  =  =  =  =  =  =

	     ORG  100H             ;AN000;REQUIRED LOCATION OF ENTRY POINT

START:

IFDEF MAKE_EXE
;
; .EXE stub to turn .COM into .EXE for debugging.
;

	      ; duplicate PSP at start of CS for .EXE
	      mov  first_ax, ax
	      mov  ax, cs
	      mov  es, ax
	      xor  si, si
	      xor  di, di
	      mov  cx, 100h
	      cld
	      rep  movsb

	      ; set up data segment and stack just like .COM
	      mov  ds, ax
	      mov  ss, ax
	      mov  sp, 0fffeh

	      ; restore original ax
	      mov  ax, first_ax
ENDIF

	      JMP  BEGIN           ;AN000;DOS ENTRY POINT

IFDEF MAKE_EXE
	 first_ax dw ?
ENDIF


; =  =  =  =  =  =  =  =  =  =  =  =
;THERE ARE TWO SETS OF DEFINITIONS OF THE GRAPHIC CHARACTERS USED IN THE DISPLAY.
;THE FIRST SET LOOKS THE BEST, BUT ON SOME PRINTERS IS A TEDIOUS, SLOW PROCESS.
;THERE ARE SOME CODEPAGES THAT DO NOT HAVE THESE SAME GRAPHIC CHARACTERS IN
;THESE CORRESPONDING CODE POINT POSITIONS.  JAPAN HAS ITS KATAKANA CHARACTER
;SET WHERE THESE GRAPHICS ARE DEFINED, AND WOULD THUS NOT WANT TO USE THIS
;FIRST GRAPHIC CHARACTERS SET.  THE SECOND SET OF EQUATES DEFINE ALTERNATE
;CHARACTERS THAT, ALTHOUGH THE OUTPUT DOES NOT LOOK AS GOOD, AT LEAST WILL
;PRINT NORMALLY, AND DOES USE THE TRADITIONAL ASCII LOWER 128 AS ITS CODE
;POINTS, THUS WOULD BE AVAILABLE FOR THOSE OTHER CODEPAGES, LIKE JAPAN'S.

;IF IT BECOMES DESIRABLE TO GENERATE YET ANOTHER DEFINITION OF THESE CHARACTERS,
;THE REQUIREMENTS ARE:
;       1. NONE OF THE FOUR CAN BE BLANK
;       2. EACH OF THE FOUR MUST BE UNIQUE
;       3. EACH CHAR MUST BE A SINGLE BYTE (NO DBCS)

;               GRAPHIC CHARACTERS
;THIS SET OF GRAPHIC CHARACTERS ARE ACTUALLY DEFINED BY THE MESSAGE 7,
;WHERE TRANSLATORS HAVE PROVIDED THE CHARACTERS COMPATABLE WITH THEIR
;NATIONAL CHARACTER CODEPAGE SET.

INCLUDE version.inc

IFNDEF TAIWAN
GRAF_TABLE   LABEL BYTE            ;AN002;DEFINITION OF FOUR GRAPHIC CHARACTERS
	     PUBLIC GRAF_TABLE     ;AN002;
ifndef JAPAN

Graf_Elbow    DB   "À"             ;AN000;192 DECIMAL ASCII VAL
GRAF_DASH    DB   "Ä"              ;AN000;196 DECIMAL ASCII VALUE
GRAF_TEE     DB   "Ã"              ;AN000;195 DECIMAL ASCII VALUE
GRAF_BAR     DB   "³"              ;AN000;179 DECIMAL ASCII VALUE

else                            ; if JAPAN

Graf_Elbow    DB   1
GRAF_DASH    DB   2
GRAF_TEE     DB   3
GRAF_BAR     DB   4

endif


;               ALTERNATE SET OF GRAPHIC CHARACTERS
;IF THE "/A" SWITCH IS SPECIFIED, THIS SET OF FOUR CHARACTERS WILL
;OVERLAY THE ABOVE SET OF GRAPHIC CHARACTERS.

GRAF_TABLE_ALT LABEL BYTE          ;AN002;ALTERNATE SET OF GRAPHIC CHARACTERS
	     PUBLIC GRAF_TABLE_ALT ;AN002;
ifndef JAPAN
A_Graf_Elbow  DB   "\"             ;AN000;
A_GRAF_DASH  DB   "-"              ;AN000;
A_GRAF_TEE   DB   "+"              ;AN000;
A_GRAF_BAR   DB   "|"              ;AN000;

else                            ; if JAPAN

A_Graf_Elbow  DB   "+"
A_GRAF_DASH  DB   "-"
A_GRAF_TEE   DB   "|"
A_GRAF_BAR   DB   "|"
endif

  ELSE                          ; if TAIWAN

GRAF_TABLE   LABEL BYTE            ;AN002;DEFINITION OF FOUR GRAPHIC CHARACTERS
	     PUBLIC GRAF_TABLE     ;AN002;
Graf_Elbow  DB  "\"              ;AN000;
GRAF_DASH  DB   "-"              ;AN000;
GRAF_TEE   DB   "+"              ;AN000;
GRAF_BAR   DB   "|"              ;AN000;

;               ALTERNATE SET OF GRAPHIC CHARACTERS
;IF THE "/A" SWITCH IS SPECIFIED, THIS SET OF FOUR CHARACTERS WILL
;OVERLAY THE ABOVE SET OF GRAPHIC CHARACTERS.

GRAF_TABLE_ALT LABEL BYTE          ;AN002;ALTERNATE SET OF GRAPHIC CHARACTERS
	     PUBLIC GRAF_TABLE_ALT ;AN002;
A_Graf_Elbow    DB   "À"             ;AN000;192 DECIMAL ASCII VAL
A_GRAF_DASH    DB   "Ä"              ;AN000;196 DECIMAL ASCII VALUE
A_GRAF_TEE     DB   "Ã"              ;AN000;195 DECIMAL ASCII VALUE
A_GRAF_BAR     DB   "³"              ;AN000;179 DECIMAL ASCII VALUE
ENDIF

; =  =  =  =  =  =  =  =  =  =  =  =
FLAGS        db   0                ;AN000;INITIALIZE ALL FLAGS TO "FALSE"
	     PUBLIC FLAGS,F_SWITCH ;AN000;ADD ENTRIES IN LINK MAP
F_DEF_PAT_TAR EQU 40H              ;AN008;IF ON, DEFAULT SUBDIR OF TARGET DRIVE IS KNOWN
				   ;IF OFF, DEF SUBDIR OF TARGET NOT KNOWN     ;AN008;
F_SUBDIR     EQU  20H              ;AN000;IF ON, A SUBDIR HAS BEEN DISPLAYED
				   ;IF OFF, A SUBDIR HAS NOT YET BEED DISPLAYED
F_FAILING    EQU  10H              ;AN000;IF ON, DO NOT RESTORE SUBDIR ON FAILING DRIVE
				   ;IF OFF, DO RESTORE SUBDIR ON TARGET DRIVE   ;AN000;
F_FLN       EQU  08H               ;AN000;IF ON, A FILENAME HAS BEEN DISPLAYED
				   ;IF OFF, NO FILNAME FOR THIS SUBDIR YET
F_SWITCH     EQU  02H              ;AN000;IF ON, THE /F SPECIFIED
				   ;IF OFF, THEN /F NOT SPECIFIED
F_APPEND     EQU  01H              ;AC006;IF ON, DOS APPEND IS IN THE MULTIPLEXOR
				   ;IF OFF, DOS APPEND IS NOT THE MULTIPLEXOR

F_Carry      equ  80h         ; 0 ==> last DOSCall FindNext didn't set carry
			      ; 1 ==> last DOSCall FindNext did set carry

APPEND_FLAGS DW   0                ;AN006;RECORDS ORIGINAL STATE OF APPEND
				   ;8000H = /X:1
				   ;4000H = /E
				   ;2000H = /PATH:1
				   ;1000H = /DRIVE:1
				   ;0001H = ENABLE APPEND

DBCSENV      DD   0                ;AN000;POINTER TO DBCS RANGES
ORIG_AX      DW   0                ;AN000;DRIVE VERIFICATION FROM DOS AT ENTRY
CURRENT_COL  DW   1                ;AN000;IN BUF, WHERE IS ELBO/TEE?
				   ; INITIALLY SET TO START IN COLUMN ONE
MEDIA_ID_BUF A_MEDIA_ID_INFO <>    ;AN001;AREA TO READ VOL SERIAL NUMBER WITH GET_MEDIA_ID
BUF          DB   ((DASH_NUM+1)*LEVEL_LIMIT+2) DUP(0) ;AN000;HAS ELBO,TEE,DASH,NUL ENDED
JUSTIN_CASE  DB   64 DUP(0)        ;AN000;CATCHES THE OVERFLOW
EXITFL       DB   EXOK             ;AN000;RETURN CODE, INITIALLY "NORMAL"
;          (SEE INCLUDED FILE OF EQUATES FOR DEFINITIONS OF VALUES)

;               REMEMBER THE DOS DEFAULT DRIVE AND SUBDIRECTORY
DEFAULT_DR   DB   ?                ;AN000;ALPHA LETTER OF DOS DEFAULT DRIVE
START_DR_NUM DB   ?                ;AN000;NUMERIC VALUE OF DOS DEFAULT DRIVE
				   ; WHERE 0=A:, 1=B:, ETC...
DEFAULT_PATH DB   BACK_SLASH       ;AN000;FIRST BYTE OF PATH IS BACKSLASH
	     DB   MAX_PATH DUP(0)  ;AN000;ORIGINAL DEFAULT PATH
JUSTIN_CASE2 DB   64 DUP(0)        ;AN000;CATCHES THE OVERFLOW
OLDINT23     DD   ?                ;AN000;ORIGINAL CONTENTS OF CTRL-BREAK VECTOR
OLDINT24     DD   ?                ;AN000;ORIGINAL CONTENTS OF CRITICAL ERROR VECTOR

QUESTION_MARK   EQU   03FH
PATH_LEN        EQU   11

;
; Current_DTA and Next_DTA are used in a single lookahead scheme for DOS
; FindFirst / FindNext calls.  This method eliminates an extra level of
; search which would otherwise be needed to set the leading graphics
; character before a file or directory name.  We also avoid re-using old
; "snapshots" of DTAs which often fail on network servers with limited
; resources.
;
; The filename from Current_DTA has always been printed before descending to
; a subdirectory, so the search position in a directory may be maintained by
; only keeping a copy of Next_DTA.
;
Current_DTA  DB   21 DUP(?)        ;AN000;RESERVED FOR FIND NEXT CALLS
TREE_FCB     EQU  Current_DTA + 7  ; this space will be used for FCB calls also
Current_DTA_Attr DB   ?            ;AN000;ATTRIBUTE
DTA_Attr_Offset  equ Current_DTA_Attr - Current_DTA
Current_DTA_Time DW   ?            ;AN000;TIME
Current_DTA_Date DW   ?            ;AN000;DATE
Current_DTA_LSize DW   ?                   ;AN000;LOW WORD OF FILE SIZE
Current_DTA_HSize DW   ?                   ;AN000;HIGH WORD OF FILE SIZE
DTA_Filename_Offset  equ Current_DTA_Filename - Current_DTA
Current_DTA_Filename DB   13 DUP(?)        ;AN000;FILENAME, WITH PERIOD, +0 BYTE
	     PUBLIC Current_DTA_Filename   ;AN000;USED TO DISPLAY VOLUME LABEL
EXTRA_SPACE  DB   20               ;leaf; want to use the DTA as space for an
; fcb, since the volume label will be found via fcb-based FindFirst, and i
; was concerned that the dta as defined above wasn't large enough


Next_DTA  DB   21 DUP(?)           ;AN000;RESERVED FOR FIND NEXT CALLS
Next_DTA_Attr DB   ?               ;AN000;ATTRIBUTE
Next_DTA_Time DW   ?               ;AN000;TIME
Next_DTA_Date DW   ?               ;AN000;DATE
Next_DTA_LSize DW   ?              ;AN000;LOW WORD OF FILE SIZE
Next_DTA_HSize DW   ?              ;AN000;HIGH WORD OF FILE SIZE
Next_DTA_Filename DB   13 DUP(?)           ;AN000;FILENAME, WITH PERIOD, +0 BYTE


;
; File_DTA is used to search the current directory for files.
;
File_DTA  DB   21 DUP(?)           ;AN000;RESERVED FOR FIND NEXT CALLS
File_DTA_Attr DB   ?               ;AN000;ATTRIBUTE
File_DTA_Time DW   ?               ;AN000;TIME
File_DTA_Date DW   ?               ;AN000;DATE
File_DTA_LSize DW   ?              ;AN000;LOW WORD OF FILE SIZE
File_DTA_HSize DW   ?              ;AN000;HIGH WORD OF FILE SIZE
File_DTA_Filename DB   13 DUP(?)           ;AN000;FILENAME, WITH PERIOD, +0 BYTE


EXTENDED_FCB    EQU     0FFH       ; leaf
FCB_FINDFIRST   EQU     11H         ; leaf

CRLF         db   CR, LF
LEN_CRLF     equ  $ - CRLF
Star_Star    DB   "*.*",0          ;AN000;UNIVERSAL FILENAME, +0
Star_Star_L  EQU  $-Star_Star      ;AN000;LENGTH OF UNIVERSAL FILENAME, INCL NUL
SAVEFILN     DB   13 DUP(?)        ;AN000;COPY OF Current_DTA_Filename, ABOVE
;        THIS NEXT SET OF WORKSPACE DEFINES THE PATH BEING PROCESSED.
;        THESE ITEMS MUST REMAIN TOGETHER, IN THIS ORDER:
START_DRIVE  DB   0,":"            ;AN000;DRIVE LETTER NEEDS TO BE FILLED IN HERE
	     PUBLIC START_DRIVE,START_PATH ;AN000;
START_PATH   DB   PERIOD           ;AN000;AREA TO RECEIVE STARTING PATH ASCIIZ
	     DB   (MAX_PATH+SIZE Current_DTA_Filename) DUP(0) ;AN000;
JUSTIN_CASE3 DB   64 DUP(0)        ;AN000;CATCHES THE OVERFLOW
;       END OF CONTIGUOUS WORKSPACE DEFINING PATH
; =  =  =  =  =  =  =  =  =  =  =  =



	     PATHLABL TREE         ;AN013;
	     HEADER <BEGIN - VERSION CHECK, SYSMSG INIT, EXIT TO DOS> ;AN000;
; $SALUT (4,3,8,36)                ;AN000;



BEGIN PROC NEAR                    ;AN000;
  PUBLIC BEGIN                     ;AN000;
;INPUT - DOS COMMAND LINE PARMS, AS DEFINED IN MODULE PROLOG.
;         CONTROL IS PASSED HERE FROM "START" AT ORG 100H.
;        AX IS SET BY DOS TO FLAG ANY INVALID DRIVE SPECIFIED ON PARMS.
;OUTPUT - "EXITFL" HAS ERRORLEVEL RETURN CODE
; =  =  =  =  =  =  =  =  =  =  =  =

  MOV  ORIG_AX,AX                  ;AN000;SAVE ORIGINAL VALUE OF AX

;SINCE THIS IS A .COM STYLE UTILITY, THE SEG ID IN THE MSG SUBLIST
;CANNOT BE SET BY THE LOADER, BUT MUST BE SET HERE, AT RUN TIME.

  MOV  SUBLIST_VOL.SUB_VALUE_SEG,CS ;AN000;MAKE SUBLIST VARIABLE ADDRESSABLE
  MOV  SUBLIST_6A.SUB_VALUE_SEG,CS ;AN001;
  MOV  SUBLIST_6B.SUB_VALUE_SEG,CS ;AN001;
  MOV  SUBLIST_PARSE.SUB_VALUE_SEG,CS ;AN004;
  MOV  SUBLIST_INVPATH.SUB_VALUE_SEG,CS ;AN014;

  CALL SYSLOADMSG                  ;AN000; INIT SYSMSG HANDLER

; $IF  C                           ;AN000; IF THERE WAS A PROBLEM
  JNC $$IF1
      CALL SYSDISPMSG              ;AN000; LET HIM SAY WHY HE HAD A PROBLEM

      MOV  EXITFL,EXVER            ;AN000; TELL ERRORLEVEL BAD DOS VERSION
; $ELSE                            ;AN000; SINCE SYSDISPMSG IS HAPPY
  JMP SHORT $$EN1
$$IF1:
      CLD                          ;AN000;CLEAR DIRECTION FLAG TO AUTO-INCREMENT

;               GET CURRENT DRIVE ID
      DOSCALL CURRDISK             ;AN000;(19H) SET AL=0 IF A:, 1 IF B:, ETC...

      MOV  START_DR_NUM,AL         ;AN000;SAVE NUMERIC VALUE OF DOS DEFAULT DRIVE
      ADD  AL,DRIVEA               ;AN000;CONVERT DRIVE NUMBER TO LETTER
      MOV  DEFAULT_DR,AL           ;AN000;REMEMBER ALPHA OF DEFAULT DRIVE
				   ; OF FILENAME TO BE SEARCHED FOR
;                   RECORD THE INITIAL SET UP
      CALL INIT_CONDITIONS         ;AC007;SET DTA,APPEND STATUS,CAPTURE CTL-BREAK VEC

ifndef JAPAN
      CALL DEFINE_GRAPHICS         ;AN002;GET PROPER GRAPHIC CHARS FROM MSG
endif

      CALL PARSE                   ;AN000;LOOK AT DOS COMMAND LINE PARAMETERS,
				   ; AND DISPLAY ERR MSG IF BAD
;     $IF  NC                      ;AN000;PARMS ARE OK?
      JC $$IF3
;           CURIOUS ODDITY:
;           "CURRDISK"    AL=0 DRIVE A:,      AL=1 DRIVE B: ETC...
;           "GET_CUR_DIR" AL=0 DEFAULT DRIVE, AL=1 DRIVE A: ETC...
;           "SELECT_DISK" AL=0 DRIVE A:,      AL=1 DRIVE B: ETC...
;           SO... THE NUMBER WE HAVE HERE AGREES WITH "SELECT_DISK", BUT
;           WE MUST ADD ONE WHEN WE DO THE "GET_CUR_DIR".

;               GET CURRENT DIRECTORY OF TARGET DRIVE
				   ;DS:SI = POINTER TO 64 BYTE USER AREA
				   ;DL = DRIVE NUM (0=DEF, 1=A, ETC)
				   ;OUTPUT: DS:SI POINTS TO FULL PATH NAME
; "DEFAULT_PATH" WILL HAVE THE DOS DEFAULT SUBDIRECTORY PATH.

	  MOV  SI,OFFSET DEFAULT_PATH+1 ;AN000;PASS 64 BYTE AREA
	  MOV  DL,START_DRIVE      ;AN000;PASS NUMERIC VALUE
	  SUB  DL,DRIVEA           ;AN000;  OF DRIVE TO BE SCANNED
	  INC  DL                  ;AN000;SEE "CURIOUS ODDITY" ABOVE...
	  DOSCALL GET_CUR_DIR      ;AN000;(47H) GET THE CURRENT SDIR OF TARGET DRIVE

	  OR   FLAGS,F_DEF_PAT_TAR ;AN008;INDICATE DEFAULT PATH OF TARGET IS KNOWN


;                    DISPLAY FUNCTION HEADER
	  CALL GET_VOL_LABEL       ;AN000;GET VOLUME LABEL TO Current_DTA_Filename

;                    DISPLAY VOLUME SERIAL ID
	  CALL EXECUTE             ;AN000;DISPLAY THE SET OF SUBDIRS

	  CALL ANY_SUBDIRS         ;AN000;DISPLAY FINAL MSG IN CASE NO SUBDIRS

;     $ELSE                        ;AN000;SINCE PARMS HAD A PROBLEM,
      JMP SHORT $$EN3
$$IF3:
	  MOV  EXITFL,EXERR        ;AN000;SET ERROR RETURN CODE
;     $ENDIF                       ;AN000;PARMS OK?
$$EN3:
;                    RESTORE SYSTEM TO INITIAL CONDITIONS
      CALL RESTORE                 ;AN007;RETURN TO INITIAL DOS DEFAULT DRIVE,
				   ; THE INITIAL DEFAULT PATH,
				   ; AND THE INITIAL "APPEND" STATE.
; $ENDIF                           ;AN000;OK WITH SYSDISPMSG?
$$EN1:

  MOV  AL,EXITFL                   ;AN000;PASS BACK ERRORLEVEL RET CODE
  DOSCALL RET_CD_EXIT              ;AN000;(4CH) RETURN TO DOS WITH RET CODE

  INT  20H                         ;AN000; IF ABOVE NOT WORK, EXIT ANYWAY

;FOR CONTROL-BREAK, "TREE" WILL EXIT TO DOS AT "CTL_BREAK" PROC
; IN STEAD OF HERE.
BEGIN ENDP                         ;AN000;
; =  =  =  =  =  =  =  =  =  =  =  =



  HEADER <DEFINE_GRAPHICS - GET GRAPHIC CHARS FROM MSG> ;AN002;
DEFINE_GRAPHICS PROC NEAR          ;AN002;
  PUBLIC DEFINE_GRAPHICS           ;AN002;
;INPUT - MESSAGE 7 HAS THE FOUR GRAPIC CHARS, DEFINED BY THE TRANSLATORS
;        TO BE ACCEPTABLE TO THIS NATIONAL CODEPAGE.
;OUTPUT- THE "GRAF_TABLE" AREA IS Revised TO HAVE THE 4 GRAPHIC CHARS
;        AS DEFINED BY THE MESSAGE
; =  =  =  =  =  =  =  =  =  =  =  =
;               DEFINE THE GRAPHIC CHARACTERS
  MOV  AX,GRAPHIC_MSGNUM           ;AN002;REQUEST THE MESSAGE WITH GRAPHIC CHAR DEFS
  MOV  DH,UTILITY_MSG_CLASS        ;AN002;
  CALL SYSGETMSG                   ;AN002;ASK WHERE THOSE GRAPHIC CHARS ARE
				   ;IF ANY PROBLEM HERE, JUST LEAVE
				   ; THE GRAPHICS AS DEFINED AT ASSEMBLY TIME.
; $IF  NC                          ;AN002;IF ALL OK, DS:SI POINTS TO MESSAGE
  JC $$IF7
      LEA  DI,GRAF_TABLE           ;AN002;POINT TO WHERE GRAPHIC CHARS ARE TO GO
      LODSW                        ;AN002;GET FIRST PAIR OF CHARS
      STOSW                        ;AN002;SAVE THEM
      LODSW                        ;AN002;GET SECOND PAIR OF CHARS
      STOSW                        ;AN002;AND SAVE THEM ALSO
; $ENDIF                           ;AN002;
$$IF7:
  RET                              ;AN002;
DEFINE_GRAPHICS ENDP               ;AN002;
; =  =  =  =  =  =  =  =  =  =  =  =



  HEADER <PARSE - TOKENIZE THE DOS COMMAND LINE PARMS> ;AN000;
PARSE PROC NEAR                    ;AN000;
  PUBLIC PARSE                     ;AN000;
;INPUT - PSP HAS DOS COMMAND LINE PARAMETERS
;OUTPUT- CARRY IS SET IF THERE IS A PROBLEM
;        CARRY IS CLEAR IF PARMS ARE OK

;        IF THERE WERE ANY PARMS, THEY ARE MOVED FROM THE PSP
;          INTO THE STRING, "COMMAND", WHERE THE PARSER WILL LOOK AT THEM.

;        IF THE SWITCH "/F" WAS SPECIFIED, "F_SWITCH" IS SET TO "ON"
;        IF THE SWITCH IS NOT SPECIFIED, "F_SWITCH" IS LEFT "OFF".

;        IF THERE ARE NO PARMS, THE DEFAULTS OF CURRENT DRIVE AND CURRENT
;          SUBDIRECTORY ARE SET UP TO BE WHERE THE SUBDIR SEARCH WILL
;          START, AND THE "/F" SWITCH IS ASSUMED NOT SPECIFIED, SO
;          THE DEFAULT DISPLAY WILL SHOW SUBDIRS ONLY, NO FILES.

;        "START_DRIVE" EITHER HAS THE SPECIFIED STARTING DRIVE, OR
;               WILL HAVE THE CURRENT DOS DEFAULT DRIVE.
;        "START_PATH" EITHER WILL HAVE THE SPECIFIED STARTING PATH, OR
;               WILL HAVE THE CURRENT DEFAULT PATH
; =  =  =  =  =  =  =  =  =  =  =  =

  MOV  CURRENT_PARM,OFFSET COMMAND+1 ;AN000;SET POINT TO BEGINNING OF STRING
  MOV  ORDINAL,ZERO                ;AN000;START WITH FIRST PARM
  CALL PARSER                      ;AN000;INTERROGATE THE DOS COMMAND LINE PARMS
				   ;OUTPUT: SET CARRY IF PROBLEM
				   ;        CLEAR CARRY IF ALL OK
; $IF  NC                          ;AN000;IF ALL OK SO FAR WITH PARSER,
  JC $$IF9
      CMP  START_DRIVE,NUL         ;AN000;SEE IF START_DRIVE FILLED IN YET
;     $IF  E                       ;AN000;NO, NOT FILLED IN YET
      JNE $$IF10
	  MOV  AL,DEFAULT_DR       ;AN000;GET ALPHA LETTER OF DEFAULT DRIVE
	  MOV  START_DRIVE,AL      ;AN000;SET WHERE TO SEARCH FOR SUBDIRS
	  CLC                      ;AN000;NO ERROR SO FAR
;     $ELSE                        ;AN000;SINCE START_DRIVE WAS SPECIFIED
      JMP SHORT $$EN10
$$IF10:
	  CALL VERIFY_DRIVE        ;AN000;SEE IF USER SPECIFIED DRIVE IS OK, AND
				   ; IF SO, CHANGE DOS DEFAULT DRIVE TO IT
				   ;CARRY WILL BE SET IF ERROR

				   ;IF A NEW DRIVE WAS SPECIFIED,
				   ; DEFAULT DRIVE HAS BEEN CHANGED TO
				   ; NEW DEFAULT DRIVE, USER SPECIFIED

;     $ENDIF                       ;AN000;FILLED IN START_DRIVE YET?
$$EN10:
;     $IF  NC                      ;AN000;IF ALL OK SO FAR,
      JC $$IF13
	  CMP  START_PATH,NUL      ;AN000;SEE IF START_PATH FILLED IN YET
;         $IF  E                   ;AN000;NO, NOT FILLED IN YET,
	  JNE $$IF14
	      MOV  DI,OFFSET START_PATH ;AN000;SET WHERE TO PUT STARTING PATH
	      MOV  AL,BACK_SLASH   ;AN000;START CURRENT SUBDIR AT ROOT
	      STOSB                ;AN000;  SO START WITH BACK SLASH

				   ;DI POINTS TO BYTE AFTER BACK SLASH
				   ; JUST ADDED TO "START_PATH"

	      MOV  SI,DI           ;AN000;DS:SI = POINTER TO 64 BYTE USER AREA
	      MOV  DL,DEFDRIVE     ;AN000;DL = DRIVE NUM (0=DEF, 1=A, ETC)
	      DOSCALL GET_CUR_DIR  ;AN000;(47H) GET CURRENT DIRECTORY
				   ;OUTPUT: DS:SI POINTS TO FULL PATH NAME
;         $ENDIF                   ;AN000;START_PATH FILLED IN YET?
$$IF14:
	  CLC                      ;AN000;INDICATE NO PROBLEM WITH PARMS
;     $ENDIF                       ;AN000;ALL OK SO FAR?
$$IF13:
; $ENDIF                           ;AN000;ALL OK WITH PARSER?
$$IF9:
  RET                              ;AN000;RETURN TO CALLER
PARSE ENDP                         ;AN000;
; =  =  =  =  =  =  =  =  =  =  =  =



  HEADER <VERIFY_DRIVE - CHECK IF USER DRIVE ID IS OK> ;AN000;
VERIFY_DRIVE PROC NEAR             ;AN000;
  PUBLIC VERIFY_DRIVE              ;AN000;
;THE USER HAS SPECIFIED A DRIVE LETTER.  VERIFY IT IS A PROPER DRIVE.
;IF THE DRIVE LETTER SPECIFIED IS THE SAME AS THE DEFAULT DRIVE, IT IS OK.
;IF DIFFERENT, ADDITIONAL VERIFICATION TESTS MUST BE MADE.
;THIS TEST IS DONE BY TRYING TO CHANGE THE CURRENT DRIVE TO THE SPECIFIED
; DRIVE, THEN BY ASKING WHAT IS THE CURRENT DRIVE.  IF THE CURRENT DRIVE
; HAS CHANGED FROM WHAT IT WAS ORIGINALLY, THEN THE NEW DRIVE LETTER IS OK.
; IF IT DID NOT CHANGE, THEN IT WAS A BOGUS DRIVE LETTER AND WE QUIT.

;INPUT: "START_DRIVE" - USER SPECIFIED DRIVE LETTER TO BE TESTED
;       "DEFAULT_DR" - ORIGINAL DOS DEFAULT DRIVE
;       "START_DR_NUM" - NUMERIC EQUIVALENT OF THE ORIGINAL DOS DEFAULT DRIVE
;       "ORIG_AX" - HAS FLAGS TO VERIFY DRIVE, SET BY DOS AT LOAD TIME.
;OUTPUT: CARRY SET IF BAD, CARRY CLEAR IF OK
;       IF BAD, ERROR MESSAGE IS DISPLAYED: "INVALID DRIVE SPECIFICATION"
; =  =  =  =  =  =  =  =  =  =  =  =
  MOV  DL,START_DRIVE              ;AN000;USING THE DRIVE SPECIFIED IN PARMS,
  CMP  DL,DEFAULT_DR               ;AN000;DID PARMS SPECIFY DRIVE SAME AS DEFAULT?
; $IF  NE                          ;AN000;IF DRIVE SPECIFIED IS DIFFERENT
  JE $$IF18
      MOV  AX,ORIG_AX              ;AN000;GET DRIVE VERIFICATION FLAGS, SAVED FROM AX
      OR   AL,AH                   ;AN000;COMBINE FLAGS FOR BOTH DRIVE ID'S, IF GIVEN
;     $IF  NZ,OR                   ;AN000;IF THERE IS A PROBLEM, OR...
      JNZ $$LL19

      SUB  DL,DRIVEA               ;AN000;CONVERT DRIVE LETTER TO DRIVE NUMBER
				   ; DL=DRIVE NUMBER (0=A,1=B)
      DOSCALL SELECT_DISK          ;AN000;(0EH) SET DEFAULT DRIVE
				   ;OUTPUT: AL=NUM. OF DRIVES (MIN 5) (NOT USED);AN000;
				   ; (NOT INTERESTED...)
      DOSCALL CURRDISK             ;AN000;(19H) GET CURRENT DEFAULT DRIVE
				   ;OUTPUT: AL = CURRENT DRIVE
				   ;  0=A,1=B,ETC.
      CMP  AL,START_DR_NUM         ;AN000;HAS THE ORIGINAL DOS DEFAULT DRIVE CHANGED?
      CLC                          ;AN000;NO ERROR
;     $IF  E                       ;AN000;IF NO CHANGE, THEN USER SPECIFIED
      JNE $$IF19
$$LL19:
				   ; INVALID DRIVE
	  MOV  MSGNUM_EXTERR,INVDRSPEC ;AN000;"INVALID DRIVE SPECIFICATION"
	  MOV  DI,OFFSET MSGNUM_EXTERR ;AN000;
	  CALL SENDMSG             ;AN000;TELL USER HE SAID BAD DRIVE LETTER

	  STC                      ;AN000;RETURN AN ERROR
;     $ENDIF                       ;AN000;NO CHANGE?
$$IF19:
; $ELSE                            ;AN000;SINCE DRIVE SPECIFIED IS THE SAME
  JMP SHORT $$EN18
$$IF18:
      CLC                          ;AN000;NO ERROR
; $ENDIF                           ;AN000;NEW DRIVE SPECIFIED?
$$EN18:
  RET                              ;AN000;RETURN TO CALLER
VERIFY_DRIVE ENDP                  ;AN000;
; =  =  =  =  =  =  =  =  =  =  =  =



  HEADER <INIT_CONDITIONS - GET INITIAL SUBDIR, APPEND,CTL_BREAK> ;AN000;
INIT_CONDITIONS PROC NEAR          ;AN000;
  PUBLIC INIT_CONDITIONS           ;AN000; MAKE ENTRY IN LINK MAP
;INPUT  - "START_DR_NUM" WILL HAVE THE NUMERIC VALUE OF DOS DEFAULT DRIVE.
;       - "DEFAULT_DR" WILL HAVE THE LETTER DRIVE ID OF DOS DEFAULT DRIVE.
;OUTPUT - "APPEND_FLAGS" RECORDS ORIGINAL STATUS OF /X OF APPEND.
;               APPEND IS COMMANDED TO HALT ITS /X PROCESSING.
;         CONTROL BREAK VECTOR IS ALTERED TO POINT TO MY HANDLER.
; =  =  =  =  =  =  =  =  =  =  =  =
;               SET UP THE LOCAL DTA
  MOV  DX,OFFSET Current_DTA       ;AN000;PASS POINTER TO DTA BUFFER
  DOSCALL SET_DTA                  ;AN000;(1AH) SET DTA FOR FIND FIRST/NEXT
;               GET CURRENT APPEND STATUS
  MOV  AX,APPEND_CHECK             ;AN006;SEE IF APPEND IS ACTIVE
  INT  2FH                         ;AN006;CALL THE MULTIPLEXOR FUNCTION

  OR   AL,AL                       ;AN006;TEST THE RESULTS
; $IF  NZ,AND                      ;AN006;IF INSTALLED
  JZ $$IF23

  MOV  AX,APPEND_VERSION           ;AN006;ASK IF DOS VERSION OF APPEND
  INT  2FH                         ;AN006;CALL THE MULTIPLEXOR FUNCTION
  CMP  AX,DOS_APPEND_VER           ;AN006;IS THIS THE DOS VERSION OF APPEND
; $IF  E                           ;AN006;YES, DEAL WITH THIS VERSION
  JNE $$IF23
      OR   FLAGS,F_APPEND          ;AN000;FLAG IT AS THE DOS VERSION
      MOV  AX,GET_APPEND           ;AN000;
      INT  2FH                     ;AN000;READ STATUS OF /X FROM APPEND
				   ;OUTPUT-BX=(SEE "APPEND_FLAGS" FOR DEFINITION
      MOV  APPEND_FLAGS,BX         ;AC006;REMEMBER APPEND STATUS
; $ENDIF                           ;AN000;APPEND INSTALLED?
$$IF23:
;               CAPTURE THE CRITICAL ERROR VECTOR
  PUSH ES                          ;AN000;SAVE SEGREG
				   ;AL = INTERRUPT NUMBER
  DOSCALL GET_VECTOR,VEC_CRITERR   ;AN000;(3524H) GET INTERRUPT VECTOR
				   ;OUTPUT: ES:BX = CONTENTS OF VECTOR
  MOV  WORD PTR OLDINT24,BX        ;AN000;SAVE THE ORIGINAL
  MOV  WORD PTR OLDINT24+WORD,ES   ;AN000; CRITICAL ERROR HANDLER VECTOR
  POP  ES                          ;AN000;RESTORE SEGREG

  MOV  DX,OFFSET MYERRORHANDLER    ;AN000;DS:DX = VECTOR TO INT HANDLER
				   ;AL = INTERRUPT NUMBER
  DOSCALL SET_VECTOR,VEC_CRITERR   ;AN000;(25H) SET INTERRUPT VECTOR

;               CAPTURE THE CONTROL BREAK VECTOR
  PUSH ES                          ;AN000;SAVE SEGREG
				   ;AL = INTERRUPT NUMBER
  DOSCALL GET_VECTOR,VEC_CTLBREAK  ;AN000;(3523H) GET INTERRUPT VECTOR
				   ;OUTPUT: ES:BX = CONTENTS OF VECTOR
  MOV  WORD PTR OLDINT23,BX        ;AN000;SAVE THE ORIGINAL
  MOV  WORD PTR OLDINT23+WORD,ES   ;AN000; CTRL-BREAK VECTOR
  POP  ES                          ;AN000;RESTORE SEGREG

  MOV  DX,OFFSET BREAK_HANDLER     ;AN000;DS:DX = VECTOR TO INT HANDLER
				   ;AL = INTERRUPT NUMBER
  DOSCALL SET_VECTOR,VEC_CTLBREAK  ;AN000;(25H) SET INTERRUPT VECTOR

;               STOP THE APPEND FUNCTION.
  MOV  AX,SET_APPEND               ;AN000;CHANGE APPEND /X STATUS
  XOR  BX,BX                       ;AN000;REQUEST TERMINATION OF /X SUPPORT OF APPEND
  INT  2FH                         ;AN000;SET IT

  RET                              ;AN000;RETURN TO CALLER
INIT_CONDITIONS ENDP               ;AN000;
; =  =  =  =  =  =  =  =  =  =  =  =



  HEADER <GET_VOL_LABEL - GET VOLUME LABEL ON SPECIFIED DRIVE> ;AN000;
GET_VOL_LABEL PROC NEAR            ;AN000;
  PUBLIC GET_VOL_LABEL             ;AN000;MAKE ENTRY IN LINK MAP
;INPUT - "START_PATH" IS ASCIIZ OF STARTING PATH
;OUTPUT - "Current_DTA_Filename" WILL HAVE ASCIIZ STRING OF VOLUME LABEL.
;         STARTING DRIVE AND PATH TO SPECIFIED SUBDIR IS DISPLAYED.
; =  =  =  =  =  =  =  =  =  =  =  =

	push    ax

	mov     al,  EXTENDED_FCB  ; EXTENDED_FCB = FFH
	mov     Current_DTA, al   ; set byte 0 to FFH
	mov     al,  attr_volid
	mov     Current_DTA + 6, al   ; set attribute byte to VOLUME
	mov     al,  start_drive   ; user specified drive.  if bad, program
				   ; should have quit by now
	sub     al,  40h           ; start_drive has ascii drive value.
				   ; convert to numeric
	mov     tree_fcb.fcb_drive,  al

; fill in the filename fields in FCB with wildcard character ?
	push    cx
	push    bx
	mov     al,  QUESTION_MARK ; QUESTION_MARK = 3FH
	mov     bx,  1
	mov     cx,  PATH_LEN      ; PATH_LEN = 11, path length in FCB
wldcrd: mov     tree_fcb.fcb_drive[bx],  al
	inc     bx
	loop    wldcrd
	pop     bx
	pop     cx

; do the int 21 - FCB-based Find First
	mov     dx,  OFFSET Current_DTA
	doscall fcb_findfirst
	or      al,  al           ; AL == 0 means found the file
	pop     ax
	jz      ffsuccess

	stc                                 ; if didn't find, set carry flag
	 mov  di, OFFSET MSGNUM_LIST   ;AN000; "Directory PATH listing"
	 call SENDMSG                   ;AN000;DISPLAY STARTING MESSAGE

	jmp     short  gvl_error

; copy the volume label into Current_DTA_Filename
ffsuccess:
	mov     si,  OFFSET tree_fcb.fcb_drive
	inc     si
	mov     di,  OFFSET Current_DTA_Filename
	mov     cx,  PATH_LEN
	rep     movsb

	mov     di,  OFFSET msgnum_vol
	 CALL SENDMSG                      ;AN000;DISPLAY STARTING MESSAGE

gvl_error:
  CALL VOLSER                      ;AN001;DISPLAY VOLUME SERIAL NUMBER, IF ANY

;               DISPLAY THE STARTING DRIVE AND SUBDIRECTORY
  MOV  DX,OFFSET START_DRIVE       ;AN000;PASS POINTER TO STRING TO BE DISPLAYED
  CALL LEN_ASCIIZ                  ;AN000;SETS CX = NUMBER OF BYTES TO WRITE

				        ;DS:DX = ADDRESS OF DATA TO WRITE
  CALL DO_WRITE                    ;AN000;DISPLAY STARTING SUBDIR TO STDOUT

  mov  dx, offset CRLF
  mov  cx, LEN_CRLF
  call Do_Write                    ; terminate line with CR / LF

; now do handle-based Findfirst so that DTA has correct information for the
; Findnext
  MOV  CX,ATTR_VOLID               ;AN000;REQUEST THE VOLUME ID
  MOV  DX,OFFSET Star_Star         ;AN000;PASS FILENAME TO BE LOOKED FOR
  DOSCALL FINDFIRST                ;AN000;LOOK FOR VOLUME LABEL

  RET                              ;AN000;RETURN TO CALLER
GET_VOL_LABEL ENDP                 ;AN000;
; =  =  =  =  =  =  =  =  =  =  =  =



  HEADER <VOLSER - DISPLAY VOLUME SERIAL NUMBER, IF ANY> ;AN001;
VOLSER PROC NEAR                   ;AN001;
  PUBLIC VOLSER                    ;AN001;
;IF THE MEDIA SUPPORTS A VOL SERIAL NUMBER, DISPLAY IT
; =  =  =  =  =  =  =  =  =  =  =  =
;              ISSUE GET MEDIA ID
  MOV  BH,ZERO                     ;AN001;BH=0, RES
  MOV  BL,START_DRIVE              ;AN001;GET LETTER OF DRIVE BEING LOOKED AT
  SUB  BL,DRIVEA-1                 ;AN001;(BACK UP 40H) BL=DRIVE NUM (1=A:, 2=B:, ETC)
  MOV  DX,OFFSET MEDIA_ID_BUF      ;AN001;DS:DX=BUFFER (see A_MEDIA_ID_INFO STRUC)
  DOSCALL GSET_MEDIA_ID,GET_ID     ;AC010;(6900H) GET MEDIA ID
				   ;CARRY SET ON ERROR (OLD STYLE BOOT RECORD)

; $IF  NC                          ;AN001;IF THE GET MEDIA ID WORKED OK,
  JC $$IF28

; NOTE: IN THE FOLLOWING TWO SUBLISTS, WE ARE GOING TO DISPLAY, IN HEX,
; A CONSECUTIVE SET OF 4 BYTES, THE VOLUME SERIAL NUMBER.  THE ORDER OF
; THESE TWO WORDS OF HEX IS, LEAST SIGNIFICANT WORD FIRST, THEN THE
; MOST SIGNIFICANT WORD.  WHEN DISPLAYED, THE MOST SIGNIFICANT IS TO BE
; DISPLAYED FIRST, SO THE VALUE AT SERIAL+2 GOES TO THE 6A SUBLIST,
; AND THE LEAST SIGNIFICANT VALUE AT SERIAL+0 GOES TO THE SECOND POSITION,
; REPRESENTED BY THE 6B SUBLIST.

      LEA  AX,MEDIA_ID_BUF.MI_SERIAL ;AN001;GET POINTER TO DATA TO BE PRINTED
      MOV  SUBLIST_6B.SUB_VALUE,AX ;AN001; INTO THE SUBLIST FOR %2

      LEA  AX,MEDIA_ID_BUF.MI_SERIAL+WORD ;AN001;GET POINTER TO DATA TO BE PRINTED
      MOV  SUBLIST_6A.SUB_VALUE,AX ;AN001; INTO THE SUBLIST FOR %1

				   ;"Volume Serial Number is %1-%2"
      MOV  DI,OFFSET MSGNUM_SERNO  ;AN001;DISPLAY THE NEW SERIAL NUMBER
      CALL SENDMSG                 ;AN001;DISPLAY THE MESSAGE

; $ENDIF                           ;AN001;IS VOL SERIAL NUM PRESENT?
$$IF28:
  RET                              ;AN001;RETURN TO CALLER
VOLSER ENDP                        ;AN001;
; =  =  =  =  =  =  =  =  =  =  =  =



  HEADER <LEN_ASCIIZ - GET LENGTH OF ASCIIZ STRING> ;AN000;
LEN_ASCIIZ PROC NEAR               ;AN000;
  PUBLIC LEN_ASCIIZ                ;AN000;MAKE ENTRY IN LINK MAP
;INPUT - ES:DX = POINTS TO START OF ASCIIZ STRING
;OUTPUT - CX = LENGTH OF CHARACTERS, NOT INCLUDING THE NUL AT THE END
; =  =  =  =  =  =  =  =  =  =  =  =

  PUSH AX                          ;AN000;SAVE THE CALLER'S
  PUSH DI                          ;AN000; REGISTERS
  MOV  CX,FULL_SEG_SIZE            ;AN000;BETTER FIND THAT NUL SOMEWHERE...
  MOV  DI,DX                       ;AN000;SET INDEX TO WALK THRU THE STRING
  MOV  AL,NUL                      ;AN000;THIS IS THE CHAR I AM LOOKING FOR
  REPNE SCASB                      ;AN000;LOOK FOR IT

  SUB  DI,DX                       ;AN000;TAKE AWAY WHERE WE STARTED, FROM WHERE WE AT
  MOV  CX,DI                       ;AN000; TO FIND NOW FAR WE MOVED
  dec  cx                          ; don't add nul
  POP  DI                          ;AN000;RESTORE THE CALLER'S
  POP  AX                          ;AN000; REGISTERS

  RET                              ;AN000;RETURN TO CALLER
LEN_ASCIIZ ENDP                    ;AN000;
; =  =  =  =  =  =  =  =  =  =  =  =



  HEADER <EXECUTE - LOOK THRU DIRECTORY LIST FOR SUBDIRS> ;AN000;
EXECUTE PROC NEAR                  ;AN000;
  PUBLIC EXECUTE                   ;AN000;MAKE ENTRY IN LINK MAP
;BECAUSE OF THE RECURSIVE NATURE OF THIS ROUTINE, ALL ITS LOCAL WORKAREA
;MUST BE DYNAMICALLY ALLOCATED BY USING A PORTION OF THE STACK.  AS EACH
;LOWER LEVEL OF STACK IS PROCESSED, A NEW CALL IS MADE TO THIS SUBROUTINE
;WHICH THEN CREATES A NEW WORKAREA FOR THAT SUBDIRECTORY.
;THE CURRENT STACK SIZE IS CHECKED TO SEE IF THERE IS ENOUGH ROOM FOR
;THE NEW STACK WORKAREA.
;INPUT:START_PATH - STRING OF PATHNAME OF PATH TO BE PROCESSED
;OUTPUT: WHEN THIS PROC RETURNS, ALL FILES IN THIS SUBDIR AND LOWER
;       LEVELS OF SUBDIRS HAVE BEEN PROCESSED.
; =  =  =  =  =  =  =  =  =  =  =  =

  PUSH BP                          ;SAVE CALLER'S BP REG

  SUB  SP,WA_SIZE                  ;ALLOCATE STACK SPACE AS WORKAREA

  MOV  BP,SP                       ;SET BASE FOR WORKAREA CALLED "FRAME"

  MOV  AX,BP                       ;
  SUB  AX,OFFSET LAST_BYTE         ;WHERE MY CODE ENDS
  CMP  AX,MIN_STACK                ;IS THE MINIMUM STACK REMAINING?

  jb   $$IF30

Check_For_Root:
  CMP  START_PATH,BACK_SLASH       ;WAS A BACKSLASH SDIR SPECIFIED,
  JE $$IF31                        ; IF SO, SKIP THIS

  MOV  DX,OFFSET START_PATH        ;POINT TO SPECIFIED PATH
  DOSCALL CHDIR                    ;(3BH) CHANGE CURRENT DIR
  JC $$IF32

  MOV  DL,START_DRIVE              ;GET TARGET DRIVE
  SUB  DL,DRIVEA-1                 ;CONVERT TO NUM (A=1,B=2,ETC.)
  LEA  SI,[BP].FRAM_CURR_PATH+1    ;WHERE TO PUT PATH
  MOV  [BP].FRAM_CURR_PATH,BACK_SLASH 
  DOSCALL GET_CUR_DIR              ;FIND WHERE WE ARE NOW

  LEA  SI,[BP].FRAM_CURR_PATH      ;WHERE PATH WENT, WITH BACKSLASH
  MOV  DI,OFFSET START_PATH        ;WHERE TO PUT IT
  MOV  CX,MAX_PATH+1               ;MOVE FULL LENGTH PLUS BACKSLASH
  REP  MOVSB                       ; TO START_PATH

$$IF32:
  JMP SHORT $$EN31

$$IF31:
  MOV  SI,OFFSET START_PATH        ;USING THE STARTING PATH,
  LEA  DI,[BP].FRAM_CURR_PATH      ;SAVE IT IN THE STACK WORKAREA
				   ;(at times like this, sure is nice to
				   ; have ES=SS.  .EXE would be a problem..)
  MOV  CX,MAX_PATH+1               ;MOVE THE ENTIRE STARTING PATH+LEADING "\"
  REP  MOVSB                       ;INTO THE WORKAREA

  MOV  DX,OFFSET START_PATH        ;POINT TO SPECIFIED PATH
  DOSCALL CHDIR                    ;(3BH) CHANGE CURRENT SDIR TO SPECIFIED SDIR

$$EN31:
  JC $$IF36

  call Begin_Find
  ; Save Error state for FIND_TYPE_DIR 
  pushf

  TEST FLAGS,F_SWITCH      ;ARE ALL FILES ASKED FOR?
				   ; (TEST WILL CLEAR CARRY FLAG)
  JZ $$IF37                        ;IS /F SET?

  CALL FIND_TYPE_NORMAL            ;DISPLAY ALL THE FILENAMES

$$IF37:
  OR   FLAGS,F_FLN                 ; Allow buffer display in Find_Type_Dir

  call Find_Next
 
  ; Restore carry out value from Begin_Find for Find_Type_Dir.
  popf

  CALL FIND_TYPE_DIR               ;DISPLAY ALL THE DIRECTORIES

  JMP SHORT $$EN36

$$IF36:
  MOV  DI,OFFSET MSGNUM_INVPATH    ;"INVALID PATH"
  CALL SENDMSG                     ;SAY WHY I QUIT

  MOV  EXITFL,EXERR                ;SET ERROR FLAG TO QUIT

$$EN36:
				   ;FINISHED WITH THIS SUBDIRECTORY, SO
  JMP SHORT $$EN30                 ;SINCE STACK TOO SMALL

$$IF30:
  MOV  DI,OFFSET MSGNUM_EXTERR     ;DESCRIPTOR FOR EXTENDED ERRORS
  MOV  [DI].MSG_NUM,INSUF_MEM      ;"INSUFFICIENT MEMORY"
  CALL SENDMSG             
  MOV  EXITFL,EXERR                ;SET ERRORLEVEL RET CODE

$$EN30:
  ADD  SP,WA_SIZE                  ;DISCARD WORKAREA

  POP  BP                          ;RESTORE CALLER'S BP REG
  RET                              ;RETURN TO CALLER
EXECUTE ENDP                       ;
; =  =  =  =  =  =  =  =  =  =  =  =



  HEADER <ANY_MORE_SUBDIR - LOOK AHEAD,SEE IF MORE SUBDIR> ;AN000;
ANY_MORE_SUBDIR PROC               ;AN000;
;HAVING JUST DONE A FIND FIRST/NEXT FOR A SUBDIRECTORY,LOOK FOR ANOTHER
;INPUT: BP = DYNAMIC WORKAREA
;OUTPUT: "FRAM_CHAR"="ELBO" = NO MORE SUBDIRS AFTER THIS ONE.
;        "FRAM_CHAR"="TEE"  = ANOTHER SUBDIR AFTER THIS ONE
; =  =  =  =  =  =  =  =  =  =  =  =
;WARNING! The function should be called only once per Find_Next!

; We can set Fram_Char here simply based on the carry out value from the last
; DOS FindNext call, which is stored in the F_Carry bit of Flags.

  test  Flags, F_Carry
  jnz   Elbow

Tee:
  mov   al, Graf_Tee
  mov   [bp].Fram_Char, al       ; store tee as frame character
  jmp   short AMS_Exit

Elbow:
  mov   al, Graf_Elbow
  mov   [bp].Fram_Char, al       ; store elbow as frame character

AMS_Exit:
  ret

ANY_MORE_SUBDIR ENDP               ;AN000;
; =  =  =  =  =  =  =  =  =  =  =  =



  HEADER <FIND_TYPE_NORMAL - PROCESS NORMAL, NON-DIR, FILES> ;AN000;
FIND_TYPE_NORMAL PROC NEAR         ;AN000;
  PUBLIC FIND_TYPE_NORMAL          ;AN000;MAKE ENTRY IN LINK MAP
;INPUT - BP=BASE OF FRAME, DYNAMIC WORKAREA WITH DTA
;OUTPUT - CY SET IF A PROBLEM, AND "EXITFL" HAS ERROR CODE
;         CY CLEAR IF NORMAL "NO MORE FILES" FROM FIND FIRST/NEXT.
; =  =  =  =  =  =  =  =  =  =  =  =

; Turn off display name flag until we have a filename to print.
  AND   FLAGS, 0ffh - F_FLN

; Set leading graphics character for files in this directory..
  call Any_More_Subdir           ; character into [BP].Fram_Char

  mov  dx, offset File_DTA
  DOSCall Set_DTA                ; set DTA as File_DTA

  mov  cx, attr_normal
  mov  dx, offset Star_Star
  DOSCall FindFirst              ; FindFirst using normal file attribute
  jc   FF_Exit                   
  or   Flags, F_FLN              ; permit display of filename

FF_Loop:
  mov  dx, offset File_DTA       ; set ds:dx to File_DTA for Show_FN
  mov  al, File_DTA_Attr
  call Show_FN

  DOSCall FindNext
  jnc   FF_Loop

FF_Exit:
  CALL IF_NOMOREFILES      ;AN000;SEE IF REASON FOR ERROR IS NO MORE FILES
				   ;CY NOT SET = "NO MORE FILES"
				   ;CY IS SET = OTHER PROBLEM, "EXITFL" SET
  JC   FTN_Exit

  mov  File_DTA_Filename, NUL
  mov  dx, offset File_DTA       ; set ds:dx to File_DTA for Show_FN
  MOV  AL,ATTR_NORMAL            ;AN000;SAY IT IS JUST A FILENAME
  CALL SHOW_FN

FTN_Exit:
  mov  dx, offset Next_DTA
  DOSCall Set_DTA                      ; set DTA back to Next_DTA

  ret

FIND_TYPE_NORMAL ENDP
; =  =  =  =  =  =  =  =  =  =  =  =



  HEADER <FIND_TYPE_DIR - PROCESS THE DIRECTORY> ;AN000;
FIND_TYPE_DIR PROC NEAR            ;AN000;
  PUBLIC FIND_TYPE_DIR             ;AN000;MAKE ENTRY IN LINK MAP
;INPUT - BP=BASE OF FRAME, DYNAMIC WORKAREA WITH DTA
;OUTPUT - CY SET IF A PROBLEM
;         CY CLEAR IF NORMAL "NO MORE FILES" FROM FIND FIRST/NEXT.
; =  =  =  =  =  =  =  =  =  =  =  =

Check_File:
  jc   Check_Error

  mov  al, Current_DTA_Attr            ; Is this really a directory?
  and  al, Attr_Dir
  jz   Not_Subdir

  CMP  Current_DTA_Filename,PERIOD      ;AN000;WAS THAT A "PERIOD" FILENAME?
  JE   Dot_Name

	 CALL ANY_MORE_SUBDIR          ;SEE IF MORE SUBDIRS BELOW THIS ONE
	 ;SETS "FRAM_CHAR" TO:
	 ;  "ELBO" - NO MORE DIR BELOW THIS ONE
	 ;  "TEE" - THERE IS ANOTHER DIR BELOW HERE

	 mov  dx, offset Current_DTA   ; set up ds:dx for Show_FN
	 mov  al, Current_DTA_Attr

	 CALL SHOW_FN      ;AN000;SHOW THIS FILE JUST FOUND

	 ADD  CURRENT_COL,DASH_NUM+1 ;AN000;LOCATE SDIR NAME IN PRINTOUT LINE

	 CALL NEXT_LEVEL           ;AN000;SHIFT TO LOWER LEVEL SUBDIRECTORY

	 Push_Frame

	 CALL EXECUTE      ;AN000;RECURSIVE CALL, PROCESS NEW LEVEL OF SUBDIR

	 Pop_Frame            ; Restore values for this frame.

	 SUB  CURRENT_COL,DASH_NUM+1 ;AN000;BACK TO ORIGINAL LEVEL
	 LEA  DX,[BP].FRAM_CURR_PATH ;AN000;DS:DX = POINTER TO ASCIIZ STRING
	 DOSCALL CHDIR     ;AN000;(3BH) CHANGE CURRENT DIRECTORY
	 ;GO BACK TO THE DIRECTORY THIS LEVEL OF
	 ; STACK WORKAREA HAS BEEN DEALING WITH,
	 ; SINCE "EXECUTE" HAD CHANGED IT TO WORK ON
	 ; A LOWER SUBDIRECTORY.

Not_Subdir:
Dot_Name:
  CMP  EXITFL,EXOK                 ;AN000;ANY ERRORS SO FAR?
  JNE  FTD_Exit

; There are more subdirectories to display; get the next one.
  CALL FIND_NEXT
  JMP  Check_File

Check_Error:
  CMP  EXITFL,EXOK                 ;AN000;IF NO ERROR FOUND SO FAR
  JNE  FTD_Exit
  CALL IF_NOMOREFILES      ;AN000;SEE IF REASON FOR ERROR IS NO MORE FILES
  ;CY NOT SET = "NO MORE FILES"
  ;CY IS SET = OTHER PROBLEM, "EXITFL" SET

FTD_Exit:
  ret

FIND_TYPE_DIR ENDP
; =  =  =  =  =  =  =  =  =  =  =  =



  HEADER <NEXT_LEVEL - SET UP TO LOOK AT LOWER LEVEL SUBDIR> ;AN000;
NEXT_LEVEL PROC NEAR               ;AN000;
  PUBLIC NEXT_LEVEL                ;AN000;
;INPUT: Current_DTA_Filename - FILE NAME OF LOWER LEVEL SUBDIR
;OUTPUT: START_PATH - HAS COMPLETE PATH TO THE NEW LEVEL SUBDIR
; =  =  =  =  =  =  =  =  =  =  =  =

  MOV  SI,OFFSET START_PATH+1   ;AN000;WHERE TO SAVE CURRENT PATH
  MOV  DL,DEFDRIVE                 ;AN000;DL = DRIVE NUM (0=DEF, 1=A, ETC)
  DOSCALL GET_CUR_DIR              ;AN000;(47H) GET CURRENT DIRECTORY
				   ;OUTPUT: DS:SI POINTS TO FULL PATH NAME
  DEC  SI                          ;AN012;START SCAN AT START_PATH
  CALL SCAN_DBCS                   ;AN012;GET LAST 2 CHARS IN DL,DH
				   ; SI NOW AT END OF STRING
  MOV  DI,SI                       ;AN012;GET POINTER TO NUL
  CMP  DL,BACK_SLASH               ;AN000;
; $IF  NE                          ;AN012;IF PATH NOT ALREADY TERMINATED WITH A "\"
  JE $$IF70
      MOV  BYTE PTR [DI],BACK_SLASH ;AN000;TERMINATE PREVIOUS PATH
      INC  DI                      ;AN000;DI POINTS TO NEXT NUL AT END OF STRING
; $ENDIF                           ;AN000;END IN "\"?
$$IF70:
  MOV  SI,OFFSET Current_DTA_Filename      ;AN000;GET NAME OF NEW SUBDIR
  MOV  CX,LENGTH Current_DTA_Filename      ;AN000;
  REP  MOVSB                       ;AN000;ADD THE NEW SUBDIR TO END

  RET                              ;AN000;RETURN TO CALLER
NEXT_LEVEL ENDP                    ;AN000;
; =  =  =  =  =  =  =  =  =  =  =  =



  HEADER <SCAN_DBCS - FIND LAST 2 SBCS CHARS IN ASCIIZ> ;AN000;
SCAN_DBCS PROC NEAR                ;AN000;
  PUBLIC SCAN_DBCS                 ;AN000;
;INPUT: DS:SI = ASCIIZ STRING TO BE SCANNED
;OUTPUT: DL=LAST SBCS CHAR BEFORE NUL
;        DH=NEXT TO LAST SBCS CHAR BEFORE NUL
;        IF NO SBCS CHAR FOUND, DL OR DH WILL BE NUL
;        SI=OFFSET TO NUL DELIMITER

  XOR  DX,DX                       ;AN000;CLEAR CHAR ACCUMULATOR
; $DO                              ;AN000;
$$DO72:
      LODSB                        ;AN000;GET NEXT CHAR FROM DS:SI TO AL
      CMP  AL,NUL                  ;AN000;IS THAT THE DELIMITER?
; $LEAVE E                         ;AN000;FOUND THE END, SO QUIT
  JE $$EN72
      CALL CHK_DBCS                ;AN000;IS THIS THE FIRST OF A DBCS PAIR?

;     $IF  C                       ;AN000;IF SO, FOUND A DBCS PAIR
      JNC $$IF74
	  INC  SI                  ;AN000;SKIP ITS PARTNER
	  MOV  AL,NUL              ;AN000;PASS BACK A NUL, INSTEAD OF AN SBCS CHAR
;     $ENDIF                       ;AN000;
$$IF74:
      MOV  DH,DL                   ;AN000;SAVE PREVIOUS CHAR
      MOV  DL,AL                   ;AN000;REMEMBER THE CHAR JUST FOUND
; $ENDDO                           ;AN000;
  JMP SHORT $$DO72
$$EN72:
  DEC  SI                          ;AN000;LODSB SET SI ONE BEYOND NUL
				   ; SO SET SI BACK TO POINT TO THE NUL
  RET                              ;AN000;RETURN TO CALLER
SCAN_DBCS ENDP                     ;AN000;
; =  =  =  =  =  =  =  =  =  =  =  =



  HEADER <BEGIN_FIND - DO FIND FIRST FILE> ;AN000;
BEGIN_FIND PROC NEAR               ;AN000;
  PUBLIC BEGIN_FIND                ;AN000;MAKE ENTRY IN LINK MAP
;INPUT - CURRENT DOS DEFAULT DRIVE HAS TARGET DRIVE TO BE RESEARCHED.
;        CURRENT DEFAULT SUBDIRECTORY HAS SUBDIR TO BE RESEARCHED.
;       BP = OFFSET OF DYNAMIC WORKAREA
;OUTPUT - DTA IS SET UP WITH FIRST FILE FOUND, READY TO BE USED BY FINDNEXT.
;        WORKAREA HAS SAVED THE RESULT OF FINDFIRST.
; =  =  =  =  =  =  =  =  =  =  =  =

  mov  dx, offset Next_DTA
  DOSCall Set_DTA                ; set DTA as Next_DTA

  mov  cx, attr_dir
  mov  dx, offset Star_Star
  DOSCall FindFirst              ; FindFirst using directory attribute
  jmp  short Skip_FN

FindFirst_Loop:
  DOSCall FindNext

Skip_FN:
  Set_F_Carry
  jc   BF_Exit

  ; Make sure we have found a directory here.  If not, keep doing FindNexts
  ; until we find one or run out of files.
  mov  al, Next_DTA_Attr
  and  al, attr_dir
  jz   FindFirst_Loop                 ; Only found a file, look again.

  cmp  byte ptr Next_DTA_Filename,Period ;Now check for "." (and "..")
  je   FindFirst_Loop                 ;If this is a "dot" directory, try again

  clc

BF_Exit:

  ret

BEGIN_FIND ENDP                    ;AN000;
; =  =  =  =  =  =  =  =  =  =  =  =



  HEADER <FIND_NEXT - LOOK FOR NEXT ENTRY IN DIRECTORY> ;AN000;
FIND_NEXT PROC NEAR                ;AN000;
  PUBLIC FIND_NEXT                 ;AN000;
;INPUT: RESERVED FIELD, LEFT FROM PREVIOUS FIND FIRST/NEXT, IN [BP].FRAM_DTA_RES
;OUTPUT: [BP].FRAM_DTA_RES UPDATED WITH NEW RESERVED DATA, FROM CURRENT DTA
;       FIX_DTA.? FIELDS ARE SET UP TO DEFINE NEW FILE JUST FOUND.
;       CY SET IF NO MORE FILES FOUND, CY CLEAR IF A NEW FILE FOUND.
; =  =  =  =  =  =  =  =  =  =  =  =

; N.b., the DOS DTA must already be set as Next_DTA
; for Find_Next to proceed properly.

  test Flags, F_Carry
  jnz  Last_Find_Not_OK

Last_Find_OK:
  ; Not first time, so copy DTA_Current to DTA_Next.
  Copy_DTA Next_DTA, Current_DTA       ;NOTE! CopyDTA SOURCE,DESTINATION

FindNext_Loop:
  DOSCall FindNext
  Set_F_Carry
  jc   Clear_C_Exit                    ; Ran out of files.

  ; Make sure we have found a directory here.  If not, keep doing FindNexts
  ; until we find one or run out of files.
  mov  al, Next_DTA_Attr
  and  al, attr_dir
  jz   FindNext_Loop                 ; Only found a file, look again.

  cmp  byte ptr Next_DTA_Filename,Period ;Now check for "." (and "..")
  je   FindNext_Loop                 ;If this is a "dot" directory, try again

Clear_C_Exit:
; N.b., the carry out from Find_Next is the carry out from the DOS FindNext
; on the file now in Current_DTA, not for the file we just got in Next_DTA.
  clc

FN_Exit:
  ret

Last_Find_Not_OK:
  ; Previous find failed, so we fail here, too.
  stc
  jmp  short FN_Exit

FIND_NEXT ENDP                     ;AN000;
; =  =  =  =  =  =  =  =  =  =  =  =



  HEADER <SHOW_FN - DISPLAY THE FILENAME FOUND> ;AN000;
SHOW_FN PROC NEAR                  ;AN000;
  PUBLIC SHOW_FN                   ;AN000;MAKE ENTRY IN LINK MAP
;INPUT - BP=OFFSET TO WORKAREA IN STACK
;        ds:dx = DTA holding filename to display
;        FLAGS (F_FLN) INDICATOR OF FLN HAVING BEEN PRINTED
;        al = file attribute to use, NOT the attribute from ds:dx.attribute
;             (copied to bl for subfunctions)
;OUTPUT - "BUF" IS SET UP TO CONTAIN THE DISPLAY LINE, AND IS SENT TO STDOUT.
; =  =  =  =  =  =  =  =  =  =  =  =
;DESCRIPTION OF THE LINE TO BE DISPLAYED, IN "BUF":
;"CURRENT_COL" HAS COL NUMBER WHERE LEADING GRAPHIC IS TO GO.
;"DASH_NUM" IS THE NUM OF DASHES THAT IMMEDIATELY FOLLOWS THE LEADING
;   GRAPHIC.  (FOR SUBDIRECTORIES ONLY - FOR REGULAR FILENAMES, THIS
;   FIELD WOULD HAVE SPACES INSTEAD OF DASHES.)
;"FLN_INDENT" IS THE NUMBER OF SPACES TO BE PUT RIGHT IN FRONT OF A
;   FILENAME.  FOR SUBDIRS, THERE IS NO SUCH FIELD
;EXAMPLE, FOR SUBDIRS
;      ÀÄÄÄDIR_FILN
;         FOR ORDINARY FILES
;      ³xxxssssANY_FILN.EXT  (WHERE x AND s ARE SPACES)
; =  =  =  =  =  =  =  =  =  =  =  =

  mov  si, dx
  CMP  byte ptr [si].DTA_Filename_Offset, PERIOD           ;AN000;DOES FILENAME START WITH PERIOD?
; $IF  NE                          ;AN000;IF NOT, CONTINUE...
  JE   SF_Exit

      MOV  BL, AL                   ;AN000;SAVE FILE ATTRIBUTE INTO BL

      CALL FLN_TO_BUF              ;AN000; MOVE NAME OF FILE TO OUTPUT BUFFER

      CALL GRAF_TO_BUF             ;AN000;DETERMINE LEADING GRAPHIC FOR BUFFER

      CALL BLANK_DASH              ;AN000;PUT BLANKS OR DASHES INTO BUF BEFORE FILENAME

; BUFFER INITIALIZED, DISPLAY IT

      MOV  DX, OFFSET BUF          ;AN000;DISPLAY FILENAME OF FILE FOUND
      CALL LEN_ASCIIZ              ;AN000;SET CX=LEN OF DX@ BUFFER, UP THRU NUL

      TEST FLAGS,F_FLN             ;AN000;HAVE ANY FLN BEEN PRINTED YET?

;     $IF  NZ                      ;AN000;IF SO, PRINT THIS
      JZ   No_Display
      CALL DO_WRITE        ;AN000;DISPLAY FILENAME IN DX TO STDOUT

No_Display:
;               CLEAN UP BUFFER FOR NEXT TIME

      CALL FIX_GRAF                ;AN000;SET UP GRAPHIC FOR NEXT LINE

      MOV  AL,[BP].FRAM_CHAR       ;AN000;GET ALTERED GRAPHIC CHAR
      MOV  DI,CURRENT_COL          ;AN000;FIND WHERE IN "BUF"
      LEA  DI,BUF-1[DI]    ;AN000; TO PUT ALTERED GRAPHIC
      STOSB                        ;AN000;STORE ALTERED GRAPHIC INTO BUFFER

      ; Is this a subdirectory?
      mov  al, bl
      and  al, ATTR_DIR
      jz   SF_Exit

      OR   FLAGS,F_SUBDIR          ;AN000;SAY, "A SUBDIR HAS BEEN PRINTED"

      MOV  DI,CURRENT_COL          ;AN000;GET COL NUM OF ELBO/TEE
      LEA  DI,BUF[DI]      ;AN000;POINT TO JUST AFTER ELBO/TEE
      MOV  AL,BLANK        ;AN000;BLANK OUT THE HORIZONTAL DASHES
      MOV  CX,DASH_NUM     ;AN000;HOW MANY DASHES WERE PUT IT
      REP  STOSB                   ;AN000;WIPE OUT THOSE DASHES FOR NEXT GUY

SF_Exit:
  RET                              ;AN000;RETURN TO CALLER
SHOW_FN ENDP                       ;AN000;
; =  =  =  =  =  =  =  =  =  =  =  =



  HEADER <FLN_TO_BUF - MOVE FILENAME TO BUFFER> ;AN000;
FLN_TO_BUF PROC NEAR               ;AN000;
  PUBLIC FLN_TO_BUF                ;AN000;
;INPUT: CURRENT_COL - INDEX INTO "BUF" WHERE THIS DISPLAY STARTS
;       ds:dx = DTA holding filename to display
;OUTPUT: "BUF" HAS LEADING GRAPHIC AND FILENAME READY FOR DISPLAY.
; =  =  =  =  =  =  =  =  =  =  =  =

  MOV  DI,CURRENT_COL              ;AN000;

  ; Is this a subdirectory?
  mov  al, bl
  and  al, ATTR_DIR
  jnz  Yes_Subdir

  LEA  DI,BUF+DASH_NUM+FLN_INDENT[DI] ;AN000;SET DESTINATION TO "BUF"+
  JMP SHORT $$EN83

Yes_Subdir:
  LEA  DI,BUF+DASH_NUM[DI]         ;AN000;SET DESTINATION TO "BUF"+

$$EN83:
  MOV  CX,LENGTH Current_DTA_Filename      ;AN000;SET COUNT TO MOVE ENTIRE FILE NAME
  MOV  SI, dx
  add  SI, DTA_Filename_Offset     ;AN000;FROM DTA OF FIND FIRST

Copy_Loop:
  LODSB                    ;AN000;GET FIRST\NEXT CHAR OF FILENAME
  STOSB                    ;AN000;MOVE THAT BYTE TO OUTPUT MSG FIELD
  CMP  AL,NUL              ;AN000;IS THIS THE NUL CHAR DELIMITER?

  JE   FTB_Exit

  LOOP Copy_Loop

FTB_Exit:
  ; Terminate with carriage return/line feed and NULL terminate
  dec  di
  mov  ax, (LF SHL 8) OR CR
  stosw
  xor  al,al
  stosb
  ret

FLN_TO_BUF ENDP                    ;AN000;
; =  =  =  =  =  =  =  =  =  =  =  =



  HEADER <GRAF_TO_BUF - SELECT LEADING GRAPHIC CHAR FOR BUF> ;AN000;
GRAF_TO_BUF PROC NEAR              ;AN000;
  PUBLIC GRAF_TO_BUF               ;AN000;
;INPUT: BL = ATTRIBUTE OF FILENAME
;       ds:dx = DTA holding filename to display
;       FLAGS (F_First_Time BIT)
;       FRAM_CHAR = LEADING CHAR, FOR FILENAME DISPLAY
;       CURRENT_COL = WHERE IN BUF TO PUT CHARS
;       BUF = TO RECEIVE LEADING CHAR
;OUTPUT: BUF HAS LEADING CHAR
; =  =  =  =  =  =  =  =  =  =  =  =

  ; Is this a subdirectory?
  mov  al, bl
  and  al, ATTR_DIR
  jnz  Y_Subdir

  CALL FIX_GRAF            ;AN000;SET UP GRAPHIC FOR NEXT LINE

Y_Subdir:
  MOV  AL,[BP].FRAM_CHAR           ;AN000;START BUF WITH CURRENT GRAPHIC CHAR
  MOV  DI,CURRENT_COL              ;AN000;
  LEA  DI,BUF-1[DI]        ;AN000;
  STOSB                            ;AN000;

  RET                              ;AN000;RETURN TO CALLER

GRAF_TO_BUF ENDP                   ;AN000;
; =  =  =  =  =  =  =  =  =  =  =  =



  HEADER <BLANK_DASH - PUT BLANKS OR DASHES BEFORE FILENAME> ;AN000;
BLANK_DASH PROC NEAR               ;AN000;
  PUBLIC BLANK_DASH                ;AN000;
;INPUT: BL - FILE ATTRIBUTE
;       CURRENT_COL - WHERE THIS DISPLAY STARTS IN "BUF"
;       BUF - PARTLY READY CHARS FOR DISPLAY
;OUTPUT: BUF HAS PROPER CHARS BETWEEN LEADING GRAPHIC AND FILENAME FIELDS.
; =  =  =  =  =  =  =  =  =  =  =  =
  ; Is this a subdirectory?
  mov  al, bl
  and  al, ATTR_DIR
  jnz  $$IF96

      MOV  AL,BLANK                ;AN000;PUT IN BLANKS
      MOV  CX,DASH_NUM + FLN_INDENT ;AN000;SPECIFY HOW MANY BLANKS TO PUT IN
; $ELSE                            ;AN000;SINCE IT IS A SUBDIR
  JMP SHORT $$EN96
$$IF96:
      MOV  AL,GRAF_DASH            ;AN000;PUT IN THE DASHES
      MOV  CX,DASH_NUM             ;AN000;SPECIFY HOW MANY DASHES TO PUT IN
; $ENDIF                           ;AN000;
$$EN96:
  MOV  DI,CURRENT_COL              ;AN000;GET COL NUM OF ELBO/TEE
  LEA  DI,BUF[DI]                  ;AN000;POINT TO JUST AFTER ELBO/TEE
  REP  STOSB                       ;AN000;ADD DASHES/BLANKS TO PRINT LINE IN "BUF"
  RET                              ;AN000;RETURN TO CALLER
BLANK_DASH ENDP                    ;AN000;
; =  =  =  =  =  =  =  =  =  =  =  =



  HEADER <FIX_GRAF - CHANGE CURRENT GRAPHIC FOR NEXT LINE> ;AN000;
  PUBLIC FIX_GRAF                  ;AN000;MAKE ENTRY IN LINK MAP
FIX_GRAF PROC NEAR                 ;AN000;
;IN THE WORKAREA, IS A CHAR THAT SHOWS WHAT HAS BEEN FOUND REGARDING ANY
;LOWER LEVELS OF SUBDIRS.
;WHEN DISPLAYING A SUBDIR, THE PRINT LINE STARTS EITHER WITH "ELBO",
;MEANING, NO LOWER LEVELS AFTER THIS ONE, OR WITH "TEE" WHICH MEANS
;THERE IS ANOTHER LEVEL OF SUBDIR BELOW THIS ONE.  AFTER THE DISPLAY,
;THE REST OF THE DISPLAY OF FILENAMES WITHIN THIS SUBDIR, WILL SHOW
;THIS CHARACTER, BUT IN A Revised FORMAT.  THIS SUBROUTINE PERFORMS
;THE MODIFICATION, AS:
;CHANGE "ELBO" TO "BLANK, OR CHANGE "TEE" TO "VERTICAL BAR".
;INPUT: BP=POINTER TO STACK WORKAREA
;       FRAM_CHAR=FIELD IN WORKAREA WITH CHAR TO BE Revised
; =  =  =  =  =  =  =  =  =  =  =  =

  MOV  AL, Graf_Elbow              ;AN002;GET ELBO CHAR
  CMP  [BP].FRAM_CHAR, AL          ;AN000;FOR NEXT DISPLAY LINE AFTER THIS ONE,
; $IF  E                           ;AN000;IF CURRENT LINE STARTS WITH "ELBO"
  JNE  Not_Elbow
  MOV  [BP].FRAM_CHAR,BLANK    ;AN000;CHANGE IT TO JUST A BLANK
; $ELSE                            ;AN000;SINCE NOT ELBO
  JMP SHORT FG_Exit

Not_Elbow:
  MOV  AL,GRAF_TEE                 ;AN002;GET THE TEE CHAR
  CMP  [BP].FRAM_CHAR,AL           ;AC002;CHANGE A "TEE"
;     $IF  E                       ;AN000;
  JNE  Not_Tee

  MOV  AL,GRAF_BAR         ;AN002;GET BAR CHAR
  MOV  [BP].FRAM_CHAR,AL   ;AC002; TO A VERTICAL "BAR"
;     $ENDIF                       ;AN000;

Not_Tee:
; $ENDIF                           ;AN000;ELBO?

FG_Exit:
  RET                              ;AN000;RETURN TO CALLER
FIX_GRAF ENDP                      ;AN000;
; =  =  =  =  =  =  =  =  =  =  =  =



  HEADER <ANY_SUBDIRS - DISPLAY MSG IF NO SUBDIRS PRINTED> ;AN000;
ANY_SUBDIRS PROC NEAR              ;AN000;
;INPUT:FLAGS (F_SUBDIR BIT) IS SET IF ANY SUBDIR HAD BEEN DISPLAYED.  IF THIS
;       BIT IS OFF, THEN DISPLAY THE MESSAGE
; =  =  =  =  =  =  =  =  =  =  =  =

  TEST FLAGS,F_SUBDIR              ;AN000;HAVE ANY SUBDIRS BEEN PRINTED YET?
; $IF  Z                           ;AN000;NO, NONE PRINTED SO FAR
  JNZ $$IF104
      MOV  DI,OFFSET MSGNUM_NOSUB  ;AN000;"No sub-directories exist"
      CALL SENDMSG                 ;AN000;DISPLAY THE MESSAGE

; $ENDIF                           ;AN000;ANY SUBDIRS PRINTED?
$$IF104:
  RET                              ;AN000;RETURN TO CALLER
ANY_SUBDIRS ENDP                   ;AN000;
; =  =  =  =  =  =  =  =  =  =  =  =



  HEADER <DO_WRITE - SEND STRING TO STDOUT> ;AN000;
DO_WRITE PROC NEAR                 ;AN000;
  PUBLIC DO_WRITE                  ;AN000;
;AFTER THE REQUESTED STRING IS SEND TO STDOUT, IT IS TERMINATED BY CR,LF
;INPUT: DX=OFFSET TO STRING TO BE WRITTEN
;       CX=LENGTH
;OUTPUT: STRING IS SENT TO STDOUT, FOLLOWED BY CR,LF.
;       BX = SAVED AND RESTORED.
; =  =  =  =  =  =  =  =  =  =  =  =

ifdef JAPAN
	call    conv_graph              ; convert graphic character to kanji
endif

  PUSH BX                          ;AN000;SAVE CALLER'S REG
  MOV  BX,STDOUT                   ;AN000;BX = FILE HANDLE
  DOSCALL WRITE                    ;AN000;(40H) WRITE FUNCTION

  POP  BX                          ;AN000;RESTORE CALLER'S REG
  RET                              ;AN000;RETURN TO CALLER
DO_WRITE ENDP                      ;AN000;
; =  =  =  =  =  =  =  =  =  =  =  =



  HEADER <IF_NOMOREFILES - ASK EXTENDED ERROR FOR WHY IS ERROR> ;AN000;
IF_NOMOREFILES PROC NEAR           ;AN000;
  PUBLIC IF_NOMOREFILES            ;AN000;
;INPUT - A DOS FUNCTION HAS JUST RETURNED WITH A CARRY INDICATING ERROR
;OUTPUT - AX=EXTENDED ERROR CODE
;       IF THE ERROR IS JUST A NO MORE FILES, CARRY IS CLEAR
;       IF ANY OTHER ERROR, THEN CARRY IS SET, AND "EXITFL" HAS RET CODE
; =  =  =  =  =  =  =  =  =  =  =  =

  CALL GET_EXTERR                  ;AN000;GET THE EXTENDED ERROR TO AX

  CMP  AX,NO_MORE_FILES            ;AN000;SEE IF FILE WAS NOT FOUND
  JNE $$IF106

  CLC                      ;AN000;INDICATE A NORMAL RETURN

; $ELSE                            ;AN000;SINCE ERROR IS SOMETHING ELSE
  JMP SHORT $$EN106

$$IF106:
      STC                          ;AN000;INDICATE AN ABNORMAL RETURN
      MOV  EXITFL,EXERR            ;AN000;INDICATE A PROBLEM TO RETURN CODE
; $ENDIF                           ;AN000;NO MORE FILES?
$$EN106:

  RET                              ;AN000;RETURN TO CALLER
IF_NOMOREFILES ENDP                ;AN000;
; =  =  =  =  =  =  =  =  =  =  =  =



  HEADER <GET_EXTERR - CALL EXTENDED ERROR> ;AN000;
GET_EXTERR PROC NEAR               ;AN000;
  PUBLIC GET_EXTERR                ;AN000;
;INPUT - A DOS FUNCTION HAS JUST RETURNED WITH A CARRY INDICATING ERROR
;OUTPUT: AX HAS EXTENDED ERROR CODE
;       NOTE: OTHER REGS, BX, CX, NORMALLY SET BY THE EXTERROR CALL
;             ARE NOT KEPT.  THESE CONTAIN "LOCUS" AND SECONDARY LEVEL CODES
;             THAT ARE NOT USED.
; =  =  =  =  =  =  =  =  =  =  =  =
  PUSH BX                          ;AN000;SAVE THE
  PUSH DS                          ;AN000; CALLER'S
  PUSH ES                          ;AN000;  REGISTERS
  PUSH CX                          ;AN000;

  MOV  BX,LEVEL_0                  ;AN000;BX=LEVEL NUMBER
  DOSCALL EXTERROR                 ;AN000;(59H) SET REGS TO SAY WHY PROBLEM

  POP  CX                          ;AN000;RESTORE REGS
  POP  ES                          ;AN000; CLOBBERED BY
  POP  DS                          ;AN000;  THE DOSCALL
  POP  BX                          ;AN000;

  RET                              ;AN000;RETURN TO CALLER
GET_EXTERR ENDP                    ;AN000;
; =  =  =  =  =  =  =  =  =  =  =  =



  HEADER <SENDMSG - PASS IN REGS DATA FROM MSG DESCRIPTOR TO DISP MSG> ;AN000;
SENDMSG PROC NEAR                  ;AN000;
  PUBLIC SENDMSG                   ;AN000;
; INPUT - DI=POINTER TO MSG_DESC STRUC FOR THIS MESSAGE
; OUTPUT - IF CARRY SET, EXTENDED ERROR MSG ATTEMPTED DISPLAYED
;          IF CARRY CLEAR, ALL OK
;          IN EITHER CASE, DI AND AX ALTERED, OTHERS OK

;  =  =  =  =  =  =  =  =  =  =  =  =

  PUSH BX                          ;AN000; SAVE CALLER'S REGS
  PUSH CX                          ;AN000;
  PUSH DX                          ;AN000;
  PUSH SI                          ;AN000;

;                PASS PARMS TO MESSAGE HANDLER IN
;                THE APPROPRIATE REGISTERS IT NEEDS.
  MOV  AX,[DI].MSG_NUM             ;AN000; MESSAGE NUMBER
  MOV  BX,[DI].MSG_HANDLE          ;AN000; HANDLE TO DISPLAY TO
  MOV  SI,[DI].MSG_SUBLIST         ;AN000; OFFSET IN ES: OF SUBLIST, OR 0 IF NONE
  MOV  CX,[DI].MSG_COUNT           ;AN000; NUMBER OF %PARMS, 0 IF NONE
  MOV  DX,[DI].MSG_CLASS           ;AN000; CLASS IN HIGH BYTE, INPUT FUNCTION IN LOW
  CALL SYSDISPMSG                  ;AN000; DISPLAY THE MESSAGE

; $IF  C                           ;AN000; IF THERE IS A PROBLEM,
  JNC $$IF109
				   ; AX=EXTENDED ERROR NUMBER
      MOV  DI,OFFSET MSGNUM_EXTERR ;AN000; GET REST OF ERROR DESCRIPTOR
      MOV  BX,[DI].MSG_HANDLE      ;AN000; HANDLE TO DISPLAY TO
      MOV  SI,[DI].MSG_SUBLIST     ;AN000; OFFSET IN ES: OF SUBLIST, OR 0 IF NONE
      MOV  CX,[DI].MSG_COUNT       ;AN000; NUMBER OF %PARMS, 0 IF NONE
      MOV  DX,[DI].MSG_CLASS       ;AN000; CLASS IN HIGH BYTE, INPUT FUNCTION IN LOW
      CALL SYSDISPMSG              ;AN000; TRY TO SAY WHAT HAPPENED

      STC                          ;AN000; REPORT PROBLEM
; $ENDIF                           ;AN000; PROBLEM WITH DISPLAY?
$$IF109:

  POP  SI                          ;AN000; RESTORE CALLER'S REGISTERS
  POP  DX                          ;AN000;
  POP  CX                          ;AN000;
  POP  BX                          ;AN000;

  RET                              ;AN000;RETURN TO CALLER
SENDMSG ENDP                       ;AN000;
; =  =  =  =  =  =  =  =  =  =  =



  HEADER <BREAK_HANDLER - CONTROL BREAK VECTOR POINTS HERE> ;AN000;
BREAK_HANDLER PROC FAR             ;AN000;"FAR" HERE IS REQUIRED FOR
  PUBLIC BREAK_HANDLER             ;AN000; BREAK INTERRUPT HANDLERS
;THE INT 23H VECTOR HAS BEEN SET TO POINT HERE.
;THIS ROUTINE GETS CONTROL IF CONTROL-BREAK IS PRESSED.
;OUTPUT: THE "STC" REQUESTS THAT DOS ABORT WHEN I RETURN.
;       THERE IS NO ERRORLEVEL VALUE TO BE PASSED BACK TO DOS AT THIS POINT.

  CALL RESTORE                     ;AN000;PUT THINGS BACK LIKE THEY WERE

  DOSCALL RET_CD_EXIT,EXCTL        ;AN000;RETURN TO DOS, WITH CTL-BREAK ERROR CODE
  INT  20H                         ;AN000;IN CASE ABOVE FAILS

;NOTE: THIS IS NOT THE MAIN EXIT FROM "TREE".
;      THE USUAL EXIT IS IN "BEGIN" PROC.
BREAK_HANDLER ENDP                 ;AN000;
; =  =  =  =  =  =  =  =  =  =  =  =



  HEADER <RESTORE - RETURN TO INITIAL DOS DEFAULT DRIVE> ;AN000;
RESTORE PROC NEAR                  ;AN000;
  PUBLIC RESTORE                   ;AN000;
;SET DOS DEFAULT DRIVE BACK TO THE INITIAL VALUE, AND
; RESTORE THE INITIAL DEFAULT PATH,
; AND THE INITIAL "APPEND" STATE,
; AND THE ORIGINAL CONTROL BREAK VECTOR.
;INPUT - "DEFAULT_DR" SET TO ALPHA LETTER OF ORIGINAL DOS DEFAULT DRIVE.
;        "START_DR_NUM" SET TO NUMERIC VALUE OF ORIGINAL DOS DEFAULT DRIVE
;        "DEFAULT_PATH" SET TO ORIGINAL CURRENT PATH OF DOS DEFAULT DRIVE
;        "APPEND_FLAGS" HAS ORIGINAL STATUS OF /X OF APPEND
;        "OLDINT23" HAS ORIGINAL OWNER OF CONTROL BREAK VECTOR 23H
;        "OLDINT24" HAS ORIGINAL OWNER OF CRITICAL ERROR VECTOR 24H
; =  =  =  =  =  =  =  =  =  =  =  =

  TEST FLAGS,F_FAILING             ;AN000;IS RESTORING SUBDIR PERMITTED?
; $IF  Z,AND                       ;AN000;YES, DO IT
  JNZ $$IF111
  TEST FLAGS,F_DEF_PAT_TAR         ;AN000;HAS ORIGINAL SUBDIR BE FOUND YET?
; $IF  NZ                          ;AN000;YES, DO IT
  JZ $$IF111

;               RESTORE THE CURRENT SUBDIRECTORY TO ITS ORIGINAL PATH

      MOV  DX,OFFSET DEFAULT_PATH  ;AN000;DS:DX = POINTER TO ASCIIZ STRING
      DOSCALL CHDIR                ;AN000;(3BH) CHANGE CURRENT DIRECTORY

; $ENDIF                           ;AN000;
$$IF111:

;               RESTORE THE DOS DEFAULT DRIVE TO ITS ORIGINAL DRIVE

  MOV  DL,START_DR_NUM             ;AN000; DL=DRIVE NUMBER (0=A,1=B)
  DOSCALL SELECT_DISK              ;AN000;(0EH) SETS DEFAULT DRIVE

;               SET APPEND BACK TO ITS ORIGINAL STATUS

  TEST FLAGS,F_APPEND              ;AN006;IF DOS VERSION OF APPEND IS ACTIVE
; $IF  NZ                          ;AN006;IT NEEDS TO BE FIXED BACK LIKE IT WAS
  JZ $$IF113
      MOV  AX,SET_APPEND           ;AN000;RESTORE APPEND TO PREVIOUS /X STATUS
      MOV  BX,APPEND_FLAGS         ;AC006;GET PREVIOUS STATUS
      INT  2FH                     ;AN000;SET IT BACK AS IT WAS

; $ENDIF                           ;AN006;DOS VERSION OF APPEND?
$$IF113:

;               FIXUP THE CONTROL BREAK VECTOR TO ITS ORIGINAL CONTENTS

  PUSH DS                          ;AN000;SAVE THE SEGREG
  LDS  DX,OLDINT23                 ;AN000;USING THE ORIGINAL CONTENTS OF THE VECTOR
				   ;DS:DX = DWORD POINTER TO BE PUT INTO VECTOR
  DOSCALL SET_VECTOR,VEC_CTLBREAK  ;AN000;(25H) RESTORE THE ORIG INT 23 HANDLER

  POP  DS                          ;AN000;RESTORE THE SEGREG

;               FIXUP THE CRITICAL ERROR VECTOR TO ITS ORIGINAL CONTENTS

  PUSH DS                          ;AN000;SAVE THE SEGREG
  LDS  DX,OLDINT24                 ;AN000;USING THE ORIGINAL CONTENTS OF THE VECTOR
				   ;DS:DX = DWORD POINTER TO BE PUT INTO VECTOR
  DOSCALL SET_VECTOR,VEC_CRITERR   ;AN000;(25H) RESTORE THE ORIG INT 24 HANDLER

  POP  DS                          ;AN000;RESTORE THE SEGREG
  RET                              ;AN000;RETURN TO CALLER
RESTORE ENDP                       ;AN000;
; =  =  =  =  =  =  =  =  =  =  =  =



  HEADER <MYERRORHANDLER - SERVICE CRITICAL ERROR HANDLER> ;AN000;
MYERRORHANDLER PROC NEAR           ;AN000;
;INPUT: DOS HAS CALLED THE CRITICAL ERROR INTERRUPT, VECTOR 24
;       AL=FAILING DRIVE NUMBER (A:=0; B:=1; ETC.)
;OUTPUT: EITHER TREE IS TERMINATED (ON ABORT OR FAIL), OR
;       AL HAS OPERATOR RESPONSE, AND IRET BACK TO DOS TO HANDLE IT.
; =  =  =  =  =  =  =  =  =  =  =  =
  ASSUME CS:CSEG                   ;AN000;ONLY THE CS REG IS WORTH A HOOT
  ASSUME DS:NOTHING                ;AN000;
  ASSUME ES:NOTHING                ;AN000;
  ASSUME SS:NOTHING                ;AN000;

  PUSH AX                          ;AN000;SAVE FAILING DRIVE NUMBER (A:=0, B:=1, ETC)
  PUSHF                            ;AN000;SAVE THE FLAGS
				   ;(THIS IS NEEDED BECAUSE THE OLD INT 24
				   ;  HANDLER WILL EXIT WITH AN "IRET")
  CALL DWORD PTR OLDINT24          ;AN000;INVOKE THE DOS ERROR HANDLER
				   ;RESPONSE WILL BE RETURNED IN AL
				   ;AL=0=IGNORE
				   ;AL=1=RETRY
				   ;AL=2=ABORT
				   ;AL=3=FAIL
  CMP  AL,ABORT                    ;AN000;DID USER SAY ABORT ?
; $IF  GE                          ;AN000;YES, PROCESS "ABORT"
  JNGE $$IF115
      PUSH CS                      ;AN000;SET UP SEGREGS
      PUSH CS                      ;AN000; SO "RESTORE" WILL LIKE THEM
      POP  ES                      ;AN000;
      POP  DS                      ;AN000;
      ASSUME DS:CSEG,ES:CSEG       ;AN000;TELL THE ASSEMBLER WHAT I JUST DID
      POP  AX                      ;AN007;GET AL=FAILING DRIVE NUMBER
      MOV  AH,START_DRIVE          ;AN007;GET THE TARGET DRIVE BEING USED
      SUB  AH,DRIVEA               ;AN007; A:=0, B:=1, ETC
      CMP  AH,AL                   ;AN007;IS START DRIVE SAME AS FAILING DRIVE?
;     $IF  E                       ;AN007;IF SAME DRIVE
      JNE $$IF116
	  OR   FLAGS,F_FAILING     ;AN007;REQUEST CHDIR ON FAILING DRIVE NOT TO BE DONE
;     $ENDIF                       ;AN007;
$$IF116:

      CALL RESTORE                 ;AN000;RESTORE ORIGINAL CONDITIONS

      DOSCALL RET_CD_EXIT,EXABORT  ;AN000;QUIT, RETURN ERRORLEVEL CODE TO DOS
; = = = = = = = = = = = = = = = = =
; $ENDIF                           ;AN000;
$$IF115:
  ADD  SP,WORD                     ;AC009;UNDO THE PUSH AX ABOVE
  IRET                             ;AN000;
MYERRORHANDLER ENDP                ;AN000;
; =  =  =  =  =  =  =  =  =  =  =  =



  HEADER <CHK_DBCS -SEE IF SPECIFIED BYTE IS A DBCS LEAD BYTE> ;AN012;
;*****************************************************************************
; Check DBCS environment
;*****************************************************************************

; Function: Check if a specified byte is in ranges of the DBCS lead bytes
; Input:    AL = Code to be examined
; Output:   If CF is on then a lead byte of DBCS
; Register: FL is used for the output, others are unchanged.

  PUBLIC CHK_DBCS                  ;AN012;
Chk_DBCS PROC                      ;AN012;
  PUSH DS                          ;AN012;save these regs, about to be clobbered
  PUSH SI                          ;AN012;
  LDS  SI,DBCSENV                  ;AN012;GET VECTOR OF DBCS RANGES

  ASSUME DS:NOTHING                ;AN012;that function clobbered old DS

  OR   SI,SI                       ;AN012;IS THIS VECTOR SET YET?
; $IF  Z                           ;AN012;NO, GO GET THE VECTOR
  JNZ $$IF119
      PUSH AX                      ;AN012;
      DOSCALL DBCS_ENV,GET_DBCS_ENV ;AN012;SET DS:SI TO POINT TO DBCS VECTOR

      MOV  WORD PTR DBCSENV,SI     ;AN012;SAVE THE DBCS VECTOR OFFSET
      MOV  WORD PTR DBCSENV+WORD,DS ;AN012; AND ITS SEGID
      POP  AX                      ;AN012;REGAIN THE CHAR TO BE CHECKED
; $ENDIF                           ;AN000;
$$IF119:
; $SEARCH                          ;AN012;
$$DO121:
      CMP  WORD PTR [SI],NUL       ;AN012;vector ends with a nul terminator entry
; $LEAVE E                         ;AN012;if that was the terminator entry, quit
  JE $$EN121
      CMP  AL,[SI]                 ;AN012;look at LOW value of vector
; $EXITIF NB,AND                   ;AN012;if this byte is in range with respect to LOW
  JB $$IF121
      CMP  AL,[SI+1]               ;AN012;look at HIGH value of vector
; $EXITIF NA                       ;AN012;if this byte is still in range
  JA $$IF121
      STC                          ;AN012;set flag to say, found a DBCS char.
; $ORELSE                          ;AN012;since char not in this vector
  JMP SHORT $$SR121
$$IF121:
      ADD  SI,WORD                 ;AN012;go look at next vector in dbcs table
; $ENDLOOP                         ;AN012;go back and check out new vector entry
  JMP SHORT $$DO121
$$EN121:
      CLC                          ;AN012;set flag to say this is not a DBCS character
; $ENDSRCH                         ;AN012;
$$SR121:
  POP  SI                          ;AN012;restore the regs
  POP  DS                          ;AN012;

  ASSUME DS:CSEG                   ;AN012;tell masm, DS back to normal

  RET                              ;AN012;
Chk_DBCS ENDP                      ;AN012;
; =  =  =  =  =  =  =  =  =  =  =  =



ifdef JAPAN
; Kanji graphic characters
KANJI_ELBO      equ     0a484h          ; 84a4h
KANJI_DASH      equ     09f84h          ; 849fh
KANJI_TEE       equ     0a584h          ; 84a5h
KANJI_BAR       equ     0a084h          ; 84a0h

;
;       convert graphic character to kanji
;
;       input:  DX = buffer address
;               CX = length
;
conv_graph      proc    near
	push    ax
	push    cx
	push    si
	mov     al,A_Graf_Elbow
	cmp     al,Graf_Elbow
	jz      cg_ret                  ; if it is in ascii mode
	mov     si,dx
cg_loop:
	lodsb
	cmp     al,Graf_Elbow
	jz      cg_elbo
	cmp     al,GRAF_DASH
	jz      cg_dash
	cmp     al,GRAF_TEE
	jz      cg_tee
	cmp     al,GRAF_BAR
	jz      cg_bar
cg_next:
	loop    cg_loop
cg_ret:
	pop     si
	pop     cx
	pop     ax
	ret

cg_elbo:
	mov     word ptr [si-1],KANJI_ELBO
	jmp     short cg_adj
cg_dash:
	mov     word ptr [si-1],KANJI_DASH
	jmp     short cg_adj
cg_tee:
	mov     word ptr [si-1],KANJI_TEE
	jmp     short cg_adj
cg_bar:
	mov     word ptr [si-1],KANJI_BAR
cg_adj:
	inc     si
	dec     cx
	jmp     short cg_next
conv_graph      endp

endif
; =  =  =  =  =  =  =  =  =  =  =  =


PATHLABL TREE              ;AN013;

CSEG ENDS                  ;AN000;

END  START                 ;AN000;


