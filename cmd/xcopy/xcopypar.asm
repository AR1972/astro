;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

;/*************************************************************************
;*                                                                        *
;*  Change History:                                                       *
;*                                                                        *
;*   Date    ID    MSFT #   STR #        Description                      *
;* -----------------------------------------------------------------------*
;*  25JUL90  C06            2133    Parser errors will sometimes not be   *
;*                                  reported correctly (eg. XCOPY Z Z Z is*
;*                                  not reported as invalid number of     *
;*                                  parameters.)                          *
;*                                                                        *
;*  08AUG90  C07          PET 668   Some errors were being printed to     *
;*                                  stdout instead of stderr.             *
;*                                                                        *
;**************************************************************************

	PAGE	,132
	TITLE	XCOPYPAR.SAL - LOOK AT COMMAND LINE PARMS
;****************** START OF SPECIFICATIONS *****************************
; MODULE NAME: XCOPYPAR.SAL
;
; DESCRIPTIVE NAME: Handle the definition of the DOS command line parameters
;		    and the interface to the DOS system PARSER.
;
;FUNCTION: The static data areas are prescribed by the DOS system PARSER
;	   to define the several parameters presented to XCOPY.  These
;	   data areas are passed to the PARSER, and its responses checked
;	   to determine the nature of the user's specifications.  Any errors
;	   found in the user's parameters are defined in messages back
;	   to the user.
;
; ENTRY POINT: PARSER, near
;
; INPUT: (DOS COMMAND LINE PARAMETERS)
;
;	      SOURCE OPERAND:			   TARGET OPERAND:
;
;	      [d:] [path] filename[.ext]	   [d:] [path] [filename[.ext]]
;		or
;	      [d:] path [filename[.ext]]
;		or
;	      d: [path] [filename[.ext]]
;
;	 WHERE
;
;	      [d:] - To specify the Source drive
;
;	      [d:] - To specify the Destination drive
;
;
;	      SWITCHES:
;
;	      /A /D /E /M /P /S /V /W
;
;
;	Upon entry to PARSER in this module,
;	"CURRENT_PARM" = offset to start of parm text in command string
;	"ORDINAL" = initialized to zero
;	PSP+81H = text of DOS command line parms string

; EXIT-NORMAL:

; EXIT-ERROR:

; INTERNAL REFERENCES:
;    ROUTINES:
;	PARSE_ERROR:NEAR Display the appropriate Parse error message.

;    DATA AREAS:
;	The several parameter control blocks, defined by the System
;	PARSER interface, defining the XCOPY parameters.

; EXTERNAL REFERENCES:
;    ROUTINES:
;	SYSPARSE:NEAR	System Command Line Common Parser.
;
;    DATA AREAS:
;	EXITFL:BYTE	Errorlevel return code.
;	MSGNUM_PARSE:WORD Message descriptor for all parse errors.
;
; NOTES:
;	 This module should be processed with the SALUT preprocessor
;	 with the re-alignment not requested, as:
;
;		SALUT XCOPYPAR,NUL
;
;	 To assemble these modules, the alphabetical or sequential
;	 ordering of segments may be used.
;
;	 For LINK instructions, refer to the PROLOG of the main module,
;	 XCOPY.SAL.
;
; REVISION HISTORY: A000 Version 4.00: add PARSER, System Message Handler,
;			 Ignore unique volume serial number differences
;		    A004 PTM0700 9/02/87 Avoid duplicate switches and
;			 display parm in error.
;
;     Label: "The DOS XCOPY Utility"
;
;****************** END OF SPECIFICATIONS *****************************
	IF1
	    %OUT    COMPONENT=XCOPY, MODULE=XCOPYPAR.SAL...
	ENDIF
; =  =	=  =  =  =  =  =  =  =	=  =
HEADER	MACRO	TEXT
.XLIST
	SUBTTL	TEXT
.LIST
	PAGE
	ENDM
; =  =	=  =  =  =  =  =  =  =	=  =
;		      $SALUT (4,23,28,36)
;		LOCAL EQUATES

INIT_ERROR_FLAG       EQU  80H	   ;AN000;critical initialization error. Should abort
CMD_BUF_SIZE	      EQU  127	   ;AN000;NUMBER BYTES IN DOS COMMAND LINE BUFFER
ZERO		      EQU  0	   ;AN000;COMPARAND FOR CLEARED REG
NUL		      EQU  0	   ;AN000;DELIMITER FOR ASCIIZ STRINGS

; =  =	=  =  =  =  =  =  =  =	=  =

;		PARSER ASSEMBLE SWITCHES

FarSW		      EQU  0	   ;AN000;CALL THE PARSER BY FAR CALL
DateSW		      EQU  1	   ;AN000;DATE FORMAT
TimeSW		      EQU  1	   ;AN000;TIME FORMAT
FileSW		      EQU  1	   ;AN000;FILE SPECIFICATION
CAPSW		      EQU  1	   ;AN000;USE FILE TABLE CAPS
CmpxSW		      EQU  0	   ;AN000;COMPLEX LIST
DrvSW		      EQU  1	   ;AN000;DRIVE ONLY FORMAT
QusSW		      EQU  0	   ;AN000;QUOTED STRING
NumSW		      EQU  1	   ;AN000;NUMERIC VALUE
KeySW		      EQU  0	   ;AN000;KEYWORDS
SwSW		      EQU  1	   ;AN000;SWITCHES
Val1SW		      EQU  0	   ;AN000;VALUE DEFINITION #1
Val2SW		      EQU  0	   ;AN000;VALUE DEFINITION #2
Val3SW		      EQU  0	   ;AN000;VALUE DEFINITION #3
; =  =	=  =  =  =  =  =  =  =	=  =
;		EXIT CODES FROM SYSPARSE (WHEN CY=0)

SYSPRM_EX_OK	      EQU  0	   ;AN000; no error
SYSPRM_EX_MANY	      EQU  1	   ;AN000; too many operands
SYSPRM_EX_MISSING     EQU  2	   ;AN000; required operand missing
SYSPRM_EX_NOT_SWLIST  EQU  3	   ;AN000; not in switch list provided
SYSPRM_EX_NOT_KEYLIST EQU  4	   ;AN000; not in keyword list provided
SYSPRM_EX_RANGE       EQU  6	   ;AN000; out of range specified
SYSPRM_EX_VALUE       EQU  7	   ;AN000; not in value list provided
SYSPRM_EX_STRING      EQU  8	   ;AN000; not in string list provided
SYSPRM_EX_SYNTAX      EQU  9	   ;AN000; syntax error
SYSPRM_EX_EOL	      EQU  -1	   ;AN000; end of command line
; =  =	=  =  =  =  =  =  =  =	=  =

		      HEADER <STRUC - DEFINITIONS OF EXTERNAL CONTROL BLOCKS>
;		$SALUT (4,17,22,36)
PSP		STRUC
		DB   80H DUP (?)   ;AN000;SKIP OVER FIRST HALF OF PSP
PSP_PARMLEN	DB   ?		   ;AN000;NUMBER OF BYTES IN DOS COMMAND LINE
PSP_COMMAND	DB   127 DUP(?)    ;AN000;TEXT OF DOS COMMAND LINE
PSP		ENDS
; =  =	=  =  =  =  =  =  =  =	=  =
CSEG		SEGMENT PUBLIC 'CODE'	   ;AN000;PLACE HOLDER FOR PARSE CODE
;C07 EXTRN	        PRINT_STDOUT:NEAR
EXTRN	        PRINT_STDERR:NEAR                                         ;C07
CSEG		ENDS
DGROUP		GROUP DSEG, DSEG_INIT
DSEG		SEGMENT PARA	PUBLIC 'DATA'
DSEG		ENDS
;
DSEG_INIT	SEGMENT PARA PUBLIC 'INIT_DATA'
;
 include version.inc
 INCLUDE PSDATA.INC		   ;AN018;WORK AREA USED BY PARSE.ASM
;
;--- EXTERNAL VARIABLES ---
EXTRN		PARM_FLAG: BYTE    ;AN000;
EXTRN		OPTIONS_SENT: BYTE
EXTRN		ERRORLEVEL: BYTE
EXTRN		ESET: BYTE
EXTRN		SSET: BYTE
EXTRN		QSET: BYTE
EXTRN   	MSG_NUM:WORD			;AN000;MESSAGE NUMBER
EXTRN	        MSG_CLASS:BYTE			;AN000;MESSAGE CLASS
EXTRN	        INPUT_FLAG:BYTE 		;AN000;TYPE INT21 USED FOR KBD INPUT
EXTRN	        SUBST_COUNT:WORD		;AN000;MESSAGE SUBSTITUTION COUNT


		IF1
		    %OUT COMPONENT=XCOPY, SUBCOMPONENT=PARSE
		ENDIF
;---
COMMAND_LINE	DB   127 DUP(?)    ;AN000;TEXT OF DOS COMMAND LINE (INTERNAL USE)
		PUBLIC COMMAND_LINE

CURRENT_PARM	DW   DGROUP:COMMAND_LINE ;AN000;POINTER INTO COMMAND OF NEXT
				   ;OPERAND
		PUBLIC CURRENT_PARM

ORDINAL 	DW   0		   ;AN000;ORDINAL NUMBER OF WHICH PARM TO PARSE
		PUBLIC ORDINAL

TAR_DRIVE	DB   " "	   ;AN000;TARGET DRIVE LETTER SPECIFIED IN PARMS
SO_DRIVE	DB   " "	   ;AN000;SOURCE DRIVE LETTER SPECIFIED
		PUBLIC TAR_DRIVE,SO_DRIVE ;AN000;PASS RESULTS TO INIT ROUTINE

; =  =	=  =  =  =  =  =  =  =	=  =
		HEADER <DOS COMMAND LINE PARSER CONTROL BLOCKS>

;INPUT PARAMETERS CONTROL BLOCK, POINTED TO BY ES:DI WHEN CALLING PARSER

		PUBLIC PARMS	   ;AN000;LET LINK MAKE PARMS BLOCK ADDRESSABLE
PARMS		LABEL BYTE	   ;AN000;PARMS CONTROL BLOCK
		DW   DGROUP:PARMSX ;AN000;POINTER TO PARMS EXTENSION
		DB   0		   ;AN000; NUMBER OF STRINGS (0, 1, 2)
		DB   1		   ;AN000; NUMBER OF ADDITIONAL DELIMITERS
		DB   ";"	   ;AN000; ADDITIONAL DELIMITER


;SYSTEM PARSER PARAMETER EXTENSION CONTROL BLOCK
PARMSX		LABEL BYTE	   ;AN000; PARMS EXTENSION CONTROL BLOCK
		DB   1,2	   ;AN000; MIN, MAX POSITIONAL OPERANDS ALLOWED
		DW   DGROUP:CONTROL_POS1 ;AN000; DESCRIPTION OF POSITIONAL 1
		DW   DGROUP:CONTROL_POS2 ;AN000; DESCRIPTION OF POSITIONAL 2

		DB   3		   ;AN000; THERE ARE 8 SWITCHES IN 2 GROUPS
				   ;AN000; (/A, /E, /M, /P, /S, /V, /W, /D)
		DW   DGROUP:SW1_7  ;AN000; POINTER TO THE  SWITCH DEFINITION AREA
		DW   DGROUP:SW8    ;AN000; POINTER TO EIGHTH SWITCH DEFINITION AREA
		DW   DGROUP:SW9    ;       POINTER TO NINTH  SWITCH DEFINITION AREA

		DB   0		   ;AN000; MAX KEYWORD OPERANDS ALLOWED
				   ;AN000; THERE IS NO CONTROL BLOCK
				   ;AN000; DEFINING KEYWORDS

; = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
		HEADER <POSITIONAL PARM DESCRIPTOR BLOCK>
;PARSER CONTROL BLOCK DEFINING THE ONLY POSITIONAL PARAMETER, OPTIONAL

;FIRST POSITIONAL PARAMETER IS:
;
;	      [d:] [path] filename[.ext]
;		or
;	      [d:] path [filename[.ext]]
;		or
;	      d: [path] [filename[.ext]]
;
		PUBLIC CONTROL_POS1 ;AN000; LET LINK MAKE THIS ADDRESSABLE
CONTROL_POS1	LABEL BYTE	   ;AN000; FIRST POSITIONAL DESCRIPTOR FOR FILESPEC
				   ;AN000;
		DW   0200H	   ;AN000; CONTROLS TYPE MATCHED
				   ; SELECTED BITS: "FILE SPEC"

				   ; 8000H=NUMERIC VALUE, (VALUE LIST WILL BE CHECKED)
				   ; 4000H=SIGNED NUMERIC VALUE (VALUE LIST WILL BE
				   ;   CHECKED)
				   ; 2000H=SIMPLE STRING(VALUE LIST WILL BE CHECKED)
				   ; 1000H=DATE STRING (VALUE LIST WON'T BE CHECKED)
				   ; 0800H=TIME STRING (VALUE LIST WON'T BE CHECKED)
				   ; 0400H=COMPLEX LIST (VALUE LIST WON'T BE CHECKED)
				   ; 0200H=FILE SPEC (VALUE LIST WON'T BE CHECKED)
				   ; 0100H=DRIVE ONLY (VALUE LIST WON'T BE CHECKED)
				   ; 0080H=QUOTED STRING (VALUE LIST WON'T BE CHECKED)
				   ; 0010H=IGNORE ":" AT END IN MATCH
				   ; 0002H=REPEATS ALLOWED
				   ; 0001H=OPTIONAL

		DW   0002H	   ;AN000; FUNCTION_FLAGS
				   ; 0001H=CAP RESULT BY FILE TABLE
				   ; 0002H=CAP RESULT BY CHAR TABLE
				   ; 0010H=REMOVE ":" AT END
		DW   DGROUP:RESULT1 ;AN000; RESULT BUFFER (FIRST)
		PUBLIC RESULT1
		DW   DGROUP:NOVALS ;AN000; NO VALUE LISTS
		DB   0		   ;AN000; NUMBER OF KEYWORD/SWITCH SYNONYMS
				   ;AN000;   IN FOLLOWING LIST

;SECOND POSITIONAL PARAMETER IS:
;
;	      [d:] [path] [filename[.ext]]
;
		PUBLIC CONTROL_POS2 ;AN000; LET LINK MAKE THIS ADDRESSABLE
CONTROL_POS2	LABEL BYTE	   ;AN000; SECOND POSITIONAL DESCRIPTOR FOR FILESPEC,
				   ;AN000; OPTIONAL
		DW   0201H	   ;AN000; CONTROLS TYPE MATCHED
				   ;AN000; SELECTED BITS: "FILE SPEC"
		DW   0002H	   ;AN000; FUNCTION_FLAGS
		DW   DGROUP:RESULT2 ;AN000; RESULT BUFFER (SECOND)
		DW   DGROUP:NOVALS ;AN000; NO VALUE LISTS
		DB   0		   ;AN000; NUMBER OF KEYWORD/SWITCH SYNONYMS
				   ;AN000;   IN FOLLOWING LIST


;VALUE CONTROL BLOCK FOR THE POSITIONAL PARAMETERS
NOVALS		DB   0		   ;AN000;NO VALUE DEFINITIONS

;RESULTS CONTROL BLOCK FOR THE FIRST POSITIONAL PARAMETER
RESULT1 	LABEL BYTE	   ;AN000; BELOW FILLED IN FOR DEFAULTS
TYPE1		DB   0		   ;AN000; TYPE RETURNED: 0=RESERVED,
		PUBLIC TYPE1
				   ;AN000;	 1=NUMBER, 2=LIST INDEX,
				   ;AN000;	 3=STRING, 4=COMPLEX,
				   ;AN000;	 5=FILESPEC, 6=DRIVE
				   ;AN000;	 7=DATE, 8=TIME
				   ;AN000;	 9=QUOTED STRING
RESULT_TAG1	DB   0FFH	   ;AN000; MATCHED ITEM TAG
		DW   0		   ;AN000; POINTER TO SYNONYM

RESULT_PTR1	DD   0		   ;AN000; FILESPEC OFFSET
		PUBLIC RESULT_PTR1

;RESULTS CONTROL BLOCK FOR THE SECOND POSITIONAL PARAMETER
RESULT2 	LABEL BYTE	   ;AN000; BELOW FILLED IN FOR DEFAULTS
		PUBLIC RESULT2
TYPE2		DB   0		   ;AN000; TYPE RETURNED: 0=RESERVED,
		PUBLIC TYPE2
				   ;AN000;	 1=NUMBER, 2=LIST INDEX,
				   ;AN000;	 3=STRING, 4=COMPLEX,
				   ;AN000;	 5=FILESPEC, 6=DRIVE
				   ;AN000;	 7=DATE, 8=TIME
				   ;AN000;	 9=QUOTED STRING
RESULT_TAG2	DB   0FFH	   ;AN000; MATCHED ITEM TAG
		DW   0		   ;AN000; POINTER TO SYNONYM

RESULT_PTR2	DD   0		   ;AN000; FILESPEC OFFSET
		PUBLIC RESULT_PTR2

; = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
		HEADER <SWITCH PARM DESCRIPTOR BLOCK>
;PARSER CONTROL BLOCK DEFINING THE SWITCHES, OPTIONAL

		PUBLIC SW1_7	   ;AN000;LET LINK MAKE THIS ADDRESSABLE
SW1_7		LABEL BYTE	   ;AN000;SWITCH DESCRIPTOR FOR THE FIRST SEVEN SW
		DW   0001H	   ;AN000; CONTROLS TYPE MATCHED
				   ;SELECTED BITS: "OPTIONAL"
				   ; 8000H=NUMERIC VALUE, (VALUE LIST WILL BE CHECKED)
				   ; 4000H=SIGNED NUMERIC VALUE (VALUE LIST WILL BE
				   ;   CHECKED)
				   ; 2000H=SIMPLE STRING(VALUE LIST WILL BE CHECKED)
				   ; 1000H=DATE STRING (VALUE LIST WON'T BE CHECKED)
				   ; 0800H=TIME STRING (VALUE LIST WON'T BE CHECKED)
				   ; 0400H=COMPLEX LIST (VALUE LIST WON'T BE CHECKED)
				   ; 0200H=FILE SPEC (VALUE LIST WON'T BE CHECKED)
				   ; 0100H=DRIVE ONLY (VALUE LIST WON'T BE CHECKED)
				   ; 0080H=QUOTED STRING (VALUE LIST WON'T BE CHECKED)
				   ; 0010H=IGNORE ":" AT END IN MATCH
				   ; 0002H=REPEATS ALLOWED
				   ; 0001H=OPTIONAL

		DW   0002H	   ;AN000; FUNCTION_FLAGS
				   ; 0001H=CAP RESULT BY FILE TABLE
				   ; 0002H=CAP RESULT BY CHAR TABLE
				   ; 0010H=REMOVE ":" AT END

		DW   DGROUP:RESULTSW1 ;AN000; RESULT BUFFER
		PUBLIC RESULTSW1   ;AN000;LET LINK MAKE THIS ADDRESSABLE
		DW   DGROUP:NOVALS ;AN000; VALUE LISTS
		DB   7		   ;AN000; NUMBER OF KEYWORD/SWITCH SYNONYMS
				   ;AN000;   IN FOLLOWING LIST
SW_A		DB   "/A",0
		PUBLIC SW_A
SW_E		DB   "/E",0
		PUBLIC SW_E
SW_M		DB   "/M",0
		PUBLIC SW_M
SW_P		DB   "/P",0
		PUBLIC SW_P
SW_S		DB   "/S",0
		PUBLIC SW_S
SW_V		DB   "/V",0
		PUBLIC SW_V
SW_W		DB   "/W",0
		PUBLIC SW_W

;PARSER CONTROL BLOCK DEFINING THE DATE SWITCH, OPTIONAL

		PUBLIC SW8	   ;AN000; LET LINK MAKE THIS ADDRESSABLE
SW8		LABEL BYTE	   ;AN000; SWITCH DESCRIPTOR FOR THE DATE SW
		DW   1000H	   ;AN000; CONTROLS TYPE MATCHED
		DW   0000H	   ;AN000; FUNCTION_FLAGS
		DW   DGROUP:DATE_BUFF ;AN000; RESULT BUFFER
		DW   DGROUP:NOVALS ;AN000; VALUE LISTS
		DB   1		   ;AN000; NUMBER OF KEYWORD/SWITCH SYNONYMS
				   ;AN000;   IN FOLLOWING LIST
SW_D		DB   "/D",0
		PUBLIC SW_D


;PARSER CONTROL BLOCK DEFINING THE /? SWITCH, OPTIONAL

		PUBLIC SW9, SW9_SYN
SW9		LABEL BYTE
		DW   0			; MATCH MASK
		DW   0			; FUNC MASK
		DW   DGROUP:SW9_BUFF	; ptr to RESULT BUFFER
		DW   DGROUP:NOVALS	; ptr to VALUE LIST
		DB   1			; # of SYNONYMS
SW9_SYN		DB   "/?",0



;RESULTS CONTROL BLOCK FOR THE /A,/E,/M,/P,/S,/V,/W SWITCHES
RESULTSW1	LABEL BYTE	   ;AN000; BELOW FILLED IN FOR DEFAULTS
		DB   3		   ;AN000; TYPE RETURNED: 0=RESERVED,
				   ;	   1=NUMBER, 2=LIST INDEX,
				   ;	   3=STRING, 4=COMPLEX,
				   ;	   5=FILESPEC, 6=DRIVE
				   ;	   7=DATE, 8=TIME
				   ;	   9=QUOTED STRING
		DB   0FFh	   ;AN000; MATCHED ITEM TAG

RESULTSWSYN	DW   0		   ;AN000; SYNONYM POINTER (BASED ON ES:)
		PUBLIC RESULTSWSYN
RESULTSWVAL	DD   0		   ;AN000; OFFSET OF STRING VALUE
		PUBLIC RESULTSWVAL


;RESULT CONTROL BLOCK FOR THE /D SWITCH
DATE_BUFF	LABEL BYTE
		DB   7		   ;AN000; TYPE RETURNED (DATE)
		DB   0FFh	   ;AN000; MATCHED ITEM TAG
		DW   0		   ;AN000; SYNONYM POINTER (BASED ON ES:)
DATE_YEAR	DW   0		   ;AN000; YEAR
		PUBLIC DATE_YEAR
DATE_MONTH	DB   0		   ;AN000; MONTH
		PUBLIC DATE_MONTH
DATE_DAY	DB   0		   ;AN000; DAY
		PUBLIC DATE_DAY

;RESULT CONTROL BLOCK FOR THE /? SWITCH

		PUBLIC SW9_BUFF, SW9_BUFF_SYN
SW9_BUFF	LABEL BYTE
		DB   0			; TYPE
		DB   0			; TAG
SW9_BUFF_SYN	DW   0			; ptr to matched SYNONYM
		DD   0			; padding (not used)


; =  =	=  =  =  =  =  =  =  =	=  =
DSEG_INIT	ENDS
		HEADER <PARSING WORKAREAS>
;	     $SALUT (4,14,19,36)
CSEG	     SEGMENT PUBLIC 'CODE'
	     ASSUME CS:CSEG, DS:DGROUP, ES:DGROUP

	     PUBLIC SYSPARSE	   ;AN000;SUBROUTINE ENTRY POINT		 ;AN000;


	     IF1
		 %OUT COMPONENT=XCOPY, SUBCOMPONENT=PARSE, MODULE=PARSE.ASM...
	     ENDIF
INCSW EQU 0			  ;AN018;TELL PARSE.ASM PSDATA.INC IS INCLUDED
BASESW EQU 1			  ;AN018;PSDATA.INC IS ADDRESSABLE WITH DS
;	INCLUDE PARSE.ASM	  ;AN000;GENERATED CODE SUPPRESSED FROM LISTING
.XLIST
.XCREF
	     INCLUDE PARSE.ASM
.LIST
.CREF

	     EXTRN GET_PARMS:NEAR  ;AN000;COMMAND LINE PARMS AND OPTIONS PROCESSING

	     HEADER <PARSER - ASK SYSPARM TO DECODE PARAMETERS>
;  $SALUT (4,4,9,36)
PARSER PROC NEAR
   PUBLIC PARSER

;INPUT: CURRENT_PARM = OFFSET TO NEXT PARM IN COMMAND STRING
;	COMMAND_LINE = COPY OF DOS COMMAND LINE PARAMETERS
;	"ORDINAL" = COUNT OF NEXT PARM TO PARSE
;OUTPUT: CARRY IS SET IF THERE WAS A PROBLEM, AX HAS PARSE RET CODE.
;	 CARRY IS CLEAR IF ALL OK WITH THE PARMS
;THE PSP IS NOT REFERENCED, SINCE THE PARMS HAVE BEEN MOVED OUT OF THERE.


   MOV	ORDINAL,ZERO		   ;AN000;OPERAND ORDINAL, INITALLY ZERO
;  $SEARCH COMPLEX		   ;AN000;LOOP THRU COMMAND LINE
   JMP SHORT $$SS1
$$DO1:
				   ;AN000;LOOKING AT RETURN CODE IN AX,
				   ;AN000; JUST PRODUCED BY SYSPARSE...
       CMP  AX,ZERO		   ;AN000;WERE THERE ANY ERRORS?
;  $EXITIF NE,OR		   ;AN000;HAD A PROBLEM
   JNE $$LL2
       MOV  ORDINAL,CX		   ;AN000;SAVE UPDATED COUNT
       MOV  CURRENT_PARM,SI	   ;AN000;REMEMBER HOW FAR I GOT
       MOV  BX,DX		   ;AN000;SET DATA BASE REG TO POINT TO THIS OPERAND
       CALL GET_PARMS		   ;AN000;GET 1ST AND 2ND PARAMETERS
       CMP  DGROUP:[OPTIONS_SENT], 0	; were options sent?
       JNE  PARSER_EXIT			; bail out now if so
       TEST PARM_FLAG,INIT_ERROR_FLAG ;AN000;CRITICAL PARAMETER ERROR HAS OCCURRED
;  $EXITIF NZ			   ;AN000;HAD A PROBLEM
   JZ $$IF1
$$LL2:
       STC			   ;AN000;SET CARRY TO INDICATE ERROR
       CALL PARM_ERROR		   ;AN000;GET OUT WITH ERROR INFORMATION
				   ;AN000;EITHER PARAMETER OR PARSER ERROR PROCESSED


;  $ORELSE			   ;AN000;SINCE NO PROBLEM, SO FAR
   JMP SHORT $$SR1
$$IF1:
;  $STRTSRCH
$$SS1:

       LEA  DI,PARMS		   ;AN000; ES:DI = PARSE CONTROL DEFINITON
       MOV  SI,CURRENT_PARM	   ;AN000; DS:SI = COMMAND STRING, NEXT PARM
       XOR  DX,DX		   ;AN000; RESERVED, INIT TO ZERO
       MOV  CX,ORDINAL		   ;AN000; OPERAND ORDINAL, INITIALLY ZERO
       CALL SYSPARSE
				   ;AN000; AX=EXIT CODE
				   ;AN000; BL=TERMINATED DELIMITER CODE
				   ;AN000; CX=NEW OPERAND ORDINAL
				   ;AN000; SI=SET TO PAST SCANNED OPERAND
				   ;AN000; DX=SELECTED RESULT BUFFER
       CMP  AX,SYSPRM_EX_EOL	   ;AN000; IS THAT THE END OF THE PARMS?
				   ;AN000;IF NOT, LOOP BACK AND FIND OUT
				   ;AN000; WHAT THAT PARM IS
;  $ENDLOOP E			   ;AN000;END OF LIST
   JNE $$DO1
       CLC			   ;AN000;CLEAR CARRY, END OF LIST OK
;  $ENDSRCH
;C06 $$SR1:
PARSER_EXIT:
;	JC  BLIP

; if /? was also specified, /e & /s not relevant, do not print errmsg
       CMP  QSET, 1
       JE   BLIP

       CMP  ESET, 0
       JE   BLIP
       CMP  SSET, 1
       JE   BLIP
 
; /E was specified w/o /S also specified.  Give Syntax Error msg and return
       PUSH    AX			
       MOV     AX, 35              ; MSG_INV_SW  (invalid switch)
       MOV     MSG_NUM,AX	
       MOV     SUBST_COUNT,  00H   ; NO_SUBST (no substitutions)
       MOV     INPUT_FLAG,  00H    ; NO_INPUT
       MOV     MSG_CLASS, 0FFH     ; UTILITY_MSG_CLASS

;C07   CALL    PRINT_STDOUT
       CALL    PRINT_STDERR                                               ;C07
       POP     AX
       MOV     OPTIONS_SENT, 1
       MOV     ERRORLEVEL, 1

$$SR1:                                                                    ;C06
BLIP:   RET				   ;AN000;RETURN TO CALLER
PARSER ENDP
; =  =	=  =  =  =  =  =  =  =	=  =
   HEADER <PARM_ERROR - ????????????>
PARM_ERROR PROC NEAR
;INPUT: DX - ADDRESS OF MESSAGE TEXT
;	PARM_FLAG set to INIT_ERROR_FLAG (critical error)
;	OR THERE WAS A PARSER ERROR

   RET				   ;AN000;RETURN TO CALLER WITH C SET

PARM_ERROR ENDP


; =  =	=  =  =  =  =  =  =  =	=  =
CSEG ENDS
   END

