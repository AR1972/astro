	PAGE	90,132			;AN000;A2
	TITLE	DCOMPPAR.SAL - LOOK AT COMMAND LINE PARMS
;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */
;****************** START OF SPECIFICATIONS *****************************
; MODULE NAME: DCOMPPAR.SAL

; DESCRIPTIVE NAME: Handle the definition of the DOS command line parameters
;		    and the interface to the DOS system PARSER.

;FUNCTION: The static data areas are prescribed by the DOS system PARSER
;	   to define the several parameters presented to DISKCOMP.  These
;	   data areas are passed to the PARSER, and its responses checked
;	   to determine the nature of the user's specifications.  Any errors
;	   found in the user's parameters are defined in messages back
;	   to the user.

; ENTRY POINT: PARSER, near

; INPUT: (DOS COMMAND LINE PARAMETERS)

;	      [d:][path] DISKCOMP  [d: [d:]] [/1] [/8]

;	 WHERE
;	      [d:][path] - Path where the DISKCOMP command resides.

;	      [d:] - To specify the Source drive
;
;	      [d:] - To specify the Target drive
;
;	      [/1] - To compare only the first side of the diskette,
;		     regardless of the diskette or drive type.

;	      [/8] - To compare only the first 8 sectors per track,
;		     even if the first diskette contains 9/15 sectors
;		     per track.
;
;	Upon entry to PARSER in this module,
;	"CURRENT_PARM" = offset to start of parm text in command string
;	"ORDINAL" = initialized to zero
;	PSP+81H = text of DOS command line parms string

; EXIT-NORMAL:
;      "SOURCE_DRIVE" = CHAR OF FIRST DRIVE ID SPECIFIED, BLANK IF NONE
;      "TARGET_DRIVE" = CHAR OF SECOND DRIVE ID IF BOTH SPECIFIED, BLANK
;		   IF NONE OR ONLY ONE SPECIFIED
;      "USER_OPTION" = 01 ON IF /1, -1 IF /1 NOT SPECIFIED.
;      "USER_OPTION_8" = 01 ON IF /8, -1 IF /8 NOT SPECIFIED.

; EXIT-ERROR:
;      IF ERROR, ERROR MESSAGE IS DISPLAYED, AND "EXITFL" HAS "EXPAR".

; INTERNAL REFERENCES:
;    ROUTINES:
;	PARSER:NEAR Call the system Parser to decode command line
;	PARSE_ERROR:NEAR Display the appropriate Parse error message.

;    DATA AREAS:
;	The several parameter control blocks, defined by the System
;	PARSER interface, defining the DISKCOMP parameters.

; EXTERNAL REFERENCES:
;    ROUTINES:
;	SENDMSG:NEAR	Uses Msg Descriptor to drive message handler.
;	SYSPARSE:NEAR	System Command Line Common Parser.

;    DATA AREAS:
;	EXITFL:BYTE	Errorlevel return code.
;	MSGNUM_PARSE:WORD Message descriptor for all parse errors.
;	USER_OPTION:BYTE /1 parm indicator
;	USER_OPTION_8:BYTE /8 parm indicator
;	SOURCE_DRIVE:BYTE character of first specified drive
;	TARGET_DRIVE:BYTE character of second specified drive

; NOTES:
;	 This module should be processed with the SALUT preprocessor
;	 with the re-alignment not requested, as:

;		SALUT DCOMPPAR,NUL

;	 To assemble these modules, the alphabetical or sequential
;	 ordering of segments may be used.

;	 For LINK instructions, refer to the PROLOG of the main module,
;	 DISKCOMP.SAL.

;****************** END OF SPECIFICATIONS *****************************
	IF1				;AN000;
	    %OUT    COMPONENT=DISKCOMP, MODULE=DCOMPPAR.SAL... ;AN000;
	ENDIF				;AN000;
	INCLUDE PATHMAC.INC		;AN013;
	INCLUDE DCMPMACR.INC
; =  =	=  =  =  =  =  =  =  =	=  =
HEADER	MACRO	TEXT			;;AN000;
.XLIST					;AN000;
	SUBTTL	TEXT			;AN000;
.LIST					;AN000;
	PAGE				;;AN000;
	ENDM				;;AN000;
; =  =	=  =  =  =  =  =  =  =	=  =
;		      $SALUT (4,23,28,36) ;AN000;
CHAR_A		      EQU  "A"	   ;AN000;ASCII VALUE OF CHARACTER "A"
NUL		      EQU  0	   ;AN003;ASCIIZ STRING DELIMITER
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
		      HEADER <STRUC - DEFINITIONS OF EXTERNAL CONTROL BLOCKS> ;AN000;
PSP		      STRUC	   ;AN000;
		      DB   80H DUP (?) ;AN000;SKIP OVER FIRST HALF OF PSP
PSP_PARMLEN	      DB   ?	   ;AN000;NUMBER OF BYTES IN DOS COMMAND LINE
PSP_COMMAND	      DB   127 DUP(?) ;AN000;TEXT OF DOS COMMAND LINE
PSP		      ENDS	   ;AN000;

MSG_DESC	      STRUC	   ;AN003;
MSG_NUM 	      DW   ?	   ;AN003;MESSAGE NUMBER (TO AX)
MSG_HANDLE	      DW   ?	   ;AN003;HANDLE OF OUTPUT DEVICE (TO BX)
MSG_SUBLIST	      DW   ?	   ;AN003;POINTER TO SUBLIST (TO SI)
MSG_COUNT	      DW   ?	   ;AN003;SUBSTITUTION COUNT (TO CX)
MSG_CLASS	      DW   ?	   ;AN003;MESSAGE CLASS (IN HIGH BYTE, TO DH)
				   ; LOW BYTE HAS 0 (FUNCTION "NO INPUT", TO DL);AN003;
MSG_DESC	      ENDS	   ;AN003;

ONE_SUBS	      EQU  1	   ;AN003;NUMBER OF VARIABLES

SUBLIST 	      STRUC	   ;AN000;
SUB_SIZE	      DB   ?	   ;AN003;SUBLIST SIZE (POINTER TO NEXT SUBLIST)
SUB_RES 	      DB   ?	   ;AN003;RESERVED
				   ;NEXT FIELD IS TO BE USED AS A DOUBLE WORD	   ;AN003;
SUB_VALUE	      DW   ?	   ;AN003;TIME, DATE, OR PTR TO DATA ITEM
SUB_VALUE_SEG	      DW   ?	   ;AN003;SEG ID OF PTR
				   ;(ABOVE FIELD MUST BE FILLED AT EXECUTION TIME  ;AN003;
				   ; IF THIS IS A .COM FILE)			   ;AN003;
SUB_ID		      DB   ?	   ;AN003;N OF %N
SUB_FLAGS	      DB   ?	   ;AN003;DATA TYPE FLAGS
SUB_MAX_WIDTH	      DB   ?	   ;AN003;MAXIMUM FIELD WIDTH (0=UNLIMITED)
SUB_MIN_WIDTH	      DB   ?	   ;AN003;MINIMUM FIELD WIDTH
SUB_PAD_CHAR	      DB   ?	   ;AN003;CHARACTER FOR PAD FIELD
				   ; CAN BE " ", "0" OR ",".			   ;AN003;
				   ; "," CAUSES INSERTION OF THE ACTIVE 	   ;AN003;
				   ; THOUSANDS SEPARATOR BETWEEN EVERY 3 DIGITS.   ;AN003;
SUBLIST 	      ENDS	   ;AN003;

; =  =	=  =  =  =  =  =  =  =	=  =
		      HEADER <PARSING WORKAREAS> ;AN000;
;	     $SALUT (4,14,19,36)   ;AN000;
	     EXTRN EXPAR:ABS	   ;AN000;ERRORLEVEL VALUE FOR BAD PARMS
	     EXTRN FINE:ABS	   ;AN000;RETURN STATUS INDICATOR
	     EXTRN MSGNUM_OPTIONS  : BYTE
	     EXTRN MSG_OPTIONS_FIRST: ABS
	     EXTRN MSG_OPTIONS_LAST: ABS

BLANK	     EQU  " "		   ;AN002;WIPE OUT SWITCH TO AVOID DUPLICATE

CSEG	     SEGMENT PARA PUBLIC 'CODE' ;AN000;
	     ASSUME CS:CSEG,DS:CSEG,ES:CSEG,SS:CSEG ;AN000;

	     EXTRN SENDMSG:NEAR    ;AN000;USES MSG DESCRIPTOR TO DRIVE MESSAGE HANDLR
	     EXTRN SYSPARSE:NEAR   ;AN000;SYSTEM COMMAND LINE PARSER

	     EXTRN EXITFL:BYTE	   ;AN000;ERRORLEVEL RETURN CODE

	     EXTRN SOURCE_DRIVE:BYTE ;AN000;FIRST DRIVE LETTER SPECIFIED IN PARMS
	     EXTRN USER_OPTION:BYTE ;AN000;NO OPTION (-1)  /1 (1), INVALID (9)
NO_OPTION    EQU  -1		   ;AN000;OPTION NOT SPECIFIED
OPTION_1     EQU  1		   ;AN000;OPTION "/1" SPECIFIED
OPTION_8     EQU  1		   ;AN000;OPTION "/8" SPECIFIED
	     EXTRN USER_OPTION_8:BYTE ;AN000;NO OPTION (-1)  /8 (1), INVALID (9)

	     EXTRN MSGNUM_PARSE:WORD ;AN000;MESSAGE DESCRIPTOR FOR ALL PARSE ERRORS
	     EXTRN MSGNUM_INVALID_PARM2:WORD ;AN005;HELP MESSAGE DESCRIPTOR
	     EXTRN SUBLIST_PARSE:WORD ;AN003;POINTS TO INVALID PARM
; =  =	=  =  =  =  =  =  =  =	=  =

CURRENT_PARM DW   81H		   ;AN000;POINTER INTO COMMAND OF NEXT OPERAND
	     PUBLIC CURRENT_PARM   ;AN000;

ORDINAL      DW   0		   ;AN000;ORDINAL NUMBER OF WHICH PARM TO PARSE
	     PUBLIC ORDINAL	   ;AN000;

; =  =	=  =  =  =  =  =  =  =	=  =
	     HEADER <DOS COMMAND LINE PARSER CONTROL BLOCKS> ;AN000;

;INPUT PARAMETERS CONTROL BLOCK, POINTED TO BY ES:DI WHEN CALLING PARSER

	     PUBLIC PARMS	   ;AN000;LET LINK MAKE PARMS BLOCK ADDRESSABLE
PARMS	     LABEL BYTE 	   ;AN000;PARMS CONTROL BLOCK
	     DW   PARMSX	   ;AN000;POINTER TO PARMS EXTENSION
	     DB   0		   ;AN000; NUMBER OF STRINGS (0, 1, 2)
				   ; NEXT LIST WOULD BE EXTRA DELIM LIST
				   ;  (,& WHITESPACE ALWAYS)
				   ; NEXT LIST WOULD BE EXTRA END OF LINE LIST
				   ;  (CR,LF,0 ALWAYS)

;SYSTEM PARSER PARAMETER EXTENSION CONTROL BLOCK
PARMSX	     LABEL BYTE 	   ;AN000;PARMS EXTENSION CONTROL BLOCK
	     DB   0,2		   ;AN000; MIN, MAX POSITIONAL OPERANDS ALLOWED
	     DW   CONTROL_POS	   ;AN000; DESCRIPTION OF POSITIONAL 1
	     DW   CONTROL_POS	   ;AN000; DESCRIPTION OF POSITIONAL 2

	     DB   1		   ;AN000; MAX SWITCH OPERANDS ALLOWED
	     DW   CONTROL_SW	   ;AN000; DESCRIPTION OF SWITCH

	     DB   0		   ;AN000; MAX KEYWORD OPERANDS ALLOWED
				   ; THERE IS NO CONTROL BLOCK
				   ;  DEFINING KEYWORDS

; = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
	     HEADER <POSITIONAL PARM DESCRIPTOR BLOCK> ;AN000;
;PARSER CONTROL BLOCK DEFINING THE ONLY POSITIONAL PARAMETER, OPTIONAL

;FIRST POSITIONAL PARAMETER IS:
;	[D:] - SPECIFY THE SOURCE DRIVE.

	     PUBLIC CONTROL_POS    ;AN000;LET LINK MAKE THIS ADDRESSABLE
CONTROL_POS  LABEL BYTE 	   ;AN000;FIRST POSITIONAL DESCRIPTOR FOR FILESPEC,
				   ; OPTIONAL
	     DW   0101H 	   ;AN000; CONTROLS TYPE MATCHED
				   ; SELECTED BITS: "DRIVE ONLY" AND "OPTIONAL"

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

	     DW   0000H 	   ;AN000;FUNCTION_FLAGS (NO CAPITALIZATION NEEDED)
				   ; 0001H=CAP RESULT BY FILE TABLE
				   ; 0002H=CAP RESULT BY CHAR TABLE
				   ; 0010H=REMOVE ":" AT END
	     DW   RESULT1	   ;AN000; RESULT BUFFER
	     DW   NOVALS	   ;AN000; NO VALUE LISTS
	     DB   0		   ;AN000; NUMBER OF KEYWORD/SWITCH SYNONYMS
				   ;   IN FOLLOWING LIST

;VALUE CONTROL BLOCK FOR THE POSITIONAL PARAMETERS
NOVALS	     DB   0		   ;AN000;NO VALUE DEFINITIONS

;RESULTS CONTROL BLOCK FOR THE POSITIONAL PARAMETER
RESULT1      LABEL BYTE 	   ;AN000; BELOW FILLED IN FOR DEFAULTS
	     DB   3		   ;AN000; TYPE RETURNED: 0=RESERVED,
				   ;	   1=NUMBER, 2=LIST INDEX,
				   ;	   3=STRING, 4=COMPLEX,
				   ;	   5=FILESPEC, 6=DRIVE
				   ;	   7=DATE, 8=TIME
				   ;	   9=QUOTED STRING
RESULT_TAG   DB   0FFH		   ;AN000; MATCHED ITEM TAG
	     DW   0		   ;AN000;POINTER TO SYNONYM

RESULT_PTR1  DB   ?		   ;AN000;DRIVE NUMBER (A=1, B=2, ETC)

; = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
	     HEADER <SWITCH PARM DESCRIPTOR BLOCK> ;AN000;
;PARSER CONTROL BLOCK DEFINING THE TWO SWITCHES, OPTIONAL

;THE SWITCH IS "/1", MEANING ONLY COMPARE THE FIRST SIDE.
;THE SECOND SWITCH IS "/8", MEANING ONLY LOOK AT FIRST 8 SECTORS PER TRACK.

	     PUBLIC CONTROL_SW	   ;AN000;LET LINK MAKE THIS ADDRESSABLE
CONTROL_SW   LABEL BYTE 	   ;AN000;SWITCH DESCRIPTOR FOR /1 OR /8
	     DW   0000H 	   ;AN000; CONTROLS TYPE MATCHED
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

	     DW   0000H 	   ;AN000;FUNCTION_FLAGS (NO CAPITALIZATION)
				   ; 0001H=CAP RESULT BY FILE TABLE
				   ; 0002H=CAP RESULT BY CHAR TABLE
				   ; 0010H=REMOVE ":" AT END

	     DW   RESULTSW1	   ;AN000; RESULT BUFFER
	     DW   NOVALS	   ;AN000; VALUE LISTS
	     DB   3		   ;AN000; NUMBER OF KEYWORD/SWITCH SYNONYMS
				   ;   IN FOLLOWING LIST
SINGLE_SIDED LABEL BYTE 	   ;AN002;
SW_1	     DB   "/1",0	   ;AN000; IF n >0, KEYWORD 1
SW_8	     DB   "/8",0	   ;AN000;SECOND KEYWORD
SW_?	     DB   "/?",0           ; third switch

PUBLIC 	     Q_BREAK
Q_BREAK	     DB    0               ; flag saying we broke out early due to ?

;RESULTS CONTROL BLOCK FOR THE SWITCHES
RESULTSW1    LABEL BYTE 	   ;AN000; BELOW FILLED IN FOR DEFAULTS
	     DB   3		   ;AN000; TYPE RETURNED: 0=RESERVED,
				   ;	   1=NUMBER, 2=LIST INDEX,
				   ;	   3=STRING, 4=COMPLEX,
				   ;	   5=FILESPEC, 6=DRIVE
				   ;	   7=DATE, 8=TIME
				   ;	   9=QUOTED STRING
	     DB   0FFh		   ;AN000; MATCHED ITEM TAG

RESULTSWSYN  DW   0		   ;AN000; SYNONYM POINTER (BASED ON ES:)
RESULT_PTR2  DD   ?		   ;AN000; OFFSET OF STRING VALUE
; =  =	=  =  =  =  =  =  =  =	=  =
	     PATHLABL DCOMPPAR	   ;AN013;
	     HEADER <PARSER - ASK SYSPARM TO DECODE PARAMETERS> ;AN000;
;  $SALUT (4,4,9,36)		   ;AN000;
PARSER PROC NEAR		   ;AN000;
   PUBLIC PARSER		   ;AN000;

;INPUT: "CURRENT_PARM" = OFFSET TO NEXT PARM IN COMMAND STRING
;	"ORDINAL" = COUNT OF NEXT PARM TO PARSE
;	PSP+81H = TEXT OF DOS COMMAND LINE PARMS STRING
;OUTPUT: "SOURCE_DRIVE" = A=1, B=2, 0 IF NONE
;	 "TARGET_DRIVE" = A=1, B=2, 0 IF NONE
;	 "USER_OPTION" = 01 ON IF /1, -1 IF /1 NOT SPECIFIED.
;	 "USER_OPTION_8" = 01 ON IF /8, -1 IF /8 NOT SPECIFIED.
;	IF ERROR, ERROR MESSAGE IS DISPLAYED, AND "EXITFL" HAS "EXPAR".
; =  =	=  =  =  =  =  =  =  =	=  =
   MOV	USER_OPTION,NO_OPTION	   ;AN000;SET DEFAULT, SWITCH NOT FOUND
   MOV	USER_OPTION_8,NO_OPTION    ;AN000;SET DEFAULT, SWITCH NOT FOUND

   JMP SHORT $$SS1
$$DO1:
				   ;LOOKING AT RETURN CODE FROM SYSPARSE...	;AN000;
       CMP  AX,SYSPRM_EX_OK	   ;AN000;WERE THERE ANY ERRORS?
   JE $$IF1
       CALL PARSE_ERROR 	   ;AN000;DISPLAY REASON FOR ERROR

   JMP $$SR1
$$IF1:
       MOV  ORDINAL,CX		   ;AN000;SAVE UPDATED COUNT
       MOV  CURRENT_PARM,SI	   ;AN000;REMEMBER HOW FAR I GOT
       MOV  BX,DX		   ;AN000;SET DATA BASE REG TO POINT TO THIS OPERAND
       CMP  BX,OFFSET RESULT1	   ;AN000;WAS POSITIONAL PARM SPECIFIED?
       JNE $$IF4
	   MOV	SI,CX		   ;AN000;USE COUNT OF POSITIONALS AS INDEX
	   MOV	AL,RESULT_PTR1	   ;AN000;GET VALUE OF DRIVE (A=1, B=2, ETC)
	   MOV	SOURCE_DRIVE-1[SI],AL ;AN000;SAVE RESPONSE DRIVE VALUE
				   ;IN EITHER SOURCE_DRIVE OR TARGET_DRIVE
				   ;ACCORDING TO ORDINAL IN SI (FROM CX)
        			   ;AN000;SINCE NOT POSITIONAL PARM SPECIFIED
       JMP SHORT $$EN4
$$IF4:
	   MOV  AL,SW_?+BYTE
	   MOV	BX,RESULTSWSYN
	   CMP	[BX]+BYTE,AL
	   JNE  NXT
	   CALL DISPLAY_OPTIONS	   ; display the options message lines
	   MOV	EXITFL,0           ; EXOK= 0
	   MOV  Q_BREAK,1          ; set a flag for "begin" to check saying
                                   ; we broke out early because of a /? option
	   MOV  DX,FINE            ; say that parsing went ok
	   JMP  $$SR1

NXT:	   MOV	AL,SINGLE_SIDED+BYTE ;AN000;GET ID PORTION OF SWITCH
	   MOV	BX,RESULTSWSYN	   ;AN000;GET OFFSET TO MATCHING SWITCH
	   CMP	[BX]+BYTE,AL	   ;AN000;WAS IT /1?
				   ;AN000;YES IT WAS /1
	   JNE $$IF6
	       MOV  SW_1,BLANK	   ;AN002;AVOID GETTING /1 AGAIN
	       MOV  USER_OPTION,OPTION_1 ;AN000;MUST HAVE BEEN THE SWITCH, /1
				   ;AN000;SINCE IT WAS NOT /1, MUST BE /8
	   JMP SHORT $$EN6
$$IF6:
	       MOV  SW_8,BLANK	   ;AN002;AVOID GETTING /8 AGAIN
	       MOV  USER_OPTION_8,OPTION_8 ;AN000;REPORT BACK, IT WAS /8
$$EN6:
$$EN4:
$$SS1:
       LEA  DI,PARMS		   ;AN000; ES:DI = PARSE CONTROL DEFINITON
       MOV  SI,CURRENT_PARM	   ;AN000; DS:SI = COMMAND STRING, NEXT PARM
       XOR  DX,DX		   ;AN000; RESERVED, INIT TO ZERO
       MOV  CX,ORDINAL		   ;AN000; OPERAND ORDINAL, INITIALLY ZERO
       CALL SYSPARSE		   ;AN000;LOOK AT DOS PARMS
				   ; AX=EXIT CODE
				   ; BL=TERMINATED DELIMETER CODE
				   ; CX=NEW OPERAND ORDINAL
				   ; SI=SET TO PAST SCANNED OPERAND
				   ; DX=SELECTED RESULT BUFFER
       CMP  AX,SYSPRM_EX_EOL	   ;AN000; IS THAT THE END OF THE PARMS?
				   ;IF NOT, LOOP BACK AND FIND OUT
				   ; WHAT THAT PARM IS
				   ;AN000;END OF LIST
       JE   SKIPO
       JMP  $$DO1
SKIPO:  MOV  DX,FINE		   ;AN000;REPORT THAT PARSER WENT OK
				   ;AN000;FINISHED WITH DOS COMMAND LINE
$$SR1:
   RET				   ;AN000;RETURN TO CALLER
PARSER ENDP			   ;AN000;

	 HEADER <DISPLAY_OPTIONS - DISPLAY OPTIONS MSG>
; =  =	=  =  =  =  =  =  =  =	=  =
; SUBROUTINE NAME :  DISPLAY_OPTIONS                                          *
;                                                                             *
; INPUT 	  :  NONE						      *
;                                                                             *
; OUTPUT	  :  NONE                                                     *
;                                                                             *
; FUNCTION        :  Displays all the lines of the user options message on    *
;                    standard output.                                         *
;                                                                             *
; =  =	=  =  =  =  =  =  =  =	=  =

	PUBLIC	DISPLAY_OPTIONS
DISPLAY_OPTIONS	PROC NEAR

	push	di
	mov	di, offset MSGNUM_OPTIONS	; get message
DO_LOOP:
	call	SENDMSG			; send this line
	cmp	word ptr[MSGNUM_OPTIONS], MSG_OPTIONS_LAST	; last msg?
	je	DO_DONE			; done if so
	inc	word ptr[MSGNUM_OPTIONS]	; else bump msg number
	jmp	short DO_LOOP		; and go display it.
DO_DONE:
	pop	di
	ret

DISPLAY_OPTIONS	ENDP

; =  =	=  =  =  =  =  =  =  =	=  =
   HEADER <PARSE_ERROR - DISPLAY REASON FOR PARSE ERROR> ;AN000;
PARSE_ERROR PROC NEAR		   ;AN000;
;INPUT:  AX - ERROR NUMBER RETURNED FROM PARSE.
;	"CURRENT_PARM" - OFFSET INTO COMMAND OF WHERE TO START LOOKING FOR PARM
;OUTPUT: APPROPRIATE ERROR MESSAGE IS PREPARED FOR DISPLAY.
;	 DX IS SET TO OFFSET OF PARSE ERROR DESCRIPTOR.
; =  =	=  =  =  =  =  =  =  =	=  =

   MOV	MSGNUM_PARSE,AX 	   ;AN000;PASS MESSAGE NUMBER TO DESCRIPTOR
   MOV	EXITFL,EXPAR		   ;AN000;ERRORLEVEL CODE TO "PARM ERROR"
   MOV	AX,CURRENT_PARM 	   ;AN003;GET POINTER TO START OF BAD PARM
   CMP	SI,AX			   ;AN003;HAS THE INDEX TO COMMAND LINE MOVED?
;  $IF	NE			   ;AN003;YES, THERE IS A FAULTY PARM
   JE $$IF13
       MOV  BYTE PTR [SI],NUL	   ;AN003;DELIMIT THE BAD PARM
       MOV  SUBLIST_PARSE.SUB_VALUE,AX ;AN000;POINT SUBLIST TO BAD PARM

       MOV  MSGNUM_PARSE.MSG_SUBLIST,OFFSET SUBLIST_PARSE ;AN003;POINT TO SUBLIST
       MOV  MSGNUM_PARSE.MSG_COUNT,ONE_SUBS ;AN003;SET COUNT OF SUBLISTS TO ONE
;  $ENDIF			   ;AN003;INDEX MOVED?
$$IF13:
   MOV	DI,OFFSET MSGNUM_PARSE	   ;AC005;OFFSET TO PARSE ERR DESCRIPTOR
   CALL SENDMSG 		   ;AN005;DISPLAY ERROR MSG

   MOV	DX,OFFSET MSGNUM_INVALID_PARM2 ;AN005;PASS BACK OFFSET TO HELP MSG
   RET				   ;AN000;RETURN TO CALLER
PARSE_ERROR ENDP		   ;AN000;
; =  =	=  =  =  =  =  =  =  =	=  =
   PATHLABL DCOMPPAR		   ;AN013;
CSEG ENDS			   ;AN000;
   END				   ;AN000;
