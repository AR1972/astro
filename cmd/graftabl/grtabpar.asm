;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1981 - 1991
; *                      All Rights Reserved.
; */
	PAGE	90,132			;A2
	TITLE	GRTABPAR.SAL - LOOK AT COMMAND LINE PARMS ;
;****************** START OF SPECIFICATIONS *****************************
; MODULE NAME: GRTABPAR.SAL

; DESCRIPTIVE NAME: Handle the definition of the DOS command line parameters
;		    and the interface to the DOS system PARSER.

;FUNCTION: The static data areas are prescribed by the DOS system PARSER
;	   to define the several parameters presented to GRAFTABL.  These
;	   data areas are passed to the PARSER, and its responses checked
;	   to determine the nature of the user's specifications.  Any errors
;	   found in the user's parameters are defined in messages back
;	   to the user.

; ENTRY POINT: PARSER, near

; INPUT: (DOS COMMAND LINE PARAMETERS)

;	      [d:][path] GRAFTABL  [cp|?|/STA|/STATUS]

;	 WHERE
;	      [d:][path] - Path where the GRAFTABL command resides.

;	      [cp]	 - Codepage number to be loaded.  If blank,
;			   437 is assumed

;	      [?]	 - This requests a display of the parameters
;			   supported by the GRAFTABL command.

;	      [/STATUS]  - (May be shortened to just /STA.)  This
;			   requests the current codepage already
;			   loaded by a previous GRAFTABL, if any,
;			   be displayed.

;	Upon entry to PARSER in this module,
;	"CURRENT_PARM" = offset to start of parm text in command string
;	"ORDINAL" = initialized to zero
;	PSP+81H = text of DOS command line parms string

; EXIT-NORMAL: If a Code Page number was specified
;		  BX = Offset to language table to be loaded
;		  DX = Integer value of Code Page specified
;	       If /STATUS (or /STA) was specified
;		  BX = 0
;	       If Question mark was specified
;		  BX=-1

; EXIT-ERROR: If there was any problem with the parms,
;	      the question mark is assumed, and the appropriate
;	      PARSE error message is displayed.
;	      The Errorlevel code of "EXPAR" (3), meaning: "PARM ERROR",
;	      set in "EXITFL", is requested to be returned to the user.

; INTERNAL REFERENCES:
;    ROUTINES:
;	PARSE_ERROR:NEAR Display the appropriate Parse error message.

;    DATA AREAS:
;	The several parameter control blocks, defined by the System
;	PARSER interface, defining the GRAFTABL parameters.

; EXTERNAL REFERENCES:
;    ROUTINES:
;	SENDMSG:NEAR	Uses Msg Descriptor to drive message handler.
;	SYSPARSE:NEAR	System Command Line Common Parser.

;    DATA AREAS:
;	TABLEUS:BYTE	First table of Fonts, happens to be the USA version.
;	  (Format of language Font defined in LANGUAGE STRUC.)
;	EXITFL:BYTE	Errorlevel return code.
;	MSGNUM_PARSE:WORD Message descriptor for all parse errors.
;	ACTIVECPID:WORD Pointer to CPID entry of active entry, where the
;	CPID	table is a set of 4 byte asciiz strings,
;			each defining the number of a Code Page font.

; NOTES:
;	 This module should be processed with the SALUT preprocessor
;	 with the re-alignment not requested, as:

;		SALUT GRTABPAR,NUL

;	 To assemble these modules, the alphabetical or sequential
;	 ordering of segments may be used.

;	 For LINK instructions, refer to the PROLOG of the main module,
;	 GRTAB.SAL.

; REVISION HISTORY:
;	A001 PTM 382 display "ACTIVE" OR "PREVIOUS" CP.
;	A002 PTM 474 Avoid duplicate switches
;	A003 PTM 538 Display parm in error

;****************** END OF SPECIFICATIONS *****************************
; =  =	=  =  =  =  =  =  =  =	=  =
	INCLUDE PATHMAC.INC
; =  =	=  =  =  =  =  =  =  =	=  =
HEADER	MACRO	TEXT
.XLIST
	SUBTTL	TEXT
.LIST
	PAGE
	ENDM
; =  =	=  =  =  =  =  =  =  =	=  =
;		      $SALUT (4,23,28,36)
		      EXTRN CPID_L:ABS ;BYTES PER CPID ENTRY

MSG_DESC	      STRUC	   ;
MSG_NUM 	      DW   ?	   ;MESSAGE NUMBER (TO AX)
MSG_HANDLE	      DW   ?	   ;HANDLE OF OUTPUT DEVICE (TO BX)
MSG_SUBLIST	      DW   ?	   ;POINTER TO SUBLIST (TO SI)
MSG_COUNT	      DW   ?	   ;SUBSTITUTION COUNT (TO CX)
MSG_CLASS	      DW   ?	   ;MESSAGE CLASS (IN HIGH BYTE, TO DH)
				   ; LOW BYTE HAS 0 (FUNCTION "NO INPUT", TO DL);
MSG_DESC	      ENDS	   ;

ONE_SUBS	      EQU  1	   ;NUMBER OF VARIABLES

SUBLIST 	      STRUC	   ;
SUB_SIZE	      DB   ?	   ;SUBLIST SIZE (POINTER TO NEXT SUBLIST)
SUB_RES 	      DB   ?	   ;RESERVED
				   ;NEXT FIELD IS TO BE USED AS A DOUBLE WORD
SUB_VALUE	      DW   ?	   ;TIME, DATE, OR PTR TO DATA ITEM
SUB_VALUE_SEG	      DW   ?	   ;SEG ID OF PTR
				   ;(ABOVE FIELD MUST BE FILLED AT EXECUTION TIME
				   ; IF THIS IS A .COM FILE)
SUB_ID		      DB   ?	   ;N OF %N
SUB_FLAGS	      DB   ?	   ;DATA TYPE FLAGS
SUB_MAX_WIDTH	      DB   ?	   ;MAXIMUM FIELD WIDTH (0=UNLIMITED)
SUB_MIN_WIDTH	      DB   ?	   ;MINIMUM FIELD WIDTH
SUB_PAD_CHAR	      DB   ?	   ;CHARACTER FOR PAD FIELD
				   ; CAN BE " ", "0" OR ",".
				   ; "," CAUSES INSERTION OF THE ACTIVE
				   ; THOUSANDS SEPARATOR BETWEEN EVERY 3 DIGITS.
SUBLIST 	      ENDS	   ;

;		LOCAL EQUATES
ZERO		      EQU  0	   ;COMPARAND FOR MISSING PARMS
RETCODE_QUESTION      EQU  -1	   ;VALUE IN BX, IF PARM=?
RETCODE_PARSE_ERROR   EQU  1	   ; Error during parse
EXPAR		      EQU  3	   ;RETURN TO DOS, INVALID DOS COMND LINE PARMS
CR		      EQU  13	   ;CARRIAGE RETURN
BLANK		      EQU  " "	   ;AVOIDS DUPLICATES SWITCHES
NUL		      EQU  0	   ;ASCIIZ DELIMITER
; =  =	=  =  =  =  =  =  =  =	=  =
;		EXIT CODES FROM SYSPARSE (WHEN CY=0)

SYSPRM_EX_OK	      EQU  0	   ; no error
SYSPRM_EX_MANY	      EQU  1	   ; too many operands
SYSPRM_EX_MISSING     EQU  2	   ; required operand missing
SYSPRM_EX_NOT_SWLIST  EQU  3	   ; not in switch list provided
SYSPRM_EX_NOT_KEYLIST EQU  4	   ; not in keyword list provided
SYSPRM_EX_RANGE       EQU  6	   ; out of range specified
SYSPRM_EX_VALUE       EQU  7	   ; not in value list provided
SYSPRM_EX_STRING      EQU  8	   ; not in string list provided
SYSPRM_EX_SYNTAX      EQU  9	   ; syntax error
SYSPRM_EX_EOL	      EQU  -1	   ; end of command line
; =  =	=  =  =  =  =  =  =  =	=  =
		      HEADER <STRUC - DEFINITIONS OF EXTERNAL CONTROL BLOCKS> ;
LANGUAGE	      STRUC	   ;DEFINITION OF EACH LANGUAGE TABLE
LANCHAR 	      DB   1024 DUP(?) ;8 BYTES PER EACH OF 128 CHARACTERS
LANID		      DW   ?	   ;TWO BYTE CODEPAGE ID, TO MATCH
				   ; GRAFTABL CMD LINE PARM
LANNAME 	      DB   14 DUP(?) ;ASCIIZ STRING NAME OF LANGUAGE
LANGUAGE	      ENDS	   ;
; =  =	=  =  =  =  =  =  =  =	=  =
PSP		      STRUC	   ;
		      DB   80H DUP (?) ;SKIP OVER FIRST HALF OF PSP
PSP_PARMLEN	      DB   ?	   ;NUMBER OF BYTES IN DOS COMMAND LINE
PSP_COMMAND	      DB   127 DUP(?) ;TEXT OF DOS COMMAND LINE
PSP		      ENDS	   ;
; =  =	=  =  =  =  =  =  =  =	=  =
		      HEADER <PARSING WORKAREAS> ;
;	     $SALUT (4,14,19,36)   ;
CSEG	     SEGMENT PARA PUBLIC   ;
	     ASSUME CS:CSEG,DS:CSEG,ES:CSEG,SS:CSEG ;

	     EXTRN SENDMSG:NEAR    ;USES MSG DESCRIPTOR TO DRIVE MESSAGE HANDLR
	     EXTRN SYSPARSE:NEAR   ;SYSTEM COMMAND LINE PARSER

	     EXTRN TABLEUS:BYTE    ;FIRST TABLE OF FONTS
	     EXTRN EXITFL:BYTE	   ;ERRORLEVEL RETURN CODE
	     EXTRN MSGNUM_PARSE:WORD ;MESSAGE DESCRIPTOR FOR ALL PARSE ERRORS
	     EXTRN SUBLIST_PARSE:WORD ;POINTS TO INVALID PARM
	     EXTRN ACTIVECPID:WORD ;POINTER TO CPID ENTRY OF ACTIVE ENTRY
; =  =	=  =  =  =  =  =  =  =	=  =

CURRENT_PARM DW   81H		   ;POINTER INTO COMMAND OF NEXT OPERAND
	     PUBLIC CURRENT_PARM   ;

ORDINAL      DW   0		   ;ORDINAL NUMBER OF WHICH PARM TO PARSE
	     PUBLIC ORDINAL	   ;

FIRST_TIME   DB   0		   ;INDICATES IF A PARM ALREADY FOUND
PARSE_RESULT DW   0		   ;TEMP, HOLDS BX TO BE RETURNED
;	       If a Code Page number was specified
;		  BX = Offset to language table to be loaded
;		  DX = Integer value of Code Page specified
;	       If /STATUS (or /STA) was specified
;		  BX = 0
;	       If Question mark was specified
;		  BX=-1
; =  =	=  =  =  =  =  =  =  =	=  =
	     HEADER <DOS COMMAND LINE PARSER CONTROL BLOCKS> ;

;INPUT PARAMETERS CONTROL BLOCK, POINTED TO BY ES:DI WHEN CALLING PARSER

	     PUBLIC PARMS	   ;LET LINK MAKE PARMS BLOCK ADDRESSABLE
PARMS	     LABEL BYTE 	   ;PARMS CONTROL BLOCK
	     DW   PARMSX	   ;POINTER TO PARMS EXTENSION
	     DB   0		   ; NUMBER OF STRINGS (0, 1, 2)
				   ; NEXT LIST WOULD BE EXTRA DELIM LIST
				   ;  (,& WHITESPACE ALWAYS)
				   ; NEXT LIST WOULD BE EXTRA END OF LINE LIST
				   ;  (CR,LF,0 ALWAYS)

;SYSTEM PARSER PARAMETER EXTENSION CONTROL BLOCK
PARMSX	     LABEL BYTE 	   ;PARMS EXTENSION CONTROL BLOCK
	     DB   0,1		   ; MIN, MAX POSITIONAL OPERANDS ALLOWED
	     DW   CONTROL_POS	   ; DESCRIPTION OF POSITIONAL 1

	     DB   2		   ; MAX SWITCH OPERANDS ALLOWED
	     DW   CONTROL_SW	   ; DESCRIPTION OF SWITCH 1 (/STATUS)
	     DW   CONTROL_SW2	   ;       DESCRIPTION OF SWITCH 2 (/?)

	     DB   0		   ; MAX KEYWORD OPERANDS ALLOWED
				   ; THERE IS NO CONTROL BLOCK
				   ;  DEFINING KEYWORDS

; = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
	     HEADER <POSITIONAL PARM DESCRIPTOR BLOCK> ;
;PARSER CONTROL BLOCK DEFINING THE ONLY POSITIONAL PARAMETER, OPTIONAL

;FIRST POSITIONAL PARAMETER IS:
;	[cp|?]	THE CODEPAGE NUMBER, OR THE QUESTION MARK

	     PUBLIC CONTROL_POS    ;LET LINK MAKE THIS ADDRESSABLE
CONTROL_POS  LABEL BYTE 	   ;FIRST POSITIONAL DESCRIPTOR FOR FILESPEC,
				   ; OPTIONAL
	     DW   2001H 	   ; CONTROLS TYPE MATCHED
				   ; SELECTED BITS: "SIMPLE STRING" AND "OPTIONAL"

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

	     DW   0000H 	   ;FUNCTION_FLAGS ("NO CAPS")
				   ; 0001H=CAP RESULT BY FILE TABLE
				   ; 0002H=CAP RESULT BY CHAR TABLE
				   ; 0010H=REMOVE ":" AT END
	     DW   RESULT	   ; RESULT BUFFER
	     DW   VALS		   ; VALUE LISTS
	     DB   0		   ; NUMBER OF KEYWORD/SWITCH SYNONYMS
				   ;   IN FOLLOWING LIST

;VALUE CONTROL BLOCK FOR THE POSITIONAL PARAMETERS
VALS	     DB   3		   ;NUMBER OF VALS DEFINITIONS (0-3)
	     DB   0		   ;NUMBER OF RANGES (NONE)
	     DB   0		   ;NUMBER OF NUMERIC CHOICES (NONE)
	     DB   6		   ;NUMBER OF CHOICE STRINGS


;THE ORDER OF THESE VALUE DEFINITIONS IS SIGNIFICANT.  THEIR ORDER MUST
;MATCH THE ORDER IN WHICH THE CORRESPONDING FONT TABLE MODULES ARE
;LINKED TOGETHER.  THUE THE "VALUE RETURNED" OF 1 MEANS THE FIRST LINKED
;FONT TABLE, THE VALUE OF 5 MEANS THE FIFTH LINKED FONT TABLE, ETC.

VAL_437      EQU  1		   ;VALUE RETURNED
	     DB   VAL_437	   ; FOR:
	     DW   S437		   ;SPECIFIC CHOICE IF STRING

VAL_850      EQU  2		   ;VALUE RETURNED
	     DB   VAL_850	   ; FOR:
	     DW   S850		   ;SPECIFIC CHOICE IF STRING

VAL_860      EQU  3		   ;VALUE RETURNED
	     DB   VAL_860	   ; FOR:
	     DW   S860		   ;SPECIFIC CHOICE IF STRING

VAL_863      EQU  4		   ;VALUE RETURNED
	     DB   VAL_863	   ; FOR:
	     DW   S863		   ;SPECIFIC CHOICE IF STRING

VAL_865      EQU  5		   ;VALUE RETURNED
	     DB   VAL_865	   ; FOR:
	     DW   S865		   ;SPECIFIC CHOICE IF STRING

VAL_852	     EQU  6		   ;VALUE RETURNED
	     DB   VAL_852	   ; FOR:
	     DW   S852		   ;SPECIFIC CHOICE IF STRING

;		SET OF ASCIIZ STRINGS, DEFINING THE POSITIONAL PARAMETER
S437	     DB   "437",0	   ;USA
S850	     DB   "850",0	   ;MULTI-LINGUAL
S860	     DB   "860",0	   ;PORTUGUESE
S863	     DB   "863",0	   ;CANADIAN FRENCH
S865	     DB   "865",0	   ;NORDIC
S852	     DB   "852",0	   ;LATIN-2

;RESULTS CONTROL BLOCK FOR THE POSITIONAL PARAMETER, AND SWITCH PARAMETER
RESULT	     LABEL BYTE 	   ; BELOW FILLED IN FOR DEFAULTS
	     DB   3		   ; TYPE RETURNED: 0=RESERVED,
				   ;	   1=NUMBER, 2=LIST INDEX,
				   ;	   3=STRING, 4=COMPLEX,
				   ;	   5=FILESPEC, 6=DRIVE
				   ;	   7=DATE, 8=TIME
				   ;	   9=QUOTED STRING
RESULT_TAG   DB   0FFH		   ; MATCHED ITEM TAG
RESULT_SYN   DW   0		   ;POINTER TO SYNONYM

RESULT_PTR   DD   ?		   ;OFFSET OF STRING VALUE

; = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
	     HEADER <SWITCH PARM DESCRIPTOR BLOCK> ;
;PARSER CONTROL BLOCK DEFINING THE SWITCHES, OPTIONAL

;THE SWITCH IS "/STA" OR "/STATUS".  WHEN REQUESTED, IT MEANS TO IDENTIFY
;WHAT CODEPAGE IS CURRENTLY SUPPORTED BY THE PREVIOUS GRAFTABL, IF ANY.
	     PUBLIC CONTROL_SW	   ;LET LINK MAKE THIS ADDRESSABLE
CONTROL_SW   LABEL BYTE 	   ;SWITCH DESCRIPTOR FOR /STA
	     DW   0001H 	   ; CONTROLS TYPE MATCHED
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

	     DW   0000H 	   ;FUNCTION_FLAGS ("NO CAPS")
				   ; 0001H=CAP RESULT BY FILE TABLE
				   ; 0002H=CAP RESULT BY CHAR TABLE
				   ; 0010H=REMOVE ":" AT END

	     DW   RESULT	   ; RESULT BUFFER
				   ;(USE SAME RESULT BUF AS DID POSITIONAL PARM)
	     DW   NOVALS	   ; VALUE LISTS
	     DB   2		   ; NUMBER OF KEYWORD/SWITCH SYNONYMS
				   ;   IN FOLLOWING LIST
SW_STA	     DB   "/STA",0	   ; IF n >0, KEYWORD 1
SW_STATUS    DB   "/STATUS",0	   ;SECOND KEYWORD

;THE SWITCH IS "/?".  WHEN REQUESTED, IT MEANS TO DISPLAY
;THE COMMAND LINE OPTIONS FOR THIS PROGRAM.

	     PUBLIC CONTROL_SW2
CONTROL_SW2  LABEL BYTE
	     DW   0000H		   ; Match Mask
	     DW   0000H 	   ; Function Mask
	     DW   RESULT	   ; ptr to RESULT BUFFER
	     DW   NOVALS	   ; ptr to VALUE LISTS
	     DB   1		   ; count of synonyms in following list
SW2_SYN	     DB   "/?",0	   ; only this one synonum

;VALUE CONTROL BLOCK FOR THE SWITCHES

NOVALS	     LABEL BYTE 	   ;
	     DB   0		   ; NUMBER OF VALUE DEFINITIONS (0 - 3)
; =  =	=  =  =  =  =  =  =  =	=  =
	     PATHLABL GRTABPAR	   ;
	     HEADER <PARSER - ASK SYSPARM TO DECODE PARAMETERS> ;
;  $SALUT (4,4,9,36)		   ;
PARSER PROC NEAR		   ;
   PUBLIC PARSER		   ;

;INPUT: "CURRENT_PARM" = OFFSET TO NEXT PARM IN COMMAND STRING
;	"ORDINAL" = COUNT OF NEXT PARM TO PARSE
;	PSP+81H = TEXT OF DOS COMMAND LINE PARMS STRING
;OUTPUT: IF A CODEPAGE NUMBER WAS SPECIFIED
;	    BX = OFFSET TO LANGUAGE TABLE TO BE LOADED
;	    DX = INTEGER VALUE OF CODEPAGE SPECIFIED
;	 IF /STATUS WAS SPECIFIED
;	    BX = 0
;	 IF QUESTION MARK WAS SPECIFIED
;	    BX=-1
;	 If parse error occurred
;	    BX = 1

;IF THERE WAS ANY PROBLEM WITH THE PARMS,
;"EXITFL" SET TO "EXPAR" TO INDICATE PARM ERROR.

;IT A CP IS SPECIFIED, A STATUS REPORT IS ALWAYS GIVEN TO SAY WHAT WHAT
;THERE PREVIOUSLY, SO IF THE USER SPECIFIED A CP AND THE /STATUS SWITCH,
;THEN THE /STATUS DOES NOTHING BEYOND WHAT WOULD HAVE BEEN DONE ALREADY
;WITH JUST THE CP SPECIFICATION.  I ALWAYS REPORT STATUS.  THE ONLY
;REASON TO HAVE /STATUS IS TO BE ABLE TO SEE WHAT IS THERE ALREADY
;WITHOUT INVOKING THE DEFAULT OF 437 (USA) BY NOT SPECIFYING A CP.
; =  =	=  =  =  =  =  =  =  =	=  =

;  $SEARCH COMPLEX		   ;LOOP THRU COMMAND LINE
   JMP SHORT $$SS1
$$DO1:
				   ;LOOKING AT RET CODE IN AX SET BY SYSPARSE
       CMP  AX,ZERO		   ;WERE THERE ANY ERRORS?
;  $EXITIF NE			   ;HAD A PROBLEM
   JE $$IF1
       CALL PARSE_ERROR 	   ;DISPLAY REASON FOR ERROR

;  $ORELSE LONG 		   ;SINCE NO PROBLEM, SO FAR
   JMP $$SR1
$$IF1:
       MOV  ORDINAL,CX		   ;SAVE UPDATED COUNT

       ; Was /? specified?
       ; If so, give the user the options help message

       cmp	[RESULT_SYN], offset SW2_SYN	; /? specified?
       jne	ParserOptionsDone		; skip this if not
       mov	bx, RETCODE_QUESTION		; else flag user help
       jmp	short $$SR1			;  and exit
ParserOptionsDone:

       CMP  FIRST_TIME,ZERO	   ;DO I HAVE A PARM YET?
;      $IF  E			   ;NOT YET, LOOK AT THIS ONE JUST FOUND
       JNE $$IF4
	   MOV	CURRENT_PARM,SI    ;REMEMBER HOW FAR I GOT
	   CMP	RESULT_SYN,ZERO    ;WAS POSITIONAL PARM SPECIFIED?
;	   $IF	E		   ;IF POSITIONAL PARM SPECIFIED,
	   JNE $$IF5
		MOV  CL,RESULT_TAG  ;GET ID OF SPECIFIED PARM
		XOR  CH,CH	   ;CLEAR HIGH BYTE
		   LEA	BX,TABLEUS ;GET WHERE FIRST CODEPAGE STARTS, USING CX
				   ; AS A COUNTER, STEP THRU THE LANGUAGE TABLES
				   ; UNTIL GETTING TO THE SPECIFIED ONE
;		   $DO	COMPLEX    ;
		   JMP SHORT $$SD8
$$DO8:
		       ADD  BX,SIZE LANGUAGE ;POINT TO NEXT TABLE
		       ADD  ACTIVECPID,CPID_L ;SELECT NEXT CPID ENTRY
;		   $STRTDO	   ;
$$SD8:
;		   $ENDDO LOOP	   ;DECREMENT INDEX
		   LOOP $$DO8
				   ;BX-OFFSET TO LANGUAGE TABLE
				   ; INDICATED BY POSITIONAL PARM
;	   $ELSE		   ;SINCE NOT POSITIONAL PARM SPECIFIED
	   JMP SHORT $$EN5
$$IF5:
				   ;RESULT_SYN POINTS TO A SWITCH
	       XOR  BX,BX	   ;  MUST HAVE BEEN THE SWITCH, /STATUS
	       MOV  SW_STA,BLANK   ;AVOID THE DUPLICATION OF THIS SWITCH
	       MOV  SW_STATUS,BLANK ;AVOID THE DUPLICATION OF THIS SWITCH
;	   $ENDIF		   ;POSITIONAL?
$$EN5:
	   INC	FIRST_TIME	   ;INDICATE A PARM HAS BEEN FOUND
;      $ELSE			   ;SINCE ALREADY HAVE A PARM
       JMP SHORT $$EN4
$$IF4:
	   PUSH SI		   ;SAVE NEW INDEX TO COMMAND LINE
	   CALL PARSE_ERROR	   ;FUSS ABOUT TOO MANY PARMS

	   POP	CURRENT_PARM	   ;REMEMBER HOW FAR I GOT

;      $ENDIF			   ;ALREADY HAVE A PARM?
$$EN4:
       MOV  PARSE_RESULT,BX	   ;SAVE THE RESULT OF THIS PARSE
;  $STRTSRCH			   ;
$$SS1:
       LEA  DI,PARMS		   ; ES:DI = PARSE CONTROL DEFINITON
       MOV  SI,CURRENT_PARM	   ; DS:SI = COMMAND STRING, NEXT PARM
       XOR  DX,DX		   ; RESERVED, INIT TO ZERO
       MOV  CX,ORDINAL		   ; OPERAND ORDINAL, INITIALLY ZERO
       CALL SYSPARSE		   ;LOOK AT DOS PARMS
				   ; AX=EXIT CODE
				   ; BL=TERMINATED DELIMETER CODE
				   ; CX=NEW OPERAND ORDINAL
				   ; SI=SET TO PAST SCANNED OPERAND
				   ; DX=SELECTED RESULT BUFFER
       CMP  AX,SYSPRM_EX_EOL	   ; IS THAT THE END OF THE PARMS?
				   ;IF NOT, LOOP BACK AND FIND OUT
				   ; WHAT THAT PARM IS
;  $ENDLOOP E			   ;END OF LIST
   JNE $$DO1
       CMP  FIRST_TIME,ZERO	   ;FIND ANYTHING YET?
;      $IF  E			   ;IF NO PARM SPECIFIED
       JNE $$IF18
	   LEA	BX,TABLEUS	   ;SPECIFY 437 (USA) AS DEFAULT
;      $ELSE			   ;SINCE A PARM WAS FOUND
       JMP SHORT $$EN18
$$IF18:
	   MOV	BX,PARSE_RESULT    ;REMEMBER PARM ALREADY FOUND
;      $ENDIF			   ;
$$EN18:
				   ;BX=-1, "?"; BX=0, "/STATUS";BX>0, CP TABLE
       CMP  BX,ZERO		   ;WAS A CP TABLE FOUND?
;      $IF  A			   ;IF A CP TABLE FOUND
       JNA $$IF21
	   MOV	DX,[BX].LANID	   ;FETCH THE TWO CHAR ID FROM TABLE
;      $ENDIF			   ;
$$IF21:
;  $ENDSRCH			   ;FINISHED WITH DOS COMMAND LINE
$$SR1:
   RET				   ;RETURN TO CALLER
PARSER ENDP			   ;
; =  =	=  =  =  =  =  =  =  =	=  =						;
   HEADER <PARSE_ERROR - DISPLAY REASON FOR PARSE ERROR> ;
PARSE_ERROR PROC NEAR		   ;
;INPUT: "FIRST_TIME" - IF NON-ZERO, FORCE ERROR CODE TO "TOO MANY PARMS"
;	 AX - ERROR NUMBER RETURNED FROM PARSE.
;	 SI - OFFSET INTO COMMAND OF FIRST BYTE BEYOND PARM IN ERROR
;	 "CURRENT_PARM" - OFFSET INTO COMMAND OF WHERE TO START LOOKING FOR PARM
;OUTPUT: APPROPRIATE ERROR MESSAGE IS DISPLAYED.
;	 BX IS SET TO PRETEND THAT THE "?" WAS SPECIFIED.
;	 "EXITFL" SET TO "EXPAR" TO INDICATE PARM ERROR.
; =  =	=  =  =  =  =  =  =  =	=  =

   CMP	FIRST_TIME,ZERO 	   ;ANY PARMS FOUND YET?
;  $IF	NE			   ;IF PARM ALREADY FOUND
   JE $$IF24
       MOV  AX,SYSPRM_EX_MANY	   ;CHANGE RETURN CODE TO "TOO MANY PARMS"
;  $ENDIF			   ;PARMS FOUND?
$$IF24:
   MOV	MSGNUM_PARSE,AX 	   ;PASS MESSAGE NUMBER TO DESCRIPTOR
   MOV	AX,CURRENT_PARM 	   ;GET POINTER TO START OF BAD PARM
   CMP	SI,AX			   ;HAS THE INDEX TO COMMAND LINE MOVED?
;  $IF	NE			   ;YES, THERE IS A FAULTY PARM
   JE $$IF26
       MOV  BYTE PTR [SI],NUL	   ;DELIMIT THE BAD PARM
       MOV  SUBLIST_PARSE.SUB_VALUE,AX ;POINT SUBLIST TO BAD PARM

       MOV  MSGNUM_PARSE.MSG_SUBLIST,OFFSET SUBLIST_PARSE ;POINT TO SUBLIST
       MOV  MSGNUM_PARSE.MSG_COUNT,ONE_SUBS ;SET COUNT OF SUBLISTS TO ONE
;  $ENDIF			   ;INDEX MOVED?
$$IF26:
   LEA	DI,MSGNUM_PARSE 	   ;PASS MESSAGE DESCRIPTOR
   CALL SENDMSG 		   ;DISPLAY ERROR MESSAGE

   MOV	BX,RETCODE_PARSE_ERROR	   ;INDICATE error occurred.
   MOV	EXITFL,EXPAR		   ;ERRORLEVEL CODE TO "PARM ERROR"
   RET				   ;RETURN TO CALLER
PARSE_ERROR ENDP		   ;
; =  =	=  =  =  =  =  =  =  =	=  =
   PATHLABL GRTABPAR		   ;
CSEG ENDS			   ;
   END				   ;
