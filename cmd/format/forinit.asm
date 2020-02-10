;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

;


;*****************************************************************************
;*****************************************************************************
;UTILITY NAME: FORMAT.COM
;
;MODULE NAME: FORINIT.SAL
;
;
;
; �����������Ŀ
; � Main_Init �
; �������������
;   �
;   ������������������Ŀ     ����������������Ŀ
;   ôInit_Input_Output�����´Preload_Messages�
;   ��������������������    �������������������
;   �			    ��������������������Ŀ   �������������������Ŀ
;   �			    ôCheck_For_FS_Switch���´Parse_For_FS_Switch�
;   �			    ����������������������  ����������������������
;   �			    �			    ���������������Ŀ
;   �			    �			    ��EXEC_FS_Format�
;   �			    �			     ����������������
;   �			    ��������������������Ŀ   ���������������Ŀ
;   �			    ��Parse_Command_Line ���ĴInterpret_Parse�
;   �			     ���������������������   �����������������
;   ����������������������Ŀ ������������������Ŀ
;   ôValidate_Target_Drive�´Check_Target_Drive�
;   �����������������������ٳ��������������������
;   �			    ������������������Ŀ
;   �			    ôCheck_For_Network�
;   �			    ��������������������
;   �			    ����������������������Ŀ
;   �			    ��Check_Translate_Drive�
;   �			     �����������������������
;   �������������Ŀ
;   ��Hook_CNTRL_C�
;    ��������������
;
;
; Change List: AN000 - New code DOS 3.3 spec additions
;	       AC000 - Changed code DOS 3.3 spec additions
;*****************************************************************************
;*****************************************************************************

;
;---------------------------------------------------------------------------
;
; M024 : B#5495. Added "Insufficient memory" message when FORMAT cannot
;		allocate memory for FAT, Directory... etc. Reclaimed
;		the msgBadDrive which was not being used. Removed the
;		unwanted EXTRN msgBadDrive. 
;
; 08/05/91 MD   Removed M030 changes.
;
;---------------------------------------------------------------------------
;

data	segment public para 'DATA'


Command_Line db NO
PSP_Segment dw	0

;These should stay togather
; ---------------------------------------
FS_String_Buffer db 13 dup(" ")
FS_String_End db "FMT.EXE",0
Len_FS_String_End equ $ - FS_String_End

;----------------------------------------

Vol_Label_Count  db 80h 			;max. string length
Vol_Label_Len	 db 00h 			;len. entered
Vol_Label_Buffer db 80h dup(0)
Vol_Label_Buffer_Length equ $ - Vol_Label_Buffer

Command_Line_Buffer db 80h dup(0)
Command_Line_Length equ $ - Command_Line_Buffer
Fatal_Error db	0

Command_Old_Ptr dw	?

data	ends

code	segment public para 'CODE'
	assume	cs:code,ds:data,es:data
code	ends

;
;*****************************************************************************
; Include files
;*****************************************************************************
;

.xlist
include bpb.inc
INCLUDE FORCHNG.INC
INCLUDE FORMACRO.INC
INCLUDE SYSCALL.INC
INCLUDE IOCTL.INC
INCLUDE FOREQU.INC
INCLUDE FORPARSE.INC
INCLUDE FORSWTCH.INC

INCLUDE VERSION.INC				;M032


.list

;
;*****************************************************************************
; Public Data
;*****************************************************************************
;

						; M033
						; Why declare variables BEFORE
						; include files?!
data	segment public para 'DATA'

Public	CMCDDFlag

CMCDDFlag		DB	?

data	ends


	Public	FS_String_Buffer
	Public	Command_Line
	Public	Fatal_Error
	Public	Vol_Label_Count
	Public	Vol_Label_Buffer
	Public	PSP_Segment
	Public	Command_Old_Ptr


;
;*****************************************************************************
; Public Routines
;*****************************************************************************
;


	Public	Main_Init

;
;*****************************************************************************
; External Routine Declarations
;*****************************************************************************
;

	Extrn	Main_Routine:Near
	Extrn	SysLoadMsg:Near
	Extrn	Get_11_Characters:Near
	Extrn	ControlC_Handler:Near
	Extrn	SysDispMsg:Near
	Extrn	SysLoadMsg:Near
	Extrn	Yes?:Near

;No more SAFE module
;	Extrn	Hook_INT_24:Near		;*Set fatal error handler
;	Extrn	Int_24_Handler:Near		;*Fatal error int handler


IF FSExec					;/FS: conditional assembly

	Extrn	EXEC_FS_Format:Near

ENDIF						;/FS: conditional assembly end

	Extrn	GetDeviceParameters:Near
;
;*****************************************************************************
; External Data Declarations
;*****************************************************************************
;

	Extrn	SwitchMap:Word
	Extrn	ExitStatus:Byte
	Extrn	Drive:Byte
	Extrn	DriveLetter:Byte
	Extrn	TranSrc:Byte
	Extrn	TrackCnt:Word
	Extrn	NumSectors:Word
	Extrn	BIOSFile:Byte
	Extrn	DOSFile:Byte
	Extrn	CommandFile:Byte
	Extrn	MsgNeedDrive:Byte
	Extrn	MsgBadVolumeID:Byte
;	Extrn	MsgBadDrive:Byte		; M024
	Extrn	MsgAssignedDrive:Byte
	Extrn	MsgNetDrive:Byte
	Extrn	msgOptions:Byte			; formsg.inc
	Extrn	msgDblspaceDrv:Byte
	Extrn	msgDblspaceHost:Byte
	Extrn	msgCrLf:Byte
	Extrn	MSG_OPTIONS_LAST:Abs
	Extrn	Parse_Error_Msg:Byte
	Extrn	Extended_Error_Msg:Byte
	Extrn	SizeMap:Byte
	Extrn	MsgSameSwitch:Byte
	Extrn	Org_AX:word			;AX on prog. entry
	Extrn	DeviceParameters:Byte
	Extrn	FAT_Flag:Byte
	Extrn	Sublist_MsgParse_Error:Dword

	EXTRN	EXIT_FATAL		:ABS

code	segment public	para	'CODE'

;*****************************************************************************
;Routine name:	Main_Init
;*****************************************************************************
;
;Description: Main control routine for init section
;
;Called Procedures: Message (macro)
;		    Check_DOS_Version
;		    Init_Input_Output
;		    Validate_Target_Drive
;		    Hook_CNTRL_C
;
;Input: None
;
;Output: None
;
;Change History: Created	5/1/87	       MT
;
;Psuedocode
; ---------
;
;	Get PSP segment
;	Fatal_Error = NO
;	Setup I/O (CALL Init_Input_Output)
;	IF !Fatal_Error
;	   Check target drive letter (CALL Validate_Target_Drive)
;	   IF !Fatal_Error
;	      Set up Control Break (CALL Hook_CNTRL_C)
;	      IF !Fatal_Error
;		 CALL Main_Routine
;	      ENDIF
;	   ENDIF
;	ENDIF
;	Exit program
;*****************************************************************************

Procedure Main_Init


	Set_Data_Segment		;Set DS,ES to Data segment
	DOS_Call GetCurrentPSP		;Get PSP segment address
	mov	PSP_Segment,bx		;Save it for later
	mov	Fatal_Error,No		;Init the error flag
	call	Init_Input_Output	;Setup messages and parse
	cmp	Fatal_Error,Yes 	;Error occur?

	JE	$$IF1			;Nope, keep going
	call	Validate_Target_Drive	;Check drive letter
	cmp	Fatal_Error,Yes		;Error occur?

	JE	$$IF2				;Nope, keep going

	call	Check_For_Dblspace	; is it a DblSpace drive?
	cmp	Fatal_Error,Yes		; Error occur?
	JE	$$IF2			;  exit if so

	call	Hook_CNTRL_C		;Set CNTRL -Break hook

;No more SAFE module			;*RUP - 10/09/89
;	call	Hook_Int_24		;*Set fatal error handler
					;*jh

	cmp	Fatal_Error,Yes		;Error occur?

	JE	$$IF3			;Nope, keep going
	jmp	SHORT End_Main_Init	;Go do the real program
$$IF3:
$$IF2:
$$IF1:
	mov	al,ExitStatus		;Get Errorlevel
	DOS_Call Exit			;Exit program
	int	20h			;If other exit fails

End_Main_Init:
	ret
Main_Init endp

;***	Check_For_Dblspace -- see if target drive is compressed, error if so
;
;	entry:
;	   Drive == 0 based drive number
;
;	exit:
;	   Fatal_Error == YES if it is
;	   ExitStatus == EXIT_FATAL

Check_For_Dblspace	proc	near

	mov	ax,4a11h		; See if DblSpace installed
	xor	bx,bx
	int	2fh
	or	ax,ax			; error?
	jnz	Check_For_Dblspace_exit	;  Dblspace will set ax == 0 if in
	cmp	bx,'DM'			;  and will set bx == 'DM'
	jnz	Check_For_Dblspace_exit

	mov	ax,4a11h
	mov	bx,1			; check drive map
	mov	dl,Drive
	int	2fh
	test	bl,80h			; compressed volume?
	jnz	cfd_is_compressed	;  yes, tell user 2 use dblspace/format

	; The target is not a DblSpace compressed drive, but it might be
	; a host for 1 or more compressed drives.  Get DblSpace drive map
	; info for each possible drive letter and see if any compressed
	; drives are hosted on the format target drive.

	xor	dx, dx			; zero based drive numbers
cfd_chk_host:
	mov	ax, 4a11h		; DblSpace ID
	mov	bx, 1			; get drive map info call
	int	2fh
	test	bl, 80h 		; is this a compressed drive?
	jz	cfd_nxt_host

	and	bl, 7fh 		; Yes, is it hosted on format target?
	cmp	bl, Drive
	je	cfd_is_host		;   yup, go tell user

cfd_nxt_host:
	inc	dl			; next drive to check
	cmp	dl, 26
	jb	cfd_chk_host
	jmp	short Check_For_Dblspace_exit

cfd_is_host:
	mov	al, Drive		; tell user target is a host
	Message msgDblspaceHost
	call	Yes?			; proceed with format (Y/N)?  CY set
					;   unless user says Yes
	pushf
	Message	msgCrlf
	popf
	jc	cfd_error		; CY set if user didn't say Yes
	jmp	short Check_For_Dblspace_exit

cfd_is_compressed:
	mov	al,Drive		; tell user to use dblspace/format
	Message msgDblspaceDrv		;   for this drive

cfd_error:
	mov	Fatal_Error,YES 	; terminate format
	mov	ExitStatus,EXIT_FATAL

Check_For_Dblspace_exit:
	ret

Check_For_Dblspace	endp

;*****************************************************************************
;Routine name: Init_Input_Output
;*****************************************************************************
;
;Description: Initialize messages, Parse command line, allocate memory as
;	      needed. If there is a /FS switch, go handle it first as
;	      syntax of IFS format may be different from FAT format.
;
;Called Procedures: Preload_Messages
;		    Parse_For_FS_Switch
;		    Parse_Command_Line
;
;Change History: Created	4/1/87	       MT
;
;Input: PSP command line at 81h and length at 80h
;	Fatal_Error  = No
;
;Output: Fatal_Error = YES/NO
;
;Psuedocode
;----------
;
;	Load messages (CALL Preload_Messages)
;	IF !Fatal_Error
;	   See if EXEC another file system (CALL Parse_For_FS_Switch)
;	   IF !FATAL_Error (in this case means FS was found and exec'd)
;	      CALL Parse_Command_Line
;	      IF !Fatal_Error
;		 CALL Interpret_Parse
;	      ENDIF
;	   ENDIF
;	ENDIF
;	ret
;*****************************************************************************

Procedure Init_Input_Output

	Set_Data_Segment			;Set DS,ES to Data segment
	call	Preload_Messages		;Load up message retriever

IF FSExec					;/FS: conditional assembly

	cmp	Fatal_Error,YES 		;Quit?
	JE	$$IF7
	call	Check_For_FS_Switch		;Specify FS other than FAT?

ENDIF						;/FS: conditional assembly end

	cmp	Fatal_Error,YES			;drive is invalid for format?
	JE	$$IF8
	call	Parse_Command_Line		;Parse in command line input
	cmp	Fatal_Error,YES			; Quit?
	JE	$$IF9

	call	Determine_FAT_Non_FAT		;see if drive was non_FAT
	call	Check_For_Invalid_Drive		;Drive joined?
$$IF9:
$$IF8:

IF FSExec					;/FS: conditional assembly

$$IF7:

ENDIF						;/FS: conditional assembly end
	ret

Init_Input_Output endp

;*****************************************************************************
;Routine name: Preload_Messages
;*****************************************************************************
;
;Description: Preload messages using common message retriever routines.
;
;Called Procedures: SysLoadMsg
;
;
;Change History: Created	5/1/87	       MT
;
;Input: Fatal_Error = NO
;
;Output: Fatal_Error = YES/NO
;
;Psuedocode
;----------
;
;	Preload All messages (Call SysLoadMsg)
;	IF error
;	   Display SysLoadMsg error message
;	   Fatal_Error = YES
;	ENDIF
;	ret
;*****************************************************************************

Procedure Preload_Messages

	Set_Data_Segment			;Set DS,ES to Data segment
	call	SysLoadMsg			;Preload the messages

	JNC	$$IF13				;Error?
	call	SysDispMsg			;Display preload msg
	mov	Fatal_Error, YES		;Indicate error exit

$$IF13:
	ret

Preload_Messages endp

;*****************************************************************************

IF FSExec					;/FS: conditional assembly

;*****************************************************************************
;Routine name: Check_For_FS_Switch
;*****************************************************************************
;
;Description: Parse to see if /FS switch entered, and if so, go EXEC the
;	      asked for file system. Set Fatal_Error = YES if FS found
;	      If we do find /FS, we need to build a string of xxxxxfmt.exe,0
;	      where xxxxx is the first 5 characters or less of /FS:xxxxx
;
;Called Procedures: Parse_For_FS_Switch
;		    EXEC_FS_Format
;
;Change History: Created	6/21/87 	MT
;
;Input: Fatal_Error = NO
;
;Output: Fatal_Error = YES/NO
;	 Exit_Status set
;
;Psuedocode
;----------
;
;	Parse for /FS switch (CALL Parse_For_FS_Switch)
;	IF !FATAL_ERROR
;	   IF /FS found
;	      Point at what was entered on /FS:xxxxx
;	      DO
;	      LEAVE end of entered string
;		Got good char, move into path
;	      ENDDO already got 5 chars (max in xxxxxfmt.exe)
;	      Tack on the rest of the string  (fmt.exe,0)
;	      Go exec the needed format (CALL EXEC_FS_Format)
;	   ENDIF
;	ENDIF
;	ret
;*****************************************************************************

Procedure Check_For_FS_Switch

	Set_Data_Segment			;Set DS,ES to Data segment
	call	Parse_For_FS_Switch		;See if /FS entered
	cmp	Fatal_Error,YES 		;Bad stuff entered??

	JE	$$IF15				;Nope, cruise onward
	cmp	Switch_String_Buffer.Switch_Pointer,offset Switch_FS_Control.Keyword

	JNE	$$IF16				;We got the switch
	mov	Switch_FS_Control.Keyword,20h	;remove switch from table
	test	SwitchMap,Switch_FS		;Have this already?

	JNZ	$$IF17				;Nope
	push	ds				;Get addressibility
	pop	es

	assume	ds:nothing,es:data

						;Get the entered FS
	mov	ax,Switch_String_Buffer.Switch_String_Seg
	mov	ds,ax
	mov	si,es:Switch_String_Buffer.Switch_String_Off
	mov	cx,FS_String_Max_Length
	mov	di,offset es:FS_String_Buffer

$$DO18: 					;Move whatever user entered
	cmp	byte ptr [si],ASCIIZ_End	;End of the string?

	JE	$$EN18				;Yep
	movsb					;Put character in buffer
	dec	cx				;Dec character counter
	cmp	cx,0				;Nope, reached max # chars?

	JNE	$$DO18				;Yes
$$EN18:
	Set_Data_Segment			;Set DS,ES to Data segment
	mov	cx,Len_FS_String_End		;Tack the FMT.EXE onto it
	mov	si,offset es:FS_String_End	;DI still points at string
	rep	movsb				;We now have Asciiz path!
	call	EXEC_FS_Format			;Go try to EXEC it.....

	JMP	SHORT $$EN17
$$IF17:
	Message msgSameSwitch
	mov	Fatal_Error,Yes

$$EN17:
$$IF16:
$$IF15:
	ret

Check_For_FS_Switch endp

;*****************************************************************************
;Routine name: Parse_For_FS_Switch
;*****************************************************************************
;
;Description: Copy the command line. Parse the new command line (Parse routines
;	      destroy the data being parsed, so need to work on copy so that
;	      complete command line can be passed to child format).
;	      The only thing we care about is if the /FS: switch exists, so
;	      parse until  end of command line found. If there was an error,
;	      and it occurred on the /FS switch, then give parse error,
;	      otherwise ignore the parse error, because it might be something
;	      file system specific that doesn't meet DOS syntax rules. Also
;	      check for drive letter, as it is alway required.
;
;Called Procedures: Message (macro)
;		    SysLoadMsg
;		    Preload_Error
;		    SysParse
;
;Change History: Created	5/1/87	       MT
;
;Input: Command line at 80h in PSP
;	   Fatal_Error = NO
;	   PSP_Segment
;
;Output: Fatal_Error = YES/NO
;
;Psuedocode
;----------
;	Copy command line to buffer
;	DO
;	   Parse command line (Call SysParse)
;	LEAVE end of parse
;	ENDDO found /FS
;	IF drive letter not found (This assumes drive letter before switches)
;	   Tell user
;	   Fatal_Error = YES
;	ENDIF
;	ret
;*****************************************************************************

Procedure Parse_For_FS_Switch

	Set_Data_Segment			;Set DS,ES to Data segment
	mov	Drive_Letter_Buffer.Drive_Number,Init
	mov	ds,PSP_Segment			;Get segment of PSP
	assume	ds:nothing

	mov	si,Command_Line_Parms		;Point at command line
	mov	di,offset data:Command_Line_Buffer ;Where to put a copy of it
	mov	cx,Command_Line_Length		;How long was input?
	repnz	movsb				;Copy it

	Set_Data_Segment			;Set DS,ES to Data segment
	xor	cx,cx
	xor	dx,dx				;Required for SysParse call
	mov	si,offset Command_Line_Buffer	;Pointer to parse line
	mov	di,offset Switch_FS_Table	;Pointer to control table

$$DO25: 					;Setup parse call
	call	SysParse			;Go parse
	cmp	ax,End_Of_Parse			;Check for end of parse

	JE	$$EN25				;Exit if it is end, or
	cmp	ax,Operand_Missing		; exit if positional missing

	JE	$$EN25				;In other words, no drive letter
	cmp	Switch_String_Buffer.Switch_Pointer, \
		offset Switch_FS_Control.Keyword

	JNE	$$DO25				;Exit if we find /FS

$$EN25: 					;Check for drive letter found
	cmp	Drive_Letter_Buffer.Drive_Type,Type_Drive

	JE	$$IF28				;Did we not find one?
	MESSAGE msgNeedDrive			;Must enter drive letter
	mov	Fatal_Error,Yes			;Indicate error on exit

$$IF28:
	ret

Parse_For_FS_Switch endp

ENDIF						;/FS: conditional assembly end


;*****************************************************************************
;Routine name: Parse_Command_Line
;*****************************************************************************
;
;Description: Parse the command line. Check for errors, and display error and
;		 exit program if found. Use parse error messages except in case
;		 of no parameters, which has its own message
;
;Called Procedures: Message (macro)
;		    SysParse
;		    Interpret_Parse
;
;Change History: Created	5/1/87	       MT
;
;Input: Fatal_Error = NO
;	PSP_Segment
;
;Output: Fatal_Error = YES/NO
;
;
;Psuedocode
;----------
;
;	Assume Fatal_Error = NO on entry
;	SEARCH
;	EXITIF Fatal_Error = YES,OR  (This can be set by Interpret_Parse)
;	   Parse command line (CALL SysParse)
;	EXITIF end of parsing command line
;	   Figure out last thing parsed (Call Interpret_Parse)
;	ORELSE
;	   See if parse error
;	LEAVE parse error,OR
;	   See what was parsed (Call Interpret_Parse)
;	LEAVE if interpret error such as bad volume label
;	ENDLOOP
;	   Display parse error message and print error operand
;	   Fatal_Error = YES
;	ENDSRCH
;	ret
;*****************************************************************************

Procedure Parse_Command_Line

	Set_Data_Segment			;Set DS,ES to Data segment
	push	ds
	mov	ds,PSP_Segment

	assume	ds:nothing,es:data

	xor	cx,cx				;Parse table @DI
	xor	dx,dx				;Parse line @SI
	mov	si,Command_Line_Parms		;Pointer to parse line
	mov	word ptr es:Command_Old_Ptr,si
	mov	di,offset es:Command_Line_Table ;Pointer to control table

$$DO30: 					;Loop until all parsed
	cmp	es:Fatal_Error,Yes		;Interpret something bad?

	JE	$$LL31				;If so, don't parse any more
	call	SysParse			;Go parse
	cmp	ax,End_Of_Parse			;Check for end of parse

	JNE $$IF30				;Is it?

$$LL31: 					;All done
	JMP	SHORT $$SR30			;Not end

$$IF30:
	cmp	ax,0				;Check for parse error

	JNE	$$EN30				;Stop if there was one
	call	Interpret_Parse 		;Go find what we parsed
	mov	word ptr es:Command_Old_Ptr,si

	JMP	SHORT $$DO30			;Parse error, see what it was

$$EN30:
	mov	byte ptr ds:[si],0
	push	di
	push	ax
	mov	di,offset es:Sublist_MsgParse_Error
	mov	ax,word ptr es:Command_Old_Ptr
	mov	word ptr es:[di+2],ax
	mov	word ptr es:[di+4],ds
	pop	ax
	pop	di
	PARSE_MESSAGE				;Display parse error
	mov	es:Fatal_Error,YES		;Indicate death!

$$SR30:
	pop	ds
	ret

Parse_Command_Line endp

;*****************************************************************************
;Routine name: Interpret_Parse
;*****************************************************************************
;
;Description: Set the SwitchMap  field with the switches found on the
;	      command line. Get the drive letter. /FS will be handled before
;	      here, will not be seen in this parse or accepted. Also, if /V
;	      see if volume label entered and verify it is good, setting up
;	      FCB for later create
;
;Called Procedures: Get_11_Characters
;
;Change History: Created	5/1/87	       MT
;
;Input: Fatal_Error = NO
;
;Output: SwitchMap set
;	 DriveLetter set
;	 DriveNum set A=0,B=1 etc...
;	 Command_Line = YES/NO
;	 Fatal_Error = YES/NO
;
;Psuedocode
;----------
;
;	IF Drive letter parsed
;	Drive = Parsed drive number -1
;	DriveLetter = (Parsed drive number - 1) +'A'
;	ENDIF
;	IF /1
;	  or	SwitchMap,Switch_1
;	ENDIF
;	IF /4
;	  or	SwitchMap,Switch_4
;	ENDIF
;	IF /8
;	  or	SwitchMap,Switch_8
;	ENDIF
;	IF /S
;	  or	SwitchMap,Switch_S
;	ENDIF
;	IF /BACKUP
;	  or	SwitchMap,Switch_BACKUP
;	ENDIF
;	IF /B
;	  or	SwitchMap,Switch_B
;	ENDIF
;	IF /T
;	  or	SwitchMap,Switch_T
;	  TrackCnt = entered value
;	ENDIF
;	IF /N
;	  or	SwitchMap,Switch_N
;	  NumSectors = entered value
;	ENDIF
;	IF /SELECT
;	  or	SwitchMap,Switch_SELECT
;	ENDIF
;	IF /V
;	  or	SwitchMap,Switch_V
;	  IF string entered
;	     Build ASCIIZ string for next call (CALL Build_String)
;	     Verify DBCS and setup FCB (CALL Get_11_Characters)
;	     Command_Line = YES
;		IF error
;		  Invalid label message
;		  Fatal_Error = YES
;		ENDIF
;	  ENDIF
;	ENDIF
;	IF /AUTOTEST
;	  or	SwitchMap,Switch_AUTOTEST
;	ENDIF
;
;	IF /F
;	  or	SwitchMap,Switch_F
;	  or	Size_Map,Item_Tag
;	ENDIF
;	IF /Z	(only if assembled)
;	  or	SwitchMap,Switch_Z
;	ENDIF
;	ret
;*****************************************************************************

Procedure Interpret_Parse

	push	ds				;Save segment
	push	si				;Restore SI for parser
	push	cx
	push	di

	Set_Data_Segment			;Set DS,ES to Data segment

; See if user put /? on command line.

	cmp	Switch_Buffer.Switch_Pointer,offset Switch_?_Control.Keyword
	jne	@F				; skip if not on
	call	Display_Options			; give the message
	mov     Fatal_Error, Yes		; flag get out now
	jmp	Interpret_Parse_Exit		;  and leave this routine
@@:
	;Have drive letter?

	cmp	byte ptr Drive_Letter_Buffer.Drive_Type,Type_Drive

	JNE	$$IF36				;Yes, save info
						;Get drive entered
	mov	al,Drive_Letter_Buffer.Drive_Number
	dec	al				;Make it 0 based
	mov	Drive,al			; "  "	  "  "
	add	al,'A'				;Make it a drive letter
	mov	DriveLetter,al			;Save it

$$IF36:
	cmp	Switch_Buffer.Switch_Pointer,OFFSET Switch_U_Control.Keyword
	jne	@F
	mov	Switch_U_Control.Keyword,20h
	or	SwitchMap,Switch_U

@@:
	cmp	Switch_Buffer.Switch_Pointer,OFFSET Switch_Q_Control.Keyword
	jne	@F
	mov	Switch_Q_Control.Keyword,20h
	or	SwitchMap,Switch_Q

@@:
	cmp	Switch_Buffer.Switch_Pointer,OFFSET Switch_1_Control.Keyword
	JNE	$$IF38
	mov	Switch_1_Control.Keyword,20h	;remove switch from table
	or	SwitchMap,Switch_1

$$IF38:
	cmp	Switch_Buffer.Switch_Pointer,OFFSET Switch_4_Control.Keyword
	JNE	$$IF40
	mov	Switch_4_Control.Keyword,20h	;remove switch from table
	or	SwitchMap,Switch_4

$$IF40:
	cmp	Switch_Buffer.Switch_Pointer,offset Switch_8_Control.Keyword
	JNE	$$IF42
	mov	Switch_8_Control.Keyword,20h	;remove switch from table
	or	SwitchMap,Switch_8

$$IF42:
	cmp	Switch_Buffer.Switch_Pointer,offset Switch_S_Control.Keyword
	JNE	$$IF44
	mov	Switch_S_Control.Keyword,20h	;remove switch from table
	or	SwitchMap,Switch_S

$$IF44:
	cmp	Switch_Buffer.Switch_Pointer,offset Switch_Backup_Control.Keyword
	JNE	$$IF46
	mov	   Switch_Backup_Control.Keyword,20h ;remove switch from table
	or	   SwitchMap,Switch_Backup

$$IF46:
	cmp	Switch_Buffer.Switch_Pointer,offset Switch_Select_Control.Keyword
	JNE	$$IF48
	mov	Switch_Select_Control.Keyword,20h ;remove switch from table
	or	SwitchMap,Switch_Select

$$IF48:
	cmp	Switch_Buffer.Switch_Pointer,offset Switch_B_Control.Keyword
	JNE	$$IF50
	mov	Switch_B_Control.Keyword,20H
	or	SwitchMap,Switch_B

$$IF50:
	cmp	Switch_Num_Buffer.Switch_Num_Pointer, \
		offset es:Switch_T_Control.Keyword

	JNE	$$IF52
	mov	Switch_T_Control.Keyword,20h ;remove switch from table
	mov	Switch_Num_Buffer.Switch_Num_Pointer,0 ;Init for next switch
	test	SwitchMap,Switch_T		;Don't allow if switch already
	JNZ	$$IF53				; entered

	or	SwitchMap,Switch_T
						;Get entered tracks
	mov	ax,Switch_Num_Buffer.Switch_Number_Low
	mov	TrackCnt,ax			;1024 or less, so always dw
	JMP SHORT $$EN53

$$IF53:
	Message msgSameSwitch
	mov	Fatal_Error,Yes

$$EN53:
$$IF52:
	cmp	Switch_Num_Buffer.Switch_Num_Pointer, \
		offset Switch_N_Control.Keyword
	JNE	$$IF57
	mov	Switch_N_Control.Keyword,20h ;remove switch from table
	mov	Switch_Num_Buffer.Switch_Num_Pointer,0 ;Init for next switch
	test	SwitchMap,Switch_N		;Make sure switch not already

	JNZ	$$IF58				; entered
	or	SwitchMap,Switch_N
	mov	ax,Switch_Num_Buffer.Switch_Number_Low ;Get entered tracks
	xor	ah,ah				;clear high byte
	mov	NumSectors,ax			;Save tracks per sector
	JMP SHORT $$EN58

$$IF58:
	Message msgSameSwitch
	mov	Fatal_Error,Yes

$$EN58:
$$IF57:
	cmp	Switch_String_Buffer.Switch_String_Pointer, \
		offset Switch_V_Control.Keyword
	JNE	$$IF62				;If /v and haven't already done

	mov	   Switch_String_Buffer.Switch_String_Pointer,0 ;Init for next switch
	mov	   Switch_V_Control.Keyword,20h ;remove switch from table
	test	SwitchMap,Switch_V		; it - Only allow one /V entry

	JNZ	$$IF63
	or	SwitchMap,Switch_V	;Set /v indicator
	mov	ds,Switch_String_Buffer.Switch_String_Seg ;Get string address

	assume	ds:nothing

; M009 - Begin
;

	cld
	mov	cx,si				; Save end of /V parameter
	mov	si,es:Switch_String_Buffer.Switch_String_Off
	cmp	byte ptr ds:[si],None	;Is there a string there?

	jne	GotAString

	push	es
	
	mov	di,Command_Old_ptr
	sub	cx,di				; cx = length of whole /v

	mov	es,es:PSP_Segment
	
	mov	al,':'
	repnz	scasb
	pop	es

	push	es
	pop	ds
	assume	DS:DATA,ES:DATA
	jcxz	$$IF64

	mov	Vol_Label_Buffer,0	
	mov	Command_Line,YES
	jmp	short $$IF64
;	JE	$$IF64				;Yep
;M009 - End

GotAString:
	assume	DS:nothing
	mov	di,offset es:Vol_Label_Buffer ;Point at buffer to move string
	mov	cx,Label_Length+1		;Max length of string
	rep	movsb				;This will copy string & always
						; leave ASCIIZ end in buffer,
						; which is init'd to 13 dup(0)
	push	es
	pop	ds				;Set DS,ES to Data segment
	assume	DS:DATA,ES:DATA

	mov	si,offset Vol_Label_Buffer	;Point at string
	mov	Command_Line,YES		;Set flag indicating vol label
	call	Get_11_Characters		;Check DBCS and build FCB

	JNC	$$IF65				;Bad DBCS setup
	Message msgBadVolumeID		;Tell user
	mov	Fatal_Error,YES			;Indicate time to quit

$$IF65:
$$IF64:
	JMP	SHORT $$EN63
$$IF63:
	Message msgSameSwitch
	mov	Fatal_Error,Yes

$$EN63:
$$IF62:
	cmp	Switch_Buffer.Switch_Pointer, \
		offset Switch_Autotest_Control.Keyword
	JNE	$$IF71
						;remove switch from table
	mov	Switch_Autotest_Control.Keyword,20h
	or	SwitchMap,Switch_Autotest

$$IF71:

IF ShipDisk
						;/Z switch?
	cmp	Switch_Buffer.Switch_Pointer, \
		offset Switch_Z_Control.Keyword
	JNE	$$IF73				;yes

	mov	Switch_Z_Control.Keyword,20h	;remove switch from table
	or	SwitchMap,Switch_Z		;signal switch found

$$IF73:
ENDIF

	cmp	Switch_String_Buffer.Switch_Pointer, \
		offset Switch_F_Control.Keyword
	JNE	$$IF75
	mov	Switch_F_Control.Keyword,20h	;remove switch from table

						; clear ptr for next iteration
	mov	Switch_String_Buffer.Switch_Pointer,0
						;Init for next switch
	mov	Switch_Num_Buffer.Switch_Num_Pointer,0
	test	SwitchMap,Switch_F		; it - do this because SysParse

	JNZ	$$IF76				; reuses string buff each time
	or	SwitchMap,Switch_F
						; Indicate what size
	mov	al,Switch_String_Buffer.Switch_String_Item_Tag
	or	SizeMap,al
	JMP	SHORT $$EN76

$$IF76:
	Message msgSameSwitch
	mov	Fatal_Error,Yes
$$EN76:
$$IF75:
Interpret_Parse_Exit:

	pop	di				;Restore parse regs
	pop	cx
	pop	si
	pop	ds
	ret

Interpret_Parse endp

;*****************************************************************************
;Routine name: Display_Options
;*****************************************************************************
;
;Description: Display the options help message on standard output.
;
;Called Procedures: Message (macro)
;
;Change History: Created	5/2/90	       c-PaulB
;
;Input:  No value passed
;
;Output: No value returned
;
;*****************************************************************************

Procedure Display_Options

DO_Loop:
	Message	msgOptions			; display the options
	cmp	word ptr [msgOptions], MSG_OPTIONS_LAST	; last msg?
	je	DO_Done				; done if so
	inc	word ptr [msgOptions]		; else get next msg
	jmp	short DO_Loop			;  and go do it
DO_Done:
	ret

Display_Options endp

;*****************************************************************************
;Routine name: Validate_Target_Drive
;*****************************************************************************
;
;Description: Control routine for validating the specified format target drive.
;	      If any of the called routines find an error, they will print
;	      message and terminate program, without returning to this routine
;
;Called Procedures: Check_Target_Drive
;		    Check_For_Network
;		    Check_Translate_Drive
;
;Change History: Created	5/1/87	       MT
;
;Input: Fatal_Error = NO
;
;Output: Fatal_Error = YES/NO
;
;Psuedocode
;----------
;
;	CALL Check_Target_Drive
;	IF !Fatal_Error
;	   CALL Check_For_Network
;	   IF !Fatal_Error
;	      CALL Check_Translate_Drive
;	   ENDIF
;	ENDIF
;	ret
;*****************************************************************************

Procedure Validate_Target_Drive

	call	Check_Target_Drive		;See if valid drive letter
	cmp	Fatal_Error,YES 		;Can we continue?
	JE	$$IF80				;Yep
	call	Check_For_Network		;See if Network drive letter
	cmp	Fatal_Error,YES			;Can we continue?

	JE	$$IF80				;Yep
	call	Check_Translate_Drive		;See if Subst, Assigned
$$IF80: 					;- Fatal_Error passed back
	ret

Validate_Target_Drive endp

;*****************************************************************************
;Routine name: Check_Target_Drive
;*****************************************************************************
;
;Description: Check to see if valid DOS drive by checking if drive is
;	      removable. If error, the drive is invalid. Save default
;	      drive info.
;
;Called Procedures: Message (macro)
;
;Change History: Created	5/1/87	       MT
;
;Input: Fatal_Error = NO
;
;Output: BIOSFile = default drive letter
;	 DOSFile = default drive letter
;	 CommandFile = default drive letter
;	 Fatal_Error = YES/NO
;
;Psuedocode
;----------
;
;	Get default drive (INT 21h, AH = 19h)
;	Convert it to drive letter
;	Save into BIOSFile,DOSFile,CommandFile
;	See if drive removable (INT 21h, AX=4409h IOCtl)
;	IF error - drive invalid
;	   Display Invalid drive message
;	   Fatal_Error= YES
;	ENDIF
;	ret
;*****************************************************************************

Procedure Check_Target_Drive

	DOS_Call Get_Default_Drive		;Find the current drive
	add	al,'A'				;Convert to drive letter
	mov	BIOSFile,al			;Put it into path strings
	mov	DOSFile,al
	mov	CommandFile,al
	mov	bl,Drive			;Set up for next call
	inc	bl				;A=1,B=2 for IOCtl call
	mov	al,09h				;See if drive is local
	DOS_Call IOCtl				;-this will fail if bad drive

	JNC	$$IF84				;CY means invalid drive
	Extended_Message			;Print message
	mov	Fatal_Error,Yes			;Indicate error
$$IF84:
	ret					;And we're outa here

Check_Target_Drive endp

;*****************************************************************************
;Routine name: Check_For_Network
;*****************************************************************************
;
;Description: See if target drive isn't local, or if it is a shared drive. If
;	      so, exit with error message. The IOCtl call is not checked for
;	      an error because it is called previously in another routine, and
;	      invalid drive is the only error it can generate. That condition
;	      would not get this far
;
;Called Procedures: Message (macro)
;
;Change History: Created	5/1/87	       MT
;
;Input: Drive
;	   Fatal_Error = NO
;
;Output: Fatal_Error = YES/NO
;
;Psuedocode
;----------
;	See if drive is local (INT 21h, AX=4409 IOCtl)
;	IF not local
;	   Display network message
;	   Fatal_ERROR = YES
;	ELSE
;	   IF  8000h bit set on return
;	      Display assign message
;	      Fatal_Error = YES
;	   ENDIF
;	ENDIF
;	ret
;*****************************************************************************

Procedure Check_For_Network

	mov	bl,Drive			;Drive is 0=A, 1=B
	inc	bl				;Get 1=A, 2=B for IOCtl call
	mov	al,09h				;See if drive is local or remote
	DOS_CALL IOCtl				;We will not check for error
	test	dx,Net_Check			;if (x & 1200H)(redir or shared);

	JZ	$$IF86				;Found a net drive
	Message MsgNetDrive			;Tell 'em
	mov	Fatal_Error,Yes			;Indicate bad stuff
	JMP	SHORT $$EN86			;Local drive, now check assign

$$IF86:
	test	dx,Assign_Check			;8000h bit is bad news
	JZ	$$IF88				;Found it

	Message MsgAssignedDrive		;Tell error
	mov	Fatal_Error,Yes			;Indicate bad stuff

$$IF88:
$$EN86:
	ret

Check_For_Network endp

;*****************************************************************************
;Routine name: Check_Translate_Drive
;*****************************************************************************
;
;Description: Do a name translate call on the drive letter to see if it is
;	      assigned by SUBST or ASSIGN
;
;Called Procedures: Message (macro)
;
;Change History: Created	5/1/87	       MT
;
;Input: Drive
;	   Fatal_Error = NO
;
;Output: Fatal_Error = YES/NO
;
;Psuedocode
;----------
;	Put drive letter in ASCIIZ string "d:\",0
;	Do name translate call (INT 21)
;	IF drive not same
;	   Display assigned message
;	   Fatal_Error = YES
;	ENDIF
;	ret
;*****************************************************************************

Procedure Check_Translate_Drive

	mov	bl,Drive			;Get drive
	add	byte ptr [TranSrc],bl		;Make string "d:\"
	mov	si,offset TranSrc		;Point to translate string
	push	ds				;Set ES=DS (Data segment)
	pop	es				;     "  "	"  "
	mov	di,offset Command_Line_Buffer	;Point at output buffer
	DOS_Call xNameTrans			;Get real path
	mov	bl,byte ptr [TranSrc]		;Get drive letter from path
	cmp	bl,byte ptr Command_Line_Buffer ;Did drive letter change?

	JE	$$IF91				;If not the same, it be bad
	Message MsgAssignedDrive		;Tell user
	mov	Fatal_Error,Yes			;Setup error flag

$$IF91:
	ret

Check_Translate_Drive endp

;*****************************************************************************
;Routine name: Hook_CNTRL_C
;*****************************************************************************
;
;Description: Change the interrupt handler for INT 13h to point to the
;	      ControlC_Handler routine
;
;Called Procedures: None
;
;Change History: Created	4/21/87 	MT
;
;Input: None
;
;Output: None
;
;Psuedocode
;----------
;
;	Point at ControlC_Handler routine
;	Set interrupt handler (INT 21h, AX=2523h)
;	ret
;*****************************************************************************

Procedure Hook_CNTRL_C

	mov	al,23H				;Specify CNTRL handler
	mov	dx, offset ControlC_Handler	;Point at it
	push	ds				;Save data seg
	push	cs				;Point to code segment
	pop	ds				;
	DOS_Call Set_Interrupt_Vector		;Set the INT 23h handler
	pop	ds				;Get Data degment back
	ret

Hook_CNTRL_C endp

;=========================================================================
; Check_For_Invalid_Drive	: This routine checks the AX received by
;				  FORMAT on its entry.	This value will
;				  tell us if we are attempting to format
;				  a JOINED drive.
;
;	Inputs	: Org_AX	- AX on entry to FORMAT
;
;	Outputs : Fatal_Error	- Yes if AL contained FFh
;=========================================================================

Procedure Check_For_Invalid_Drive

	push	ax				;save ax
	cmp	FAT_Flag,Yes			;FAT system?
	JNE	$$IF93				;yes
	mov	ax,Org_AX			;get its org. value
	cmp	al,0ffh 			;Invalid drive?
	JNE	$$IF94				;yes
	mov	Fatal_Error,YES 		;flag an error
	mov	ax,Invalid_Drive		;error message
	Extended_Message			;tell error

$$IF94:
$$IF93:
	pop	ax
	ret

Check_For_Invalid_Drive endp


;=========================================================================
; Determine_FAT_Non_FAT 	- This routine determines whether or
;				  not a device is formatted to a FAT
;				  specification versus a Non-FAT
;				  specification.
;
;	Inputs	: DX - Pointer to device parameters buffer
;
;	Outputs : DeviceParameters - buffer containing BPB.
;
;	Date	: 11/6/87
;=========================================================================

Procedure Determine_FAT_Non_FAT
;
; M031: With memory card media, a get default BPB requires a media
; in the drive. At this point, we haven't outputted the "Insert disk ..."
; message. This code was also buggy because if the GetDeviceParameters
; failed, FAT_Flag was not properly initialized (it is a DB ?, so it would
; have been equivalent to No). I patched the code to first check if there is
; a CMCDD disk. If so, we will set FAT_flag to Yes.

	push	ax				;save regs
	push	dx

	call	CheckCMCDD			; M033
	cmp	CMCDDFlag, Yes
	je	$$IF98				; M031

	lea	dx, deviceParameters		;point to buffer
	mov	deviceParameters.DP_SpecialFunctions, 0	 ;get default BPB
	call	GetDeviceParameters		;make the call
	JC	$$IF97				;no error occurred

						;non-FAT system?
	cmp	byte ptr DeviceParameters.DP_BPB.BPB_NumberOfFATS,00h
	JNE	$$IF98				;yes
			; Can't create recovery file or do quick
			; format for non FAT disks

	or	SwitchMap,Switch_U
	and	Switchmap, NOT Switch_Q

	mov	FAT_Flag,No			;signal system non-FAT
	mov	ax,5f07h			;allow access to disk
	mov	dl,Drive			;get 0 based driver number
	int	21h				;allow access to the drive

	JMP	SHORT $$EN98			;FAT system
$$IF98:
	mov	FAT_Flag,Yes			;flag FAT system

$$EN98:
$$IF97:

	pop	dx				;restore regs
	pop	ax
	ret

Determine_FAT_Non_FAT	endp

;M033 - Begin
;=========================================================================
; CheckCMCDD	 		- This routine determines whether or
;				  not the drive is a CMCDD drive.
;
;	Inputs	: None
;
;	Outputs : CMCDDFlag is Yes iff we are on a CMCDD drive
;
;	Trashes : AX, BX, CX, DX
;
;	Date	: 6/5/91
;=========================================================================
procedure CheckCMCDD

	mov	ax,(IOCTL shl 8) or IOCTL_QUERY_BLOCK ; Check if function supported
	mov	bl, Drive
	inc	bl				; 1 based
	mov	cx,(RAWIO shl 8) or 73h		; Determine if get_system_info
						; exist (only CMCDD has it).
	int	21h				; see if CMCDD
	jc	notCMCDD

	mov	CMCDDFlag, Yes
	ret

notCMCDD:
	mov	CMCDDFlag, No
	ret

CheckCMCDD	endp
;M033 - end

; =========================================================================

code	ends
	end

