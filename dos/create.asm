TITLE	DOS_CREATE/DOS_CREATE_NEW - Internal CREATE calls for MS-DOS
NAME	DOS_CREATE

;
;	Microsoft Confidential
;	Copyright (C) Microsoft Corporation 1991
;	All Rights Reserved.
;

;**	Internal Create and Create new to create a local or NET file and SFT.
;
;	DOS_CREATE
;	DOS_CREATE_NEW
;	SET_MKND_ERR
;	SET_Media_ID
;	SET_EXT_Mode
;
;	Revision history:
;
;	    A000 version 4.00	  Jan. 1988
;	    A001  D490 -- Change IOCTL subfunctios from 63h,43h to 66h, 46h

	.xlist
	.xcref
	include version.inc
	include dosseg.inc
	INCLUDE DOSSYM.INC
	INCLUDE DEVSYM.INC
	include sf.inc
	include mult.inc
	include filemode.inc
	include curdir.inc
	.cref
	.list

Installed = TRUE

	i_need	THISSFT,DWORD
	i_need	THISCDS,DWORD
	I_need	EXTERR,WORD
	I_Need	ExtErr_locus,BYTE
	I_need	JShare,DWORD
	I_need	VOLCHNG_FLAG,BYTE
	I_need	SATTRIB,BYTE
	I_need	CALLVIDM,DWORD
	I_need	EXTOPEN_ON,BYTE 		  ;AN000; extended open
	I_need	NAME1,BYTE			  ;AN000;
	I_need	NO_NAME_ID,BYTE 		  ;AN000;
	I_need	Packet_Temp,WORD		  ;AN000;
	I_need	DOS34_FLAG,WORD 		  ;AN000;
	I_need	SAVE_BX,WORD			  ;AN000;


DOSCODE SEGMENT
	ASSUME	CS:DOSCODE


;***	DOS_CREATE - Create a File
;
;	DOS_Create is called to create the specified file, truncating
;	the old one if it exists.
;
;
;	ENTRY	AX is Attribute to create
;		(ds) = DOSDATA
;		[WFP_START] Points to WFP string ("d:/" must be first 3 chars, NUL
;			terminated)
;		[CURR_DIR_END] Points to end of Current dir part of string
;			( = -1 if current dir not involved, else
;			 Points to first char after last "/" of current dir part)
;		[THISCDS] Points to CDS being used
;			(Low word = -1 if NUL CDS (Net direct request))
;		[THISSFT] Points to SFT to fill in if file created
;			(sf_mode field set so that FCB may be detected)
;		[SATTRIB] Is attribute of search, determines what files can be found
;
;	EXIT	sf_ref_count is NOT altered
;		CARRY CLEAR
;		    THISSFT filled in.
;			sf_mode = unchanged for FCB, sharing_compat + open_for_both
;		CARRY SET
;		    AX is error code
;			error_path_not_found
;				Bad path (not in curr dir part if present)
;			error_bad_curr_dir
;				Bad path in current directory part of path
;			error_access_denied
;				Attempt to re-create read only file , or
;				create a second volume id or create a dir
;			error_sharing_violation
;				The sharing mode was correct but not allowed
;				generates an INT 24
;	USES	all but DS

procedure   DOS_Create,NEAR

;hkn; dispatched to from file.asm and fcbio.asm. DS set up to DOSDATA at 
;hkn; this point.

	DOSAssume   <DS,SS>,"DOS_Create"

	XOR	AH,AH		; Truncate is OK

;	Enter here from Dos_Create_New
;
;	(ah) = 0 iff truncate OK

Create_inter:
	TEST	AL,NOT (attr_all + attr_ignore + attr_volume_id)
				; Mask out any meaningless bits
	JNZ	AttErr
	TEST	AL,attr_volume_id
	JZ	NoReset
	OR	[DOS34_FLAG],DBCS_VOLID      ;AN000;FOR dbcs volid
	MOV	AL,attr_volume_id
NoReset:
	OR	AL,attr_archive ; File changed
	TEST	AL,attr_directory + attr_device
	JZ	ATT_OK
AttErr:
	MOV	AX,5		; Attribute problem
	MOV	Exterr_Locus,errLOC_Unk
	JMP	SHORT SET_MKND_ERR ; Gotta use MKDIR to make dirs, NEVER allow
				   ;	attr_device to be set.
ATT_OK:
	LES	DI,[THISSFT]
	PUSH	ES
	LES	SI,[THISCDS]
	CMP	SI,-1
	JNE	TEST_RE_NET

;	No CDS, it must be redirected.

	POP	ES

;Extended open hooks
	TEST	[EXTOPEN_ON],ext_open_on    ;AN000;EO. from extnded open
	JZ	NOEXTOP 		    ;AN000;EO. no, do normal
IFS_extopen:				    ;AN000;EO.
	PUSH	AX			    ;AN000;EO. pass create attr
	MOV	AX,(multNET SHL 8) OR 46    ;AN000;EO. issue extended open verb
	INT	2FH			    ;AN000;EO.
	POP	BX			    ;AN000;EO. trash bx
	MOV	[EXTOPEN_ON],0		    ;AN000;EO.
	return				    ;AN000;EO.
NOEXTOP:				    ;AN000;
;Extended open hooks

IF NOT Installed
	transfer NET_SEQ_CREATE
ELSE
	PUSH	AX
	MOV	AX,(multNET SHL 8) OR 24
	INT	2FH
	POP	BX			; BX is trashed anyway
	return
ENDIF

;	We have a CDS.	See if it's network

TEST_RE_NET:
	TEST	ES:[SI.curdir_flags],curdir_isnet
	POP	ES
	JZ	LOCAL_CREATE

	CALL	Set_EXT_mode		    ;AN000;EO.
	JC	SHORT dochk		    ;AN000;EO.
	OR	ES:[DI.sf_mode],sharing_compat + open_for_both ;IFS.

;Extended open hooks

dochk:
	TEST	[EXTOPEN_ON],ext_open_on    ;AN000;EO. from extnded open
	JNZ	IFS_extopen		    ;AN000;EO. yes, issue extended open
;Extended open hooks

IF NOT Installed
	transfer NET_CREATE
ELSE
	PUSH	AX
	MOV	AX,(multNET SHL 8) OR 23
	INT	2FH
	POP	BX			; BX is trashed anyway
nomore:
	return
ENDIF


;**	It's a local create.  We have a local CDS for it.

LOCAL_CREATE:
	CALL	Set_EXT_mode	       ;AN000;EO. set mode if from extended open
	JC	setdone 	       ;AN000;EO.
	OR	ES:[DI].sf_mode,sharing_compat+open_for_both
setdone:
	EnterCrit   critDisk
	invoke	MakeNode
	JNC	Create_ok
	mov	[VOLCHNG_FLAG],-1	; indicate no change in volume label
	LeaveCrit   critDisk

entry	SET_MKND_ERR
	DOSAssume   <DS>,"Set_MkNd_Err"
	ASSUME	ES:NOTHING

;	Looks up MakeNode errors and converts them. AL is MakeNode
;	error, SI is GetPath bad spot return if path_not_found error.

;hkn; CRTERRTAB is in TABLE seg (DOSCODE)
	MOV     BX,OFFSET DOSCODE:CRTERRTAB
;hkn;	XLAT
	XLAT	byte ptr CS:[BX]
CreatBadRet:
	STC
	return

Public CREAT001S,CREAT001E
CREAT001S  label  byte
CRTERRTAB LABEL BYTE		; Lookup table for MakeNode returns
	DB	?			; none
	DB	error_access_denied	; MakeNode error 1
	DB	error_cannot_make	; MakeNode error 2
	DB	error_file_exists	; MakeNode error 3
	DB	error_path_not_found	; MakeNode error 4
	DB	error_access_denied	; MakeNode error 5
	DB	error_sharing_violation ; MakeNode error 6
	DB	error_file_not_found	; MakeNode error 7
CREAT001E  label byte

; We have just created a new file.  This results in the truncation of old
; files.  We must inform the sharer to slash all the open SFT's for this
; file to the current size.

; If we created a volume id on the diskette, set the VOLCHNG_FLAG to logical
; drive number to force a Build BPB after Media Check.

;;; FASTOPEN 8/29/86
Create_ok:
	invoke	FastOpen_Delete
;;; FASTOPEN 8/29/86
	mov	al,SATTRIB
	test	al,attr_volume_id
	jz	NoVolLabel
	LES	DI,THISCDS
	mov	ah,ES:CURDIR_TEXT[DI]	; get drive letter
	sub	ah,'A'                  ; convert to drive letter
	mov	VOLCHNG_FLAG,ah		;Set flag to indicate volid change
	MOV	BH,1			;AN000;>32mb set volume id to boot record
	CALL	Set_Media_ID		;AN000;>32mb
	EnterCrit CritDisk
	invoke	FatRead_CDS		; force a media check
	LeaveCrit CritDisk

NoVolLabel:
	MOV	ax,2
	LES	DI,ThisSFT
if installed
	call	JShare + 14 * 4
else
	Call	ShSU
endif
	LeaveCrit   critDisk
	transfer SET_SFT_MODE

EndProc DOS_Create

;---------------------------------------------------------------------------
;
; Procedure Name : Dos_Create_New
;
; Inputs:
;	[WFP_START] Points to WFP string ("d:/" must be first 3 chars, NUL
;		terminated)
;	[CURR_DIR_END] Points to end of Current dir part of string
;		( = -1 if current dir not involved, else
;		 Points to first char after last "/" of current dir part)
;	[THISCDS] Points to CDS being used
;		(Low word = -1 if NUL CDS (Net direct request))
;	[THISSFT] Points to SFT to fill in if file created
;		(sf_mode field set so that FCB may be detected)
;	[SATTRIB] Is attribute of search, determines what files can be found
;	AX is Attribute to create
; Function:
;	Try to create the specified file truncating an old one that exists
; Outputs:
;	sf_ref_count is NOT altered
;	CARRY CLEAR
;	    THISSFT filled in.
;		sf_mode = sharing_compat + open_for_both for Non-FCB SFT
;	CARRY SET
;	    AX is error code
;		error_path_not_found
;			Bad path (not in curr dir part if present)
;		error_bad_curr_dir
;			Bad path in current directory part of path
;		error_access_denied
;			Create a second volume id or create a dir
;		error_file_exists
;			Already a file by this name
; DS preserved, others destroyed
;---------------------------------------------------------------------------

procedure   DOS_Create_New,NEAR
	DOSAssume   <DS>,"DOS_Create_New"

	MOV	AH,1		; Truncate is NOT OK
	JMP	Create_inter

EndProc DOS_Create_New


;---------------------------------------------------------------------------
;
; Procedure Name : Set_Media_ID
;
; Inputs:
;	NAME1= Volume ID
;	BH= 0, delete volume id
;	    1, set new volume id
;	DS= DOSGROUP
; Function:
;	Set Volume ID to DOS 4.00 Boot record.
; Outputs:
;	CARRY CLEAR
;	    volume id set
;	CARRY SET
;	    AX is error code
;---------------------------------------------------------------------------

procedure   Set_Media_ID,NEAR
	DOSAssume   <DS>,"SetMedID"

	PUSH	AX		;AN000;;>32mb
	PUSH	ES		;AN000;;>32mb
	PUSH	DI		;AN000;;>32mb

	INC	AH		;AN000;;>32mb  bl=drive #
	MOV	BL,AH		;AN000;;>32mb  bl=drive # (A=1,B=2,,,)
	MOV	AL,0DH		;AN000;;>32mb  generic IOCTL
	MOV	CX,0866H	;AN001;;>32mb  get media id

;hkn; PACKET_TEMP is in DOSDATA
	MOV	DX,OFFSET DOSDATA:PACKET_TEMP	;AN000;>32mb

	PUSH	BX		;AN000;;>32mb
	PUSH	DX		;AN000;;>32mb
	XOR	BH,BH		;AN000;;>32mb

	invoke	$IOCTL		;AN000;;>32mb
	POP	DX		;AN000;;>32mb
	POP	BX		;AN000;;>32mb
	JC	geterr		;AN000;;>32mb

	OR	BH,BH		;AN000;;>32mb delete volume id
	JZ	NoName		;AN000;>32mb yes

;hkn; NAME1 is in DOSDATA
	MOV	SI,OFFSET DOSDATA:NAME1   ;AN000;>32mb

	JMP	SHORT doset	;AN000;>32mb yes
Noname: 			;AN000;

;hkn; NO_NAME_ID is in DOSDATA
	MOV	SI,OFFSET DOSDATA:NO_NAME_ID  ;AN000;>32mb

doset:					       ;AN000;
	MOV	DI,DX		;AN000;;>32mb
	ADD	DI,MEDIA_LABEL	;AN000;;>32mb

;hkn; ES & DS must point to SS
;hkn;	PUSH	CS		;AN000;;>32mb  move new volume id to packet
	PUSH	SS		;AN000;;>32mb  move new volume id to packet

	POP	DS		;AN000;;>32mb

;hkn;	PUSH	CS		;AN000;;>32mb
	PUSH	SS		;AN000;;>32mb

	POP	ES		;AN000;;>32mb
	MOV	CX,11		;AN000;;>32mb
	REP	MOVSB		;AN000;;>32mb
	MOV	CX,0846H	;AN001;;>32mb
	MOV	AL,0DH		;AN000;;>32mb
	XOR	BH,BH		;AN000;;>32mb
	invoke	$IOCTL		;AN000;;>32mb  set volume id
geterr: 			;AN000;

;hkn;	PUSH	CS		;AN000;>32mb
	PUSH	SS		;AN000;>32mb

	POP	DS		;AN000;>32mb   ds= dosgroup

	POP	DI		;AN000;;>32mb
	POP	ES		;AN000;;>32mb
	POP	AX		;AN000;;>32mb
	return			;AN000;>32mb

EndProc Set_Media_ID		;AN000;

;---------------------------------------------------------------------------
;
; Procedure Name : Sest_EXT_mode
;
; Inputs:
;	[EXTOPEN_ON]= flag for extende open
;	SAVE_BX= mode specified in Extended Open
; Function:
;	Set mode in ThisSFT
; Outputs:
;	carry set,mode is set if from Extended Open
;	carry clear, mode not set yet
;----------------------------------------------------------------------------

procedure   Set_EXT_mode,NEAR

;hkn; SS override
	TEST	[EXTOPEN_ON],ext_open_on    ;AN000;EO. from extnded open
	JZ	NOTEX			    ;AN000;EO. no, do normal
	PUSH	AX			    ;AN000;EO.

;hkn; SS override
	MOV	AX,[SAVE_BX]		    ;AN000;EO.
	OR	ES:[DI.sf_mode],AX	    ;AN000;EO.
	POP	AX			    ;AN000;EO.
	STC				    ;AN000;EO.
NOTEX:					    ;AN000;
	return				    ;AN000;EO.

EndProc Set_EXT_mode			    ;AN000;

DOSCODE	ENDS
	END
