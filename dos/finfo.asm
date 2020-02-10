	TITLE	FILE_INFO - Internal Get/Set File Info routines
	NAME	FILE_INFO

;
;	Microsoft Confidential
;	Copyright (C) Microsoft Corporation 1991
;	All Rights Reserved.
;

;**	Low level routines for returning file information and setting file
;	attributes
;
;	GET_FILE_INFO
;	SET_FILE_ATTRIBUTE
;
;	Modification history:
;
;	    Created: ARR 30 March 1983
;
;	M025: Return access_denied if attempting to set
;	      attribute of root directory.
;


	.xlist
	.xcref
	include version.inc
	include dosseg.inc
	INCLUDE DOSSYM.INC
	INCLUDE DEVSYM.INC
	include fastxxxx.inc
	include fastopen.inc
	include mult.inc
	.cref
	.list

Installed = TRUE

	i_need	THISCDS,DWORD
	i_need	CURBUF,DWORD
	i_need	NoSetDir,BYTE
	i_need	THISDRV,BYTE
	I_need	EXTERR_CLASS,BYTE
	I_need	EXTERR_ACTION set
	I_need	EXTERR_LOCUS,BYTE
	i_need	DMAADD,DWORD
	i_need	FastOpenFlg,BYTE

DOSCODE	SEGMENT
	ASSUME	SS:DOSDATA,CS:DOSCODE


SUBTTL GET_FILE_INFO -- Get File Information
PAGE
;----------------------------------------------------------------------------
;
; Procedure Name : GET_FILE_INFO
;
; Inputs:
;	[WFP_START] Points to WFP string ("d:/" must be first 3 chars, NUL
;		terminated)
;	[CURR_DIR_END] Points to end of Current dir part of string
;		( = -1 if current dir not involved, else
;		 Points to first char after last "/" of current dir part)
;	[THISCDS] Points to CDS being used
;		(Low word = -1 if NUL CDS (Net direct request))
;	[SATTRIB] Is attribute of search, determines what files can be found
; Function:
;	Get Information about a file
; Returns:
;	CARRY CLEAR
;	    AX = Attribute of file
;	    CX = Time stamp of file
;	    DX = Date stamp of file
;	    BX:DI = Size of file (32 bit)
;	CARRY SET
;	    AX is error code
;		error_file_not_found
;			Last element of path not found
;		error_path_not_found
;			Bad path (not in curr dir part if present)
;		error_bad_curr_dir
;			Bad path in current directory part of path
; DS preserved, others destroyed
;---------------------------------------------------------------------------


	procedure   GET_FILE_INFO,NEAR

;hkn; get_file_info is called from file.asm and fcbio.asm. DS has been set 
;hkn; to DOSDATA at this point. So DOSassume is OK.

	DOSAssume   <DS>,"Get_File_Info"
	ASSUME	ES:NOTHING

	Invoke	TestNet
	JNC	Local_Info

IF NOT Installed
	transfer NET_GET_FILE_INFO
ELSE
	MOV	AX,(multNET SHL 8) OR 15
	INT	2FH
	return
ENDIF

LOCAL_INFO:
	EnterCrit   critDisk
	MOV	[NoSetDir],1		; if we find a dir, don't change to it
	invoke	Get_FAST_PATH
info_check:
	JNC	info_check_dev

NO_PATH:
	DOSAssume   <DS>,"No_Path"
	ASSUME	ES:NOTHING

	JNZ	bad_path
	OR	CL,CL
	JZ	bad_path
info_no_file:
	MOV	AX,error_file_not_found
BadRet:
	STC
justRet:
	LeaveCrit   critDisk
	return

bad_path:
	MOV	AX,error_path_not_found
	jmp	BadRet

info_check_dev:
	OR	AH,AH
	JS	info_no_file		; device
;SR;
; If root dir then CurBuf == -1. Check for this case and return subdir attr
;for a root dir
;
	cmp	word ptr CurBuf,-1		;is it a root dir?
	jne	not_root		;no, CurBuf ptr is valid

	xor	ah,ah
	mov	al,ATTR_DIRECTORY
	clc
	jmp	short justret
not_root:

	PUSH	DS
	MOV	DS,WORD PTR [CURBUF+2]
ASSUME	DS:NOTHING
	MOV	SI,BX
	XOR	BX,BX			; Assume size=0 (dir)
	MOV	DI,BX
	MOV	CX,[SI.dir_time]
	MOV	DX,[SI.dir_date]
	XOR	AH,AH
	MOV	AL,[SI.dir_attr]
	TEST	AL,attr_directory
	JNZ	NO_SIZE
	MOV	DI,[SI.dir_size_l]
	MOV	BX,[SI.dir_size_h]
NO_SIZE:
	POP	DS
	CLC
	jmp	JustRet
EndProc GET_FILE_INFO

Break	<SET_FILE_ATTRIBUTE -- Set File Attribute>
;-------------------------------------------------------------------------------
;
; Procedure Name : SET_FILE_ATTRIBUTE
; Inputs:
;	[WFP_START] Points to WFP string ("d:/" must be first 3 chars, NUL
;		terminated)
;	[CURR_DIR_END] Points to end of Current dir part of string
;		( = -1 if current dir not involved, else
;		 Points to first char after last "/" of current dir part)
;	[THISCDS] Points to CDS being used
;		(Low word = -1 if NUL CDS (Net direct request))
;	[SATTRIB] is attribute of search (determines what files may be found)
;	AX is new attributes to give to file
; Function:
;	Set File Attributes
; Returns:
;	CARRY CLEAR
;	    No error
;	CARRY SET
;	    AX is error code
;		error_file_not_found
;			Last element of path not found
;		error_path_not_found
;			Bad path (not in curr dir part if present)
;		error_bad_curr_dir
;			Bad path in current directory part of path
;		error_access_denied
;			Attempt to set an attribute which cannot be set
;			(attr_directory, attr_volume_ID)
;		error_sharing_violation
;			Sharing mode of file did not allow the change
;			(this request requires exclusive write/read access)
;			(INT 24H generated)
; DS preserved, others destroyed
;----------------------------------------------------------------------------

	procedure   SET_FILE_ATTRIBUTE,NEAR

;hkn; set_file_attr is called from file.asm. DS has been set 
;hkn; to DOSDATA at this point. So DOSassume is OK.

	DOSAssume   <DS>,"Set_File_Attribute"
    ASSUME ES:NOTHING

	TEST	AX,NOT attr_changeable
	JZ	set_look
BAD_ACC:
	MOV	ExtErr_Locus,errLoc_UNK
	MOV	ExtErr_Class,errClass_Apperr
	MOV	ExtErr_Action,errAct_Abort
	MOV	AX,error_access_denied
	STC
	return

set_look:
	Invoke	TestNet
	JNC	Local_Set

IF NOT Installed
	transfer NET_SEQ_SET_FILE_ATTRIBUTE
ELSE
	PUSH	AX
	MOV	AX,(multNET SHL 8) OR 14
	INT	2FH
	POP	BX			; clean stack
	return
ENDIF

LOCAL_SET:
	EnterCrit   critDisk
	PUSH	AX			; Save new attributes
	MOV	[NoSetDir],1		; if we find a dir, don't change to it
	invoke	GetPath 		; get path through fastopen if there	 ;AC000;
	JNC	set_check_device
	POP	BX			; Clean stack (don't zap AX)
	JMP	NO_PATH

set_check_device:
	OR	AH,AH
	JNS	set_check_share
	POP	AX
	LeaveCrit   critDisk
	JMP	BAD_ACC 		; device

set_check_share:
	POP	AX			; Get new attributes

	cmp	word ptr [CURBUF], -1	; M025: Q: is this the root dir
	je	cannot_set_root		; M025: Y: return error

	invoke	REN_DEL_Check
	JNC	set_do
	MOV	AX,error_sharing_violation
	jmp	short	ok_bye

cannot_set_root:			; M025:
	mov	ax, error_access_denied	; M025: return error is attempting
	stc				; M025: to set attr. of root
	jmp	short ok_bye		; M025:

set_do:
	LES	DI,[CURBUF]
	AND	BYTE PTR ES:[BX].dir_attr,NOT attr_changeable
	OR	BYTE PTR ES:[BX].dir_attr,AL

	TEST	ES:[DI.buf_flags],buf_dirty  ;LB. if already dirty		;AN000;
	JNZ	yesdirty		  ;LB.	  don't increment dirty count   ;AN000;
	invoke	INC_DIRTY_COUNT 	  ;LB.					;AN000;
	OR	ES:[DI.buf_flags],buf_dirty
yesdirty:
	MOV	AL,[THISDRV]
;;;; 10/1/86 F.C update fastopen cache
	PUSH	DX
	PUSH	DI
	MOV	AH,0		  ; dir entry update
	MOV	DL,AL		  ; drive number A=0,B=1,,
	MOV	DI,BX		  ; ES:DI -> dir entry
	invoke	FastOpen_Update
	POP	DI
	POP	DX
;;;; 9/11/86 F.C update fastopen cache
	invoke	FlushBuf
	JNC	OK_BYE
	MOV	AX,error_file_not_found
OK_BYE:
	LeaveCrit   critDisk
	return

EndProc SET_FILE_ATTRIBUTE



	procedure   GET_FAST_PATH,NEAR
	ASSUME	DS:NOTHING,ES:NOTHING

;hkn; use SS override for FastOpenFlg
	OR	[FastOpenFlg],FastOpen_Set ;FO. trigger fastopen		;AN000;
	invoke	GetPath
	PUSHF			       ;FO.					;AN000;
	AND    [FastOpenFlg],Fast_yes  ;FO. clear all fastopen flags		;AN000;
	POPF			       ;FO.					;AN000;
	return

EndProc GET_FAST_PATH

DOSCODE	ENDS
    END

