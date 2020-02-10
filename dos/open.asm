	TITLE	DOS_OPEN - Internal OPEN call for MS-DOS
	NAME	DOS_OPEN

;
;	Microsoft Confidential
;	Copyright (C) Microsoft Corporation 1991
;	All Rights Reserved.
;

;**	OPEN.ASM - File Open
;
;	Low level routines for openning a file from a file spec.
;	Also misc routines for sharing errors
;
;	DOS_Open
;	Check_Access_AX
;	SHARE_ERROR
;	SET_SFT_MODE
;	Code_Page_Mismatched_Error		   ; DOS 4.00
;
;	Revision history:
;
;	    Created: ARR 30 March 1983
;	    A000	version 4.00   Jan. 1988
;
;	M034 - The value in save_bx must be pushed on to the stack for
; 	       remote extended opens and not save_cx.
;
;	M035 - if open made from exec then we must set the appropriate bits
;	       on the stack before calling off to the redir.
;	M042 - Bit 11 of DOS34_FLAG set indicates that the redir knows how 
;	       to handle open from exec. In this case set the appropriate bit
;	       else do not.
	

	.xlist
	.xcref
	include version.inc
	include dosseg.inc
	include dossym.inc
	include devsym.inc
	include fastopen.inc
	include fastxxxx.inc
	include sf.inc
	include mult.inc
	include filemode.inc
	include curdir.inc

	.cref
	.list

Installed = TRUE

	i_need	NoSetDir,BYTE
	i_need	THISSFT,DWORD
	i_need	THISCDS,DWORD
	i_need	CURBUF,DWORD
	i_need	CurrentPDB,WORD
	i_need	CURR_DIR_END,WORD
	I_need	RetryCount,WORD
	I_need	Open_Access,BYTE
	I_need	fSharing,BYTE
	i_need	JShare,DWORD
	I_need	FastOpenFlg,byte
	I_need	EXTOPEN_ON,BYTE 		  ;AN000;; DOS 4.00
	I_need	ALLOWED,BYTE			  ;AN000;; DOS 4.00
	I_need	EXTERR,WORD			  ;AN000;; DOS 4.00
	I_need	EXTERR_LOCUS,BYTE		  ;AN000;; DOS 4.00
	I_need	EXTERR_ACTION,BYTE		  ;AN000;; DOS 4.00
	I_need	EXTERR_CLASS,BYTE		  ;AN000;; DOS 4.00
	I_need	CPSWFLAG,BYTE			  ;AN000;; DOS 4.00
	I_need	EXITHOLD,DWORD			  ;AN000;; DOS 4.00
	I_need	THISDPB,DWORD			  ;AN000;; DOS 4.00
	I_need	SAVE_CX,WORD			  ;AN000;; DOS 4.00
	I_need	SAVE_BX,WORD			  ;M034

	I_need	DOS_FLAG,BYTE
	I_need	DOS34_FLAG,WORD			  ;M042



DOSCODE	SEGMENT
	ASSUME	SS:DOSDATA,CS:DOSCODE

Break	<DOS_Open - internal file access>
;---------------------------------------------------------------------------
;
; Procedure Name : DOS_Open
;
; Inputs:
;	[WFP_START] Points to WFP string ("d:/" must be first 3 chars, NUL
;		terminated)
;	[CURR_DIR_END] Points to end of Current dir part of string
;		( = -1 if current dir not involved, else
;		 Points to first char after last "/" of current dir part)
;	[THISCDS] Points to CDS being used
;		(Low word = -1 if NUL CDS (Net direct request))
;	[THISSFT] Points to SFT to fill in if file found
;		(sf_mode field set so that FCB may be detected)
;	[SATTRIB] Is attribute of search, determines what files can be found
;	AX is Access and Sharing mode
;	  High NIBBLE of AL (Sharing Mode)
;		sharing_compat	   file is opened in compatibility mode
;		sharing_deny_none  file is opened Multi reader, Multi writer
;		sharing_deny_read  file is opened Only reader, Multi writer
;		sharing_deny_write file is opened Multi reader, Only writer
;		sharing_deny_both  file is opened Only reader, Only writer
;	  Low NIBBLE of AL (Access Mode)
;		open_for_read	file is opened for reading
;		open_for_write	file is opened for writing
;		open_for_both	file is opened for both reading and writing.
;
;	  For FCB SFTs AL should = sharing_compat + open_for_both
;		(not checked)
; Function:
;	Try to open the specified file
; Outputs:
;	sf_ref_count is NOT altered
;	CARRY CLEAR
;	    THISSFT filled in.
;	CARRY SET
;	    AX is error code
;		error_file_not_found
;			Last element of path not found
;		error_path_not_found
;			Bad path (not in curr dir part if present)
;		error_bad_curr_dir
;			Bad path in current directory part of path
;		error_invalid_access
;			Bad sharing mode or bad access mode or bad combination
;		error_access_denied
;			Attempt to open read only file for writting, or
;			open a directory
;		error_sharing_violation
;			The sharing mode was correct but not allowed
;			generates an INT 24 on compatibility mode SFTs
; DS preserved, others destroyed
;----------------------------------------------------------------------------

procedure   DOS_Open,NEAR

	; DS has been set up to DOSDATA in file.asm and fcbio2.asm. 

	DOSAssume   <DS>,"DOS_Open"

	MOV	[NoSetDir],0
	CALL	Check_Access_AX
	JC	ret_label		    ; retc
	LES	DI,[THISSFT]
	XOR	AH,AH

	; sleaze! move only access/sharing mode in.  Leave sf_isFCB unchanged

	MOV	BYTE PTR ES:[DI.sf_mode],AL ; For moment do this on FCBs too
	PUSH	ES
	LES	SI,[THISCDS]
	CMP	SI,-1
	JNZ	TEST_RE_NET
	POP	ES
;Extended open hooks

	TEST	[EXTOPEN_ON],ext_open_on    ;FT. from extnded open		;AN000;
	JZ	NOEXTOP 		    ;FT. no, do normal			;AN000;
IFS_extopen:									;AN000;
	MOV	AL,byte ptr [SAVE_BX]	    ; M034 - save_bx has original bx  
					    ; with which call was made. This
					    ; has the open access bits. 
;;	MOV	AL,byte ptr [SAVE_CX]	    ; M034 - FT. al= create attribute

	PUSH	AX			    ;FT. pass create attr to IFS	;AN000;
	MOV	AX,(multNET SHL 8) OR 46    ;FT. issue extended open verb	;AN000;
	INT	2FH			    ;FT.				;AN000;
	POP	BX			    ;FT. trash bx			;AN000;
	MOV	[EXTOPEN_ON],0		    ;FT.				;AN000;

ret_label:
	return				    ;FT.				;AN000;
NOEXTOP:

;Extended open hooks

IF NOT Installed
	transfer NET_SEQ_OPEN
ELSE

do_net_int2f:


	testb	[DOS_FLAG], EXECOPEN	; Q: was this open call made from exec
	jz	not_exec_open		; N: just do net open
					; Y: check to see if redir is aware
					;    of this 

					; M042 - start
	testb	[DOS34_FLAG], EXEC_AWARE_REDIR
					; Q: does this redir know how to 
					;    this
	jz	not_exec_open		; N: just do net open
					; Y: set bit 3 of access byte and 
					;    set sharing mode to DENY_WRITE
					; M042 - end

	; NOTE: This specific mode has not been set for the code assembled
	; under the "NOT Installed" conditional. Currently Installed is 
	; always one.
					; M035 - set the bits on the stack
	mov	AL, SHARING_DENY_WRITE+EXEC_OPEN

not_exec_open:

	PUSH	AX
	MOV	AX,(multNET SHL 8) OR 22
	INT	2FH
	POP	BX			; clean stack
	return
ENDIF

TEST_RE_NET:
	TEST	ES:[SI.curdir_flags],curdir_isnet
	POP	ES
	JZ	LOCAL_OPEN

;Extended open hooks

	TEST	[EXTOPEN_ON],ext_open_on    ;FT. from extnded open		;AN000;
	JNZ	IFS_extopen		    ;FT. isuue extended open		;AN000;

;Extended open hooks

IF NOT Installed
	transfer NET_OPEN
ELSE

	jmp	short do_net_int2f

;**	PUSH	AX
;**	MOV	AX,(multNET SHL 8) OR 22
;**	INT	2FH
;**	POP	BX			    ; clean stack
;**	return

ENDIF

LOCAL_OPEN:
	EnterCrit   critDisk

; DOS 3.3 FastOPen 6/16/86

	OR	[FastOpenFlg],FastOpen_Set+Special_Fill_Set   ;  only open can
	invoke	GetPath


; DOS 3.3 FastOPen 6/16/86

	JNC	Open_found
	JNZ	bad_path
	OR	CL,CL
	JZ	bad_path
OpenFNF:
	MOV	AX,error_file_not_found
OpenBadRet:

;hkn; FastOpenFlg is in DOSDATA use SS override
	AND	BYTE PTR SS:[FastOpenFlg],Fast_yes    ;; DOS 3.3
	STC
	LeaveCrit   critDisk
	JMP	Clear_FastOpen

bad_path:
	MOV	AX,error_path_not_found
	JMP	OpenBadRet

open_bad_access:
	MOV	AX,error_access_denied
	JMP	OpenBadRet

Open_found:
	JZ	Open_Bad_Access 	; test for directories
	OR	AH,AH
	JS	open_ok 		; Devices don't have attributes
	MOV	ES,WORD PTR [CURBUF+2]	; get buffer location
	MOV	AL,ES:[BX].dir_attr
	TEST	AL,attr_volume_id	; can't open volume ids
	JNZ	open_bad_access
	TEST	AL,attr_read_only	; check write on read only
	JZ	open_ok
;
; The file is marked READ-ONLY.  We verify that the open mode allows access to
; the read-only file.  Unfortunately, with FCB's and net-FCB's we cannot
; determine at the OPEN time if such access is allowed.  Thus, we defer such
; processing until the actual write operation:
;
; If FCB, then we change the mode to be read_only.
; If net_FCB, then we change the mode to be read_only.
; If not open for read then error.
;
	SAVE	<DS,SI>
	LDS	SI,[THISSFT]
	MOV	CX,[SI].sf_mode
	TEST	CX,sf_isFCB		; is it FCB?
	JNZ	ResetAccess		; yes, reset the access
	MOV	DL,CL
	AND	DL,sharing_mask
	CMP	DL,sharing_net_FCB	; is it net FCB?
	JNZ	NormalOpen		; no
ResetAccess:
	AND	CX,NOT access_mask	; clear access
	.errnz	open_for_read
;	OR	CX,open_for_read	; stick in open_for_read
	MOV	[SI].sf_mode,CX
	JMP	SHORT FillSFT
;
; The SFT is normal.  See if the requested access is open_for_read
;
NormalOpen:
	AND	CL,access_mask		; remove extras
	CMP	CL,open_for_read	; is it open for read?
	JZ	FillSFT
	RESTORE <SI,DS>
	JMP	short open_bad_access
;
; All done, restore registers and fill the SFT.
;
FillSFT:
	RESTORE <SI,DS>
open_ok:
	invoke	DOOPEN			; Fill in SFT

;hkn; FastOpenFlg is in DOSDATA. use SS override
	AND	BYTE PTR SS:[FastOpenFlg],Fast_yes    ;; DOS 3.3
	CALL	DO_SHARE_CHECK		;
	JNC	Share_Ok
	LeaveCrit   critDisk
	JMP	short Clear_FastOPen

SHARE_OK:
	MOV	AX,3
	LES	DI,ThisSFT
if installed
	call	JShare + 14 * 4
else
	Call	ShSU
endif
	LeaveCrit   critDisk
	FallThru    Set_SFT_Mode

EndProc DOS_Open,NoCheck

;----------------------------------------------------------------------------
; Procedure Name : SET_SFT_MODE
;
; Finish SFT initialization for new reference.	Set the correct mode.
;
;   Inputs:
;	ThisSFT points to SFT
;
;   Outputs:
;	Carry clear
;   Registers modified: AX.
;---------------------------------------------------------------------------

;hkn; called from create. DS already set up to DOSDATA.

PROCEDURE Set_SFT_Mode,NEAR

	DOSAssume   <DS>,"Set_SFT_Mode"
	LES	DI,ThisSFT
	invoke	DEV_OPEN_SFT
	TEST	ES:[DI.sf_mode],sf_isfcb; Clears carry
	JZ	Clear_FastOpen		; sf_mode correct (retz)
	MOV	AX,[CurrentPDB]
	MOV	ES:[DI.sf_PID],AX	; For FCB sf_PID=PDB

Clear_FastOpen:
	return			       ;;;;; DOS 3.3

ENDPROC Set_SFT_MODE

;----------------------------------------------------------------------------
;
; Procedure Name : SHARE_ERROR
;
; Called on sharing violations. ES:DI points to SFT. AX has error code
; If SFT is FCB or compatibility mode gens INT 24 error.
; Returns carry set AX=error_sharing_violation if user says ignore (can't
; really ignore).  Carry clear
; if user wants a retry. ES, DI, DS preserved
;---------------------------------------------------------------------------

procedure SHARE_ERROR,NEAR
	DOSAssume   <DS>,"Share_Error"

	TEST	ES:[DI.sf_mode],sf_isfcb
	JNZ	HARD_ERR
	MOV	CL,BYTE PTR ES:[DI.sf_mode]
	AND	CL,sharing_mask
	CMP	CL,sharing_compat
	JNE	NO_HARD_ERR
HARD_ERR:
	invoke	SHARE_VIOLATION
	retnc				; User wants retry
NO_HARD_ERR:
	MOV	AX,error_sharing_violation
	STC
	return

EndProc SHARE_ERROR

;----------------------------------------------------------------------------
;
; Procedure Name : DO_SHARE_CHECK
;
; Input: THISDPB, WFP_Start, THISSFT set
; Functions: check file sharing mode is valid
; Output: carry set, error
;	  carry clear, share ok
;----------------------------------------------------------------------------

procedure DO_SHARE_CHECK,NEAR
	DOSAssume   <DS>,"DO_SHARE__CHECK"
	EnterCrit   critDisk		; enter critical section

OPN_RETRY:
	MOV	CX,RetryCount		; Get # tries to do
OpenShareRetry:
	SAVE	<CX>			; Save number left to do
	invoke	SHARE_CHECK		; Final Check
	RESTORE <CX>		; CX = # left
	JNC	Share_Ok2		; No problem with access
	Invoke	Idle
	LOOP	OpenShareRetry		; One more retry used up
OpenShareFail:
	LES	DI,[ThisSft]
	invoke	SHARE_ERROR
	JNC	OPN_RETRY		; User wants more retry
Share_Ok2:
	LeaveCrit   critDisk		; leave critical section
	return

EndProc DO_SHARE_CHECK

;-----------------------------------------------------------------------------
;
; Procedure Name : Check_Access
;
; Inputs:
;	AX is mode
;	  High NIBBLE of AL (Sharing Mode)
;		sharing_compat	   file is opened in compatibility mode
;		sharing_deny_none  file is opened Multi reader, Multi writer
;		sharing_deny_read  file is opened Only reader, Multi writer
;		sharing_deny_write file is opened Multi reader, Only writer
;		sharing_deny_both  file is opened Only reader, Only writer
;	  Low NIBBLE of AL (Access Mode)
;		open_for_read	file is opened for reading
;		open_for_write	file is opened for writing
;		open_for_both	file is opened for both reading and writing.
; Function:
;	Check this access mode for correctness
; Outputs:
;	[open_access] = AL input
;	Carry Clear
;		Mode is correct
;		AX unchanged
;	Carry Set
;		Mode is bad
;		AX = error_invalid_access
; No other registers effected
;----------------------------------------------------------------------------

procedure Check_Access_AX
	DOSAssume   <DS>,"Check_Access"

	MOV	Open_Access,AL
	PUSH	BX

;	If sharing, then test for special sharing mode for FCBs

	MOV	BL,AL
	AND	BL,sharing_mask
	CMP	fSharing,-1
	JNZ	CheckShareMode		; not through server call, must be ok
	CMP	BL,sharing_NET_FCB
	JZ	CheckAccessMode 	; yes, we have an FCB
CheckShareMode:
	CMP	BL,40h			; is this a good sharing mode?
	JA	Make_Bad_Access
CheckAccessMode:
	MOV	BL,AL
	AND	BL,access_mask
	CMP	BL,2
	JA	Make_Bad_Access
	POP	BX
	CLC
	return

make_bad_access:
	MOV	AX,error_invalid_access
	POP	BX
	STC
	return

EndProc Check_Access_AX


DOSCODE	ENDS
	END


