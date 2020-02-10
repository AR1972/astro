	TITLE	FILE - Pathname related system calls
	NAME	FILE

;
;	Microsoft Confidential
;	Copyright (C) Microsoft Corporation 1991
;	All Rights Reserved.
;

;**	FILE.ASM - Pathname Related System Calls
;
;	These will be passed direct text of the
;	pathname from the user.  They will need to be passed through the macro
;	expander prior to being sent through the low-level stuff.  I/O specs are
;	defined in DISPATCH.	The system calls are:
;
;	$Open	      written
;	$Creat	      written
;	$ChMod	      written
;	$Unlink	      written
;	$Rename	      written
;	$CreateTempFile   written
;	$CreateNewFile	  written
;	$Extended_Open	  written  DOS 4.00
;	GetIOParms	      written  DOS 4.00
;
;	Revision history:
;
;	    Created: MZ 4 April 1983
;	    A000   version 4.00  Jan. 1988
;	    A001   DOS 4.0, PTM #6069, BL, ROMDOS fix
;           M022   conditional removed 
;	    M049   If access_denied is obtained whle trying to create a temp
;		   file, check to see if this was due to the network drive
; 		   being read only.
;
;	M069	Bug #5913. CreateTempFile now checks for error_net_access_
;		denied instead of error_cannot_make because ExtErr is set
;		to the first errorcode when we try to create a file on a
;		read-only share.
;
;	M070	Bug #5943. CreateTempFile now has a check for
;		error_failed_I24 which is set if the user fails on an I24
;		that is got on a write-protected disk. Previously, the
;		code would go ahead and try another name and then hang
;		because DOS is in an inconsistent state.
;

	.xlist
	.xcref
	include version.inc
	include dosseg.inc
	include dossym.inc
	include devsym.inc
	include sf.inc
	include filemode.inc
	include bugtyp.inc
	.cref
	.list

	I_need	WFP_Start,WORD		; pointer to beginning of expansion
	I_Need	ThisCDS,DWORD		; pointer to curdir in use
	I_need	ThisSft,DWORD		; SFT pointer for DOS_Open
	I_need	pJFN,DWORD		; temporary spot for pointer to JFN
	I_need	JFN,WORD		; word JFN for process
	I_need	SFN,WORD		; word SFN for process
	I_Need	OpenBuf,128		; buffer for filename
	I_Need	RenBuf,128		; buffer for filename in rename
	I_need	Sattrib,BYTE		; byte attribute to search for
	I_need	Ren_WFP,WORD		; pointer to real path
	I_need	cMeta,BYTE
	I_need	EXTERR,WORD		; extended error code
	I_need	EXTERR_LOCUS,BYTE	; Extended Error Locus
	i_need	JShare,DWORD		; share jump table
	I_need	fSharing,BYTE		; TRUE => via ServerDOSCall
	I_need	FastOpenTable,BYTE
	I_need	CPSWFLAG,BYTE		;AN000;FT. cpsw falg
	I_need	EXTOPEN_FLAG,WORD	;AN000;FT. extended file open flag
	I_need	EXTOPEN_ON,BYTE 	;AN000;FT. extended open flag
	I_need	EXTOPEN_IO_MODE,WORD	;AN000;FT. IO mode
	I_need	SAVE_ES,WORD		;AN000;;FT. for get/set XA
	I_need	SAVE_DI,WORD		;AN000;;FT. for get/set XA
	I_need	SAVE_DS,WORD		;AN000;;FT. for get/set XA
	I_need	SAVE_SI,WORD		;AN000;;FT. for get/set XA
	I_need	SAVE_DX,WORD		;AN000;;FT. for get/set XA
	I_need	SAVE_BX,WORD		;AN000;;FT. for get/set XA
	I_need	SAVE_CX,WORD		;AN000;;FT. for get/set XA
	I_need	Temp_Var,WORD		;AN000;;
	I_need	DOS34_FLAG,WORD 	;AN000;;
	I_need	Temp_Var2,WORD		;AN000;;

DOSCODE	SEGMENT
	ASSUME	SS:DOSDATA,CS:DOSCODE
	EXTRN	DOS_OPEN:NEAR,DOS_CREATE:NEAR,DOS_Create_New:NEAR

	EXTRN	SFNFree:near
        extrn	Set_EXT_mode:near               ;M022 conditional removed

	BREAK <$Open - open a file from a path string>


;**	$OPen - Open a File
;
;	given a path name in DS:DX and an open mode in AL, $Open opens the
;	file and and returns a handle
;
;	ENTRY	(DS:DX) = pointer to asciz name
;		(AL) = open mode
;	EXIT	'C' clear if OK
;		  (ax) = file handle
;		'C' set iff error
;		  (ax) = error code
;	USES	all

Procedure   $Open,NEAR
	DOSAssume   <SS>,"Open"

	fmt TypSysCall,LevArgs,<" Mode = $x file = '$S'\n">,<AX,DS,DX>
	XOR	AH,AH
Entry $Open2				;AN000;
	mov	ch,attr_hidden+attr_system+attr_directory
	call	SetAttrib
	MOV	CX,OFFSET DOSCODE:DOS_Open ; address of routine to call
	SAVE	<AX>			; Save mode on stack
IFDEF DBCS 				;AN000;
	MOV	Temp_Var,0		;AN000;KK. set variable with 0;smr;SS Override
ENDIF					;AN000;


;*	General file open/create code.	The $CREATE call and the various
;	$OPEN calls all come here.
;
;	We'll share a lot of the standard stuff of allocating SFTs, cracking
;	path names, etc., and then dispatch to our individual handlers.
;	WARNING - this info and list is just a guess, not definitive - jgl
;
;	(TOS) = create mode
;	(CX) = address of routine to call to do actual function
;	(DS:DX) = ASCIZ name
;	SAttrib = Attribute mask

;	Get a free SFT and mark it "being allocated"

AccessFile:
IFDEF  DBCS				;AN000;
	TEST	Temp_Var,ATTR_VOLUME_ID	; volume id bit set?
	JZ	novol
	OR	[DOS34_FLAG],DBCS_VOLID ; warn transpath about VOLID
novol:
ENDIF
	EnterCrit   critSFT
	call	SFNFree 		; get a free sfn
	LeaveCrit   critSFT
	JC	OpenFailJ		; oops, no free sft's
	MOV	SFN,BX			; save the SFN for later;smr;SS Override
	fmt	TypAccess,LevSFN,<"AccessFile setting SFN to $x\n">,<BX>
	MOV	WORD PTR ThisSFT,DI	; save the SF offset	;smr;SS Override
	MOV	WORD PTR ThisSFT+2,ES	; save the SF segment	;smr;SS Override

;	Find a free area in the user's JFN table.

	invoke	JFNFree 		; get a free jfn
	JNC	SaveJFN
OpenFailJ:
	JMP	OpenFail		; there were free JFNs... try SFN

SaveJFN:MOV	WORD PTR pJFN,DI	; save the jfn offset	;smr;SS Override
	MOV	WORD PTR pJFN+2,ES	; save the jfn segment	;smr;SS Override
	MOV	JFN,BX			; save the jfn itself	;smr;SS Override

;	We have been given an JFN.  We lock it down to prevent other tasks from
;	reusing the same JFN.

	MOV	BX,SFN						;smr;SS Override
	MOV	ES:[DI],BL		; assign the JFN
	MOV	SI,DX			; get name in appropriate place
	MOV	DI,OFFSET DOSDATA:OpenBuf  ; appropriate buffer
	SAVE	<CX>			; save routine to call
	invoke	TransPath		; convert the path
	RESTORE <BX>			; (bx) = routine to call
	LDS	SI,ThisSFT					;smr;SS Override
    ASSUME DS:NOTHING
	JC	OpenCleanJ		; no error, go and open file
	CMP	cMeta,-1					;smr;SS Override
	JZ	SetSearch
	MOV	AL,error_file_not_found ; no meta chars allowed
OpenCleanJ:
	JMP	short OpenClean

SetSearch:
	RESTORE <AX>		; Mode (Open), Attributes (Create)

;	We need to get the new inheritance bits.

	xor	cx,cx
	MOV	[SI].sf_mode,cx		; initialize mode field to 0
	MOV	[SI].SF_mft,cx		; clean out sharing info
	CMP	BX,OFFSET DOSCODE:DOS_OPEN
	JNZ	DoOper
	TEST	AL,sharing_no_inherit	; look for no inher
	JZ	DoOper
	AND	AL,07Fh 		; mask off inherit bit
	MOV	CX,sf_no_inherit
DoOper:

;**	Check if this is an extended open. If so you must set the
;	modes in sf_mode. Call Set_EXT_mode to do all this. See
;	Set_EXT_mode in creat.asm

	SAVE	<di, es>                ;M022 conditional removed here
	push	ds
	pop	es
	push	si
	pop	di		; (es:di) = SFT address
	call	Set_EXT_mode
	RESTORE <es, di>

	Context DS
	SAVE	<CX>
	CALL	BX			; blam!
	RESTORE <CX>
	LDS	SI,ThisSFT
	ASSUME	DS:NOTHING
	JC	OpenE2			;AN000;FT. chek extended open hooks first

;	The SFT was successfully opened.  Remove busy mark.

OpenOK:
	ASSUME	DS:NOTHING
	MOV	[SI].sf_ref_count,1
	OR	[SI].sf_flags,CX	; set no inherit bit if necessary

; If the open mode is 70, we scan the system for other SFT's with the same
; contents.  If we find one, then we can 'collapse' thissft onto the already
; opened one.  Otherwise we use this new one.  We compare uid/pid/mode/mft
;
; Since this is only relevant on sharer systems, we stick this code into the
; sharer.

	MOV	AX,JFN						;smr;SS Override
if installed
	Call	JShare + 12 * 4					;smr;SS Override
else
	Call	ShCol						;PBUGBUG
endif
	fmt	TypAccess,LevSFN,<"AccessFile setting SFN to -1\n">
	MOV	SFN,-1			; clear out sfn pointer	;smr;SS Override
	transfer    Sys_Ret_OK		; bye with no errors
;Extended Open hooks check
OpenE2: 				   ;AN000;;EO.
	CMP	AX,error_invalid_parameter ;AN000;;EO. IFS extended open ?
	JNZ	OpenE			   ;AN000;;EO. no.
	JMP	short OpenCritLeave		   ;AN000;;EO. keep handle

;	Extended Open hooks check
;
;	AL has error code.  Stack has argument to dos_open/dos_create.

OpenClean:
	fmt TypSysCall,LevErr,<"Return value from transpath $x\n">,<AX>
	RESTORE <bx>		; clean off stack
OpenE:
	MOV	[SI.SF_Ref_Count],0	; release SFT
	LDS	SI,pJFN						;smr;SS Override
	MOV	BYTE PTR [SI],0FFh	; free the SFN...
	JMP	SHORT OpenCritLeave

OpenFail:
	STI
	RESTORE <CX>		; Clean stack
OpenCritLeave:
	MOV	SFN,-1			; remove mark.
	fmt TypSysCall,LevErr,<"Open/CreateXX: error $x\n">,<AX>
;; File Tagging DOS 4.00
	CMP	[EXTERR],error_Code_Page_Mismatched	;AN000;;FT. code page mismatch;smr;SS Override
	JNZ	NORERR					;AN000;;FT. no
	transfer From_GetSet				;AN000;;FT. yes
NORERR: 						;AN000;

;; File Tagging DOS 4.00
	transfer    Sys_Ret_Err 	; no free, return error

EndProc $Open, NoCheck


	BREAK <$Creat - create a brand-new file>


;**	$Creat - Create a File
;
;	$Creat creates the directory entry specified in DS:DX and gives it the
;	initial attributes contained in CX
;
;	ENTRY	(DS:DX) = ASCIZ path name
;		(CX) = initial attributes
;	EXIT	'C' set if error
;		  (ax) = error code
;		'C' clear if OK
;		  (ax) = file handle
;	USES	all

Procedure   $Creat,NEAR
	DOSAssume   <SS>,"Creat"

	fmt TypSysCall,LevArgs,<"file = '$S'\n">,<DS,DX>
IFDEF DBCS 				;AN000;
	MOV	Temp_Var,CX		;AN000;KK. set variable with attribute	      ;AN000;smr;SS Override
ENDIF					;AN000;
	SAVE	<CX>			; Save attributes on stack
	MOV	CX,OFFSET DOSCODE:DOS_Create; routine to call
AccessSet:
	mov	SAttrib,attr_hidden+attr_system			;smr;SS Override
	JMP	AccessFile		; use good ol' open

EndProc $Creat, NoCheck


	BREAK <$CHMOD - change file attributes>

;**	$CHMOD - Change File Attributes
;
;   Assembler usage:
;	    LDS     DX, name
;	    MOV     CX, attributes
;	    MOV     AL,func (0=get, 1=set)
;	    INT     21h
;   Error returns:
;	    AX = error_path_not_found
;	    AX = error_access_denied
;

procedure $CHMOD,NEAR
	ASSUME	CS:DOSCODE,SS:DOSDATA
	MOV	DI,OFFSET DOSDATA:OpenBuf  ; appropriate buffer
	SAVE	<AX,CX> 		; save function and attributes
	MOV	SI,DX			; get things in appropriate places
	invoke	TransPathSet		; get correct path
	RESTORE <CX,AX>		; and get function and attrs back
	JC	ChModErr		; errors get mapped to path not found
	Context DS			; set up for later possible calls
	CMP	cMeta,-1
	JNZ	ChModErr
	MOV	[SAttrib],attr_hidden+attr_system+attr_directory
	SUB	AL,1			; fast way to discriminate
	JB	ChModGet		; 0 -> go get value
	JZ	ChModSet		; 1 -> go set value
	MOV	EXTERR_LOCUS,errLoc_Unk ; Extended Error Locus
	error	error_invalid_function	; bad value
ChModGet:
	invoke	Get_File_Info		; suck out the ol' info
	JC	ChModE			; error codes are already set for ret
	invoke	Get_User_stack		; point to user saved vaiables
	assume	DS:NOTHING		;smr;PBUGBUG
	MOV	[SI.User_CX],AX 	; return the attributes
	transfer    Sys_Ret_OK		; say sayonara
ChModSet:
	MOV	AX,CX			; get attrs in position
	invoke	Set_File_Attribute	; go set
	JC	ChModE			; errors are set
	transfer    Sys_Ret_OK
ChModErr:
	mov	al,error_path_not_found
ChmodE:
	Transfer    SYS_RET_ERR
EndProc $ChMod

	BREAK <$UNLINK - delete a file entry>

;**	$UNLINK - Delete a File
;
;
;	Assembler usage:
;	    LDS     DX, name
;	    IF VIA SERVER DOS CALL
;	     MOV     CX,SEARCH_ATTRIB
;	    MOV     AH, Unlink
;	    INT     21h
;
;	ENTRY	(ds:dx) = path name
;		(cx) = search_attribute, if via server_dos
;	EXIT	'C' clear if no error
;		'C' set if error
;		  (ax) = error code
;			= error_file_not_found
;			= error_access_denied

procedure $UNLINK,NEAR
	ASSUME	CS:DOSCODE,SS:DOSDATA

	SAVE	<CX>			; Save possible CX input parm
	MOV	SI,DX			; Point at input string
	MOV	DI,OFFSET DOSDATA:OpenBuf  ; temp spot for path
	invoke	TransPathSet		; go get normalized path
	RESTORE <CX>
	JC	ChModErr		; badly formed path
	CMP	cMeta,-1		; meta chars?		;smr;SS Override
	JNZ	NotFound
	Context DS
	mov	ch,attr_hidden+attr_system   ; unlink appropriate files
	call	SetAttrib
	invoke	DOS_Delete		; remove that file
	JC	UnlinkE 		; error is there
	transfer    Sys_Ret_OK		; okey doksy
NotFound:
	MOV	AL,error_path_not_found
UnlinkE:
	transfer    Sys_Ret_Err 	; bye

EndProc $UnLink

BREAK <$RENAME - move directory entries around>
;
;   Assembler usage:
;	    LDS     DX, source
;	    LES     DI, dest
;	    IF VIA SERVER DOS CALL
;	     MOV     CX,SEARCH_ATTRIB
;	    MOV     AH, Rename
;	    INT     21h
;
;   Error returns:
;	    AX = error_file_not_found
;	       = error_not_same_device
;	       = error_access_denied

procedure $RENAME,NEAR
	ASSUME	CS:DOSCODE,SS:DOSDATA
	SAVE	<CX,DS,DX>		; save source and possible CX arg
	PUSH	ES
	POP	DS			; move dest to source
	MOV	SI,DI			; save for offsets
	MOV	DI,OFFSET DOSDATA:RenBuf


	invoke	TransPathSet		; munge the paths
	PUSH	WFP_Start		; get pointer		;smr;SS Override
	POP	Ren_WFP 		; stash it		;smr;SS Override
	RESTORE <SI,DS,CX>		; get back source and possible CX arg
epjc2:	JC	ChModErr		; get old error
	CMP	cMeta,-1					;smr;SS Override
	JNZ	NotFound
	SAVE	<CX>			; Save possible CX arg
	MOV	DI,OFFSET DOSDATA:OpenBuf  ; appropriate buffer
	invoke	TransPathSet		; wham
	RESTORE <CX>
	JC	EPJC2
	Context DS
	CMP	cMeta,-1
	JB	NotFound

	PUSH	WORD PTR [THISCDS]	   ;AN000;;MS.save thiscds
	PUSH	WORD PTR [THISCDS+2]	   ;AN000;;MS.
	MOV	DI,OFFSET DOSDATA:OpenBuf ;AN000;;MS.
	PUSH	SS			   ;AN000;;MS.
	POP	ES			   ;AN000;;MS.es:di-> source
	XOR	AL,AL			   ;AN000;;MS.scan all CDS
rnloop: 				   ;AN000;
	invoke	GetCDSFromDrv		   ;AN000;;MS.
	JC	dorn			   ;AN000;;MS.	end of CDS
	invoke	StrCmp			   ;AN000;;MS.	current dir ?
	JZ	rnerr			   ;AN000;;MS.	yes
	INC	AL			   ;AN000;;MS.	next
	JMP	rnloop			   ;AN000;;MS.
rnerr:					   ;AN000;
	ADD	SP,4			   ;AN000;;MS. pop thiscds
	error	error_current_directory    ;AN000;;MS.
dorn:					   ;AN000;
	POP	WORD PTR SS:[THISCDS+2]    ;AN000;;MS.;PBUGBUG;SS REQD??
	POP	WORD PTR SS:[THISCDS]	   ;AN000;;MS.;PBUGBUG;SS REQD??
	Context DS
	mov	ch,attr_directory+attr_hidden+attr_system; rename appropriate files
	call	SetAttrib
	invoke	DOS_Rename		; do the deed
	JC	UnlinkE 		; errors


	transfer    Sys_Ret_OK
EndProc $Rename

Break <$CreateNewFile - Create a new directory entry>

;
;   CreateNew - Create a new directory entry.  Return a file handle if there
;	was no previous directory entry, and fail if a directory entry with
;	the same name existed previously.
;
;   Inputs:	DS:DX point to an ASCIZ file name
;		CX contains default file attributes
;   Outputs:	Carry Clear:
;		    AX has file handle opened for read/write
;		Carry Set:
;		    AX has error code
;   Registers modified: All

Procedure $CreateNewFile,NEAR
	ASSUME	CS:DOSCODE,SS:DOSDATA
	fmt TypSysCall,LevArgs,<" Att = $x file = '$S'\n">,<CX,DS,DX>
IFDEF DBCS 				;AN000;
	MOV	Temp_Var,CX		;AN000;KK. set variable with attribute;smr;SS Override
ENDIF					;AN000;
	SAVE	<CX>			; Save attributes on stack
	MOV	CX,OFFSET DOSCODE:DOS_Create_New   ; routine to call
	JMP	AccessSet		; use good ol' open

EndProc $CreateNewFile, NoCheck

	Break	<BinToAscii - convert a number to a string>

;**	BinToAscii - conver a number to a string.
;
;	BinToAscii converts a 16 bit number into a 4 ascii characters.
;	This routine is used to generate temp file names so we don't spend
;	the time and code needed for a true hex number, we just use
;	A thorugh O.
;
;	ENTRY	(ax) = value
;		(es:di) = destination
;	EXIT	(es:di) updated by 4
;	USES	cx, di, flags

Procedure   BinToAscii,NEAR
	mov	cx,404h			; (ch) = digit counter, (cl) = shift cnt
bta5:	ROL	AX,CL			; move leftmost nibble into rightmost
	SAVE	<AX>			; preserve remainder of digits
	AND	AL,0Fh			; grab low nibble
	ADD	AL,'A'			; turn into ascii
	STOSB				; drop in the character
	RESTORE <AX>			; (ax) = shifted number
	dec	ch
	jnz	bta5			; process 4 digits
	return

EndProc BinToAscii

Break	<$CreateTempFile - create a unique name>

;
;   $CreateTemp - given a directory, create a unique name in that directory.
;	Method used is to get the current time, convert to a name and attempt
;	a create new.  Repeat until create new succeeds.
;
;   Inputs:	DS:DX point to a null terminated directory name.
;		CX  contains default attributes
;   Outputs:	Unique name is appended to DS:DX directory.
;		AX has handle
;   Registers modified: all

Procedure $CreateTempFile,NEAR
	ASSUME	CS:DOSCODE,SS:DOSDATA

	fmt TypSysCall,LevArgs,<" Att = $x dir = '$S'\n">,<CX,DS,DX>
PUBLIC FILE001S,FILE001E
FILE001S:
	LocalVar    EndPtr,DWORD
	LocalVar    FilPtr,DWORD
	LocalVar    Attr,WORD
FILE001E:
	Enter
	TEST	CX,NOT attr_changeable
	JZ	OKatts			; Ok if no non-changeable bits set
;
; We need this "hook" here to detect these cases (like user sets one both of
; vol_id and dir bits) because of the structure of the or $CreateNewFile loop
; below.  The code loops on error_access_denied, but if one of the non
; changeable attributes is specified, the loop COULD be infinite or WILL be
; infinite because CreateNewFile will fail with access_denied always.  Thus we
; need to detect these cases before getting to the loop.
;
	MOV	AX,error_access_denied
	JMP	SHORT SETTMPERR

OKatts:
	MOV	attr,CX 		; save attribute
	MOV	FilPtrL,DX		; pointer to file
	MOV	FilPtrH,DS
	MOV	EndPtrH,DS		; seg pointer to end of dir
	PUSH	DS
	POP	ES			; destination for nul search
	MOV	DI,DX
	MOV	CX,DI
	NEG	CX			; number of bytes remaining in segment
 IFDEF  DBCS				;AN000;
Kloop:					;AN000;; 2/13/KK
	MOV	AL, BYTE PTR ES:[DI]	;AN000;; 2/13/KK
	INC	DI			;AN000;; 2/13/KK
	OR	AL,AL			;AN000;; 2/13/KK
	JZ	GOTEND			;AN000;; 2/13/KK
	invoke	testkanj		;AN000;; 2/13/KK
	jz	Kloop			;AN000;; 2/13/KK
	inc	di			;AN000;; Skip over second kanji byte 2/13/KK
	CMP	BYTE PTR ES:[DI],0	;AN000;; 2/13/KK
	JZ	STOREPTH		;AN000; When char before NUL is sec Kanji byte
					;AN000; do not look for path char. 2/13/KK
	jmp	Kloop			;AN000; 2/13/KK
GOTEND: 				;AN000; 2/13/KK
 ELSE					;AN000;
	OR	CX,CX			;AN000;MS.  cx=0 ? ds:dx on segment boundary
	JNZ	okok			;AN000;MS.  no
	MOV	CX,-1			;AN000;MS.
okok:					;AN000;
	XOR	AX,AX			;AN000;
	REPNZ	SCASB			;AN000;
 ENDIF					;AN000;
	DEC	DI			; point back to the null
	MOV	AL,ES:[DI-1]		; Get char before the NUL
	invoke	PathChrCmp		; Is it a path separator?
	JZ	SETENDPTR		; Yes
STOREPTH:
	MOV	AL,'\'
	STOSB				; Add a path separator (and INC DI)
SETENDPTR:
	MOV	EndPtrL,DI		; pointer to the tail
CreateLoop:
	Context DS			; let ReadTime see variables
	SAVE	<BP>
	invoke	ReadTime		; go get time
	RESTORE <BP>
;
; Time is in CX:DX.  Go drop it into the string.
;
	les	di,EndPtr		; point to the string
	mov	ax,cx
	call	BinToAscii		; store upper word
	mov	ax,dx
	call	BinToAscii		; store lower word
	xor	al,al
	STOSB				; nul terminate
	LDS	DX,FilPtr		; get name
ASSUME	DS:NOTHING
	MOV	CX,Attr 		; get attr
	SAVE	<BP>
	CALL	$CreateNewFile		; try to create a new file
	RESTORE <BP>
	JNC	CreateDone		; failed, go try again
;
; The operation failed and the error has been mapped in AX.  Grab the extended
; error and figure out what to do.
;
					; M049 - start
;;	mov	ax,ExtErr					;smr;SS Override
;;	cmp	al,error_file_exists
;;	jz	CreateLoop		; file existed => try with new name
;;	cmp	al,error_access_denied
;;	jz	CreateLoop		; access denied (attr mismatch)

	CMP	AL,error_file_exists	; Q: did file already exist
	JZ	CreateLoop		; Y: try again
	CMP	AL,error_access_denied	; Q: was it access denied
	JNZ	SETTMPERR		; N: Error out
					; Y: Check to see if we got this due
					;    to the network drive. Note that
					;    the redir will set the exterr
					;    to error_cannot_make if this is 
					;    so. 
	CMP	[EXTERR],error_net_access_denied ; M069
					; See if it's REALLY an att mismatch
	je	SetTmpErr		; no, network error, stop
;M070
; If the user failed on an I24, we do not want to try again
;
	cmp	ExtErr,error_FAIL_I24		;User failed on I24? ;M070
	je	SetTmpErr		;yes, do not try again ;M070

	jmp	short CreateLoop		;attr mismatch, try again ;M070

;;	MOV	AL,error_access_denied	; Return this "extended" error

					; M049 - end

SETTMPERR:
	STC
CreateDone:
	Leave
	JC	CreateFail
	transfer    Sys_Ret_OK		; success!
CreateFail:
	transfer    Sys_Ret_Err
EndProc $CreateTempFile

Break	<SetAttrib - set the search attrib>

;
;   SetAttrib will set the search attribute (SAttrib) either to the normal
;   (CH) or to the value in CL if the current system call is through
;   serverdoscall.
;
;   Inputs:	fSharing == FALSE => set sattrib to CH
;		fSharing == TRUE => set sattrib to CL
;   Outputs:	none
;   Registers changed:	CX

procedure   SetAttrib,NEAR
	assume	SS:DOSDATA			;smr;
	test	fSharing,-1					;smr;SS Override
	jnz	Set
	mov	cl,ch
Set:
	mov	SAttrib,cl					;smr;SS Override
	return
EndProc SetAttrib


Break	<Extended_Open- Extended open the file>

; Input: AL= 0 reserved  AH=6CH
;	 BX= mode
;	 CL= create attribute  CH=search attribute (from server)
;	 DX= flag
;	 DS:SI = file name
;	 ES:DI = parm list
;			   DD  SET EA list (-1) null
;			   DW  n  parameters
;			   DB  type (TTTTTTLL)
;			   DW  IOMODE
; Function: Extended Open
; Output: carry clear
;		     AX= handle
;		     CX=1 file opened
;			2 file created/opened
;			3 file replaced/opened
;	  carry set: AX has error code
;


procedure   $Extended_Open,NEAR 			       ;AN000;
	ASSUME	CS:DOSCODE,SS:DOSDATA	;AN000;

	MOV	[EXTOPEN_FLAG],DX	  ;AN000;EO. save ext. open flag;smr;SS Override
	MOV	[EXTOPEN_IO_MODE],0	  ;AN000;EO. initialize IO mode;smr;SS Override
	TEST	DX,reserved_bits_mask	  ;AN000;EO. reserved bits 0  ?
	JNZ	ext_inval2		  ;AN000;EO. no
	MOV	AH,DL			  ;AN000;EO. make sure flag is right
	CMP	DL,0			  ;AN000;EO. all fail ?
	JZ	ext_inval2		  ;AN000;EO. yes, error
	AND	DL,exists_mask		  ;AN000;EO. get exists action byte
	CMP	DL,2			  ;AN000;EO, > 02
	JA	ext_inval2		  ;AN000;EO. yes ,error
	AND	AH,not_exists_mask	  ;AN000;EO. get no exists action byte
	CMP	AH,10H			  ;AN000;EO. > 10
	JA	ext_inval2		  ;AN000;EO. yes error

;	CMP	DI,-1			  ;AN000;EO. null parm list
;	JZ	no_parm 		  ;AN000;EO. yes
;					  ;AN000;EO
;	PUSH	CX			  ;AN000;EO.
;					  ;AN000;EO.
;	MOV	CX,ES:[DI.EXT_NUM_OF_PARM];AN000;EO. get number of parms
;	OR	CX,CX			  ;AN000;EO. 0 pamrs ?
;	JZ	parmend 		  ;AN000;EO. yes
;	PUSH	SI			  ;AN000;EO.
;	PUSH	DS			  ;AN000;EO.
;	MOV	SI,DI			  ;AN000;EO.
;	ADD	SI,size EXT_OPEN_PARM	  ;AN000;EO. position to 1st parm
;	PUSH	ES			  ;AN000;EO.
;	POP	DS			  ;AN000;EO. ds:si -> parm list
;	CALL	GetIOParms		  ;AN000;EO.
;	POP	DS			  ;AN000;EO.
;	POP	SI			  ;AN000;EO.
;parmend:				  ;AN000;EO
;	POP	CX			  ;AN000;EO. restore CX
;no_parm:				  ;AN000;EO.
	MOV	[SAVE_ES],ES		  ;AN000;EO. save API parms;smr;SS Override
	MOV	[SAVE_DI],DI		  ;AN000;EO.;smr;SS Override
	PUSH	[EXTOPEN_FLAG]		  ;AN000;EO.;smr;SS Override
	POP	[SAVE_DX]		  ;AN000;EO.;smr;SS Override
	MOV	[SAVE_CX],CX		  ;AN000;EO.;smr;SS Override
	MOV	[SAVE_BX],BX		  ;AN000;EO.;smr;SS Override
	MOV	[SAVE_DS],DS		  ;AN000;EO.;smr;SS Override
	MOV	[SAVE_SI],SI		  ;AN000;EO.;smr;SS Override
	MOV	DX,SI			  ;AN000;EO. ds:dx points to file name
	MOV	AX,BX			  ;AN000;EO. ax= mode

;	TEST	[EXTOPEN_FLAG],no_code_page_check    ;AN000;EO. check no  code page
;	JNZ	no_cdpg_chk			 ;AN000;;EO.  no
	JMP	SHORT goopen2			 ;AN000;;EO.  do nromal
ext_inval2:					 ;AN000;;EO.
	error	error_Invalid_Function		 ;AN000;EO..  invalid function
ext_inval_parm: 				 ;AN000;EO..
	POP	CX				 ;AN000;EO..  pop up satck
	POP	SI				 ;AN000;EO..
	error	error_Invalid_data		 ;AN000;EO..  invalid parms
error_return:					 ;AN000;EO.
	ret					 ;AN000;EO..  return with error
;no_cdpg_chk:						EO.
;	MOV	[CPSWFLAG],0			 ;AN000;EO..  set CPSW flag off
goopen2:					 ;AN000;
	TEST	BX,int_24_error 		 ;AN000;EO..  disable INT 24 error ?
	JZ	goopen				 ;AN000;EO..  no
	OR	[EXTOPEN_ON],EXT_OPEN_I24_OFF	 ;AN000;EO..  set bit to disable;smr;SS Override

goopen: 					 ;AN000;
	OR	[EXTOPEN_ON],EXT_OPEN_ON	 ;AN000;EO..  set Extended Open active;smr;SS Override
	AND	[EXTOPEN_FLAG],0FFH		 ;AN000;EO.create new ?;smr;SS Override
	CMP	[EXTOPEN_FLAG],ext_exists_fail + ext_nexists_create ;AN000;FT.;smr;SS Override
	JNZ	chknext 			 ;AN000;;EO.  no
	invoke	$CreateNewFile			 ;AN000;;EO.  yes
	JC	error_return			 ;AN000;;EO.  error
	CMP	[EXTOPEN_ON],0			 ;AN000;;EO.  IFS does it;smr;SS Override
	JZ	ok_return2			 ;AN000;;EO.  yes
	MOV	[EXTOPEN_FLAG],action_created_opened ;AN000;EO. creted/opened;smr;SS Override
	JMP	setXAttr			 ;AN000;;EO.  set XAs
ok_return2:
	transfer SYS_RET_OK			 ;AN000;;EO.
chknext:
	TEST	[EXTOPEN_FLAG],ext_exists_open	 ;AN000;;EO.  exists open;smr;SS Override
	JNZ	exist_open			 ;AN000;;EO.  yes
	invoke	$Creat				 ;AN000;;EO.  must be replace open
	JC	error_return			 ;AN000;;EO.  return with error
	CMP	[EXTOPEN_ON],0			 ;AN000;;EO.  IFS does it;smr;SS Override
	JZ	ok_return2			 ;AN000;;EO.  yes
	MOV	[EXTOPEN_FLAG],action_created_opened  ;AN000;EO. prsume create/open;smr;SS Override
	TEST	[EXTOPEN_ON],ext_file_not_exists      ;AN000;;EO. file not exists ?;smr;SS Override
	JNZ	setXAttr			      ;AN000;;EO. no
	MOV	[EXTOPEN_FLAG],action_replaced_opened ;AN000;;EO. replaced/opened;smr;SS Override
	JMP	SHORT setXAttr			      ;AN000;;EO. set XAs
error_return2:
	STC                         ; Set Carry again to flag error     ;AN001;
	ret					 ;AN000;;EO.  return with error
						 ;AN000;
exist_open:					 ;AN000;
	test	fSharing,-1			 ;AN000;;EO. server doscall?;smr;SS Override
	jz	noserver			 ;AN000;;EO. no
	MOV	CL,CH				 ;AN000;;EO. cl=search attribute

noserver:
	call	$Open2				 ;AN000;;EO.  do open
	JNC	ext_ok				 ;AN000;;EO.
	CMP	[EXTOPEN_ON],0			 ;AN000;;EO.  error and IFS call;smr;SS Override
	JZ	error_return2			 ;AN000;;EO.  return with error
local_extopen:

	CMP	AX,error_file_not_found 	 ;AN000;;EO.  file not found error
	JNZ	error_return2			 ;AN000;;EO.  no,
	TEST	[EXTOPEN_FLAG],ext_nexists_create;AN000;;EO.  want to fail;smr;SS Override
	JNZ	do_creat			 ;AN000;;EO.  yes
	JMP	short extexit 			 ;AN000;;EO.  yes
do_creat:
	MOV	CX,[SAVE_CX]			 ;AN000;;EO.  get ds:dx for file name;smr;SS Override
	LDS	SI,DWORD PTR [SAVE_SI]		 ;AN000;;EO.  cx = attribute;smr;SS Override
	MOV	DX,SI				 ;AN000;;EO.
	invoke	$Creat				 ;AN000;;EO.  do create
	JC	extexit 			 ;AN000;;EO.  error
	MOV	[EXTOPEN_FLAG],action_created_opened  ;AN000;;EO. is created/opened;smr;SS Override
	JMP	SHORT setXAttr			      ;AN000;;EO.   set XAs

ext_ok:
	CMP	[EXTOPEN_ON],0			 ;AN000;;EO.  IFS call ?;smr;SS Override
	JZ	ok_return			 ;AN000;;EO.  yes
	MOV	[EXTOPEN_FLAG],action_opened	 ;AN000;;EO.  opened;smr;SS Override
setXAttr:
;	LES	DI,DWORD PTR [SAVE_DI]	;AN000;EO.
	PUSH	AX			;AN000;;EO. save handle for final
;	MOV	BX,AX			;AN000;;EO. bx= handle
;	MOV	AX,04H			;AN000;;EO. set extended attr by handle
;	PUSH	DS			;AN000;;EO. save file name addr
;	PUSH	DX			;AN000;;EO.
;	CMP	DI,-1			;AN000;;EO. null parm list
;	JZ	nosetea 		;AN000;;EO. yes
;	CMP	WORD PTR ES:[DI],-1	;AN000;;EO. null set list
;	JZ	nosetea 		;AN000;;EO. yes
;	LES	DI,DWORD PTR ES:[DI]	;AN000;;EO. es:di -> set list
;	invoke	$File_times		;AN000;;EO.
;nosetea:				;AN000; EO
;	POP	DX			;AN000;;EO. restore file name addr
;	POP	DS			;AN000;;EO.
;	JC	extexit2		;AN000;;EO.
	invoke	get_user_stack		;AN000;;EO.
	MOV	AX,[EXTOPEN_FLAG]	;AN000;;EO.;smr;SS Override
	MOV	[SI.USER_CX],AX 	;AN000;;EO. set action code for cx
	POP	AX			;AN000;;EO.
	MOV	[SI.USER_AX],AX 	;AN000;;EO. set handle for ax

ok_return:				;AN000;
	transfer SYS_RET_OK		;AN000;;EO.

extexit2:				;AN000; ERROR RECOVERY

	POP	BX			;AN000;EO. close the handle
	PUSH	AX			;AN000;EO. save error code from set XA
	CMP	[EXTOPEN_FLAG],action_created_opened	;AN000;EO. from create;smr;SS Override
	JNZ	justopen		;AN000;EO.
	LDS	SI,DWORD PTR [SAVE_SI]		 ;AN000;EO.  cx = attribute;smr;SS Override
	LDS	DX,DWORD PTR [SI]		 ;AN000;EO.
	invoke	$UNLINK 		;AN000;EO. delete the file
	JMP	SHORT reserror		;AN000;EO.

justopen:				;AN000;
	invoke	$close			;AN000;EO. pretend never happend
reserror:				;AN000;
	POP	AX			;AN000;EO. retore error code from set XA
	JMP	SHORT extexit		;AN000;EO.


ext_file_unfound:			;AN000;
	MOV	AX,error_file_not_found ;AN000;EO.
	JMP	SHORT extexit		;AN000;EO.
ext_inval:				 ;AN000;
	MOV	AX,error_invalid_function;AN000;EO.
extexit:
	transfer SYS_RET_ERR		;AN000;EO.

EndProc $Extended_Open			;AN000;


Break	<GetIOParms - get IO parms form extended open parm list>

;
;
;   Inputs: DS:SI -> IO parm list
;	    CX= number of parms
;   Function: get IO parms from parm list
;   Outputs:  [EXT_IOMODE]= IO mode parm

;procedure   GetIOParms,NEAR
;
;	LODSB					; get parm type 		;AN000;
;	CMP	AL,0*100B+10B			; have IOMODE			;AN000;
;	JE	SET_IOMODE							;AN000;
;	AND	AL,00000011B			; decode it			;AN000;
;	JZ	SKIP_ASCIIZ							;AN000;
;	DEC	AL								;AN000;
;	JZ	SKIP_LEN							;AN000;
;;	DEC	AL								;AN000;
;	JZ	SKIP_WORD							;AN000;
;SKIP_DWORD:					 ; copy DWORD parm		 ;AN000;
;	LODSW									;AN000;
;SKIP_WORD:					 ; copy WORD parm		 ;AN000;
;	LODSW									;AN000;
;	JMP	SHORT NEXT_PARM 						;AN000;
;SET_IOMODE:					 ; copy IOMODE			 ;AN000;
;	LODSW									;AN000;
;	MOV	[EXTOPEN_IO_MODE],AX						;AN000;
;	JMP	SHORT NEXT_PARM 						;AN000;
;SKIP_LEN:					 ; copy LENGTH parm		 ;AN000;
;	LODSW									;AN000;
;	ADD	SI,AX								;AN000;
;	JMP	SHORT NEXT_PARM 						;AN000;
;SKIP_ASCIIZ:					 ; copy ASCIIZ parm		 ;AN000;
;	LODSB									;AN000;
;	OR	AL,AL								;AN000;
;	JNE	SKIP_ASCIIZ							;AN000;
;NEXT_PARM:									 ;AN000;
;	LOOP	GetIOParms							;AN000;
;	return									;AN000;
;EndProc GetIOParms								 ;AN000;


DOSCODE ENDS
	END


