	TITLE	DOS_RENAME - Internal RENAME call for MS-DOS
	NAME	DOS_RENAME
;
;	Microsoft Confidential
;	Copyright (C) Microsoft Corporation 1991
;	All Rights Reserved.
;

;**	Low level routine for renaming files
;
;	DOS_RENAME
;
;	Modification history:
;
;	    Created: ARR 30 March 1983

	.xlist
	.xcref
	include version.inc
	include dosseg.inc
	INCLUDE DOSSYM.INC
	INCLUDE DEVSYM.INC
	include sf.inc
	include find.inc
	include filemode.inc
	include mult.inc
	.cref
	.list

Installed = TRUE

	i_need	RENAMEDMA,BYTE
	i_need	AUXSTACK,BYTE
	i_need	DESTSTART,WORD
	i_need	DIRSTART,WORD
	i_need	CURBUF,DWORD
	I_need	NAME1,BYTE
	i_need	NAME2,BYTE
	i_need	WFP_START,WORD
	i_need	REN_WFP,WORD
	i_need	CURR_DIR_END,WORD
	i_need	DMAADD,DWORD
	i_need	THISCDS,DWORD
	i_need	THISDPB,DWORD
	i_need	THISSFT,DWORD
	i_need	CREATING,BYTE
	i_need	THISDRV,BYTE
	i_need	ATTRIB,BYTE
	i_need	FOUND_DEV,BYTE
	i_need	FAILERR,BYTE
	i_need	EXTERR_LOCUS,BYTE
	i_need	SAVE_BX,WORD


DOSCODE	SEGMENT
	ASSUME	SS:DOSDATA,CS:DOSCODE
;----------------------------------------------------------------------------
;
; Procedure Name : DOS_RENAME
;
; Inputs:
;	[WFP_START] Points to SOURCE WFP string ("d:/" must be first 3
;		chars, NUL terminated)
;	[CURR_DIR_END] Points to end of Current dir part of string [SOURCE]
;		( = -1 if current dir not involved, else
;		 Points to first char after last "/" of current dir part)
;	[REN_WFP] Points to DEST WFP string ("d:/" must be first 3
;		chars, NUL terminated)
;	[THISCDS] Points to CDS being used
;		(Low word = -1 if NUL CDS (Net direct request))
;	[SATTRIB] Is attribute of search, determines what files can be found
; Function:
;	Rename the specified file(s)
;	NOTE: This routine uses most of AUXSTACK as a temp buffer.
; Outputs:
;	CARRY CLEAR
;	    OK
;	CARRY SET
;	    AX is error code
;		error_file_not_found
;			No match for source, or dest path invalid
;		error_not_same_device
;			Source and dest are on different devices
;		error_access_denied
;			Directory specified (not simple rename),
;			Device name given, Destination exists.
;			NOTE: In third case some renames may have
;			 been done if metas.
;		error_path_not_found
;			Bad path (not in curr dir part if present)
;			SOURCE ONLY
;		error_bad_curr_dir
;			Bad path in current directory part of path
;			SOURCE ONLY
;		error_sharing_violation
;			Deny both access required, generates an INT 24.
; DS preserved, others destroyed
;
;----------------------------------------------------------------------------

	procedure   DOS_RENAME,NEAR

;hkn; DOS_RENAME is called from file.asm and fcbio.asm. DS has been set up
;hkn; at this point to DOSDATA.

	DOSAssume   <DS>,"DOS_Rename"
	ASSUME	ES:NOTHING

	Invoke	TestNet
	JNC	LOCAL_RENAME

IF NOT Installed
	transfer NET_RENAME
ELSE
	MOV	AX,(multNET SHL 8) OR 17
	INT	2FH
	return
ENDIF

LOCAL_RENAME:
	MOV	[EXTERR_LOCUS],errLOC_Disk
	MOV	SI,[WFP_START]
	MOV	DI,[REN_WFP]
	MOV	AL,BYTE PTR [SI]
	MOV	AH,BYTE PTR [DI]
	OR	AX,2020H		; Lower case
	CMP	AL,AH
	JZ	SAMEDRV
	MOV	AX,error_not_same_device
	STC
	return

SAMEDRV:
	PUSH	WORD PTR [DMAADD+2]
	PUSH	WORD PTR [DMAADD]
	MOV	WORD PTR [DMAADD+2],DS

;hkn; RENAMEDMA is in DOSDATA
	MOV	WORD PTR [DMAADD],OFFSET DOSDATA:RENAMEDMA
	MOV	[Found_dev],0		; Rename fails on DEVS, assume not a dev
	EnterCrit   critDisk
	invoke	DOS_SEARCH_FIRST	; Sets [NoSetDir] to 1, [CURBUF+2]:BX
					;    points to entry
	JNC	Check_Dev
	CMP	AX,error_no_more_files
	JNZ	GOTERR
	MOV	AX,error_file_not_found
GOTERR:
	STC
RENAME_POP:
	POP	WORD PTR [DMAADD]
	POP	WORD PTR [DMAADD+2]
	LeaveCrit   critDisk
	return

Check_dev:
	MOV	AX,error_access_denied	; Assume error

	PUSH	DS			      ;PTM.				;AN000;
	LDS	SI,[DMAADD]		      ;PTM.  chek if source a dir	;AN000;
	ADD	SI,find_buf_attr	      ;PTM.				;AN000;
	TEST	[SI.dir_attr],attr_directory  ;PTM.				;AN000;
	JZ	notdir			      ;PTM.				;AN000;
	MOV	SI,[REN_WFP]		      ;PTM.  if yes, make sure path	;AN000;
	invoke	Check_Pathlen2		      ;PTM.   length < 67		;AN000;
notdir:
	POP	DS			      ;PTM.				;AN000;
	JA	GOTERR			      ;PTM.				;AN000;

	CMP	[Found_dev],0
	JNZ	GOTERR
; At this point a source has been found.  There is search continuation info (a
; la DOS_SEARCH_NEXT) for the source at RENAMEDMA, together with the first
; directory entry found.
; [THISCDS], [THISDPB], and [THISDRV] are set and will remain correct
; throughout the RENAME since it is known at this point that the source and
; destination are both on the same device.
; [SATTRIB] is also set.
	MOV	SI,BX
	ADD	SI,dir_first
	invoke	REN_DEL_Check
	JNC	REN_OK1
	MOV	AX,error_sharing_violation
	JMP	RENAME_POP

;------------------------------------------------------------------------------
; Check if the source is a file or directory.  If file, delete the entry
; from the Fastopen cache. If directory, rename it later
;------------------------------------------------------------------------------
REN_OK1:				;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	PUSH	SI
	LDS	SI,[DMAADD]		      ;BN00X; PTM. chek if source a dir       ;AN000;
	ADD	SI,find_buf_attr	      ;;BN00XPTM.P5520				   ;AN000;
	TEST	[SI.dir_attr],attr_directory  ;;BN00XPTM.			      ;AN000;
	JZ	Not_dir1		      ;;BN00XPTM.			      ;AN000;
	POP	SI			      ;BN00X
	JMP	SHORT  SWAP_SOURCE	      ;BN00X

NOT_DIR1:				;;BN00X it is a file, delete the entry
	POP	SI
	invoke	FastOpen_Delete 	; delete dir info in fastopen DOS 3.3

SWAP_SOURCE:
	MOV	AX,[WFP_START]		; Swap source and destination
	MOV	SI,[REN_WFP]		; Swap source and destination
	MOV	[WFP_START],SI		; WFP_START = Destination path
	MOV	[REN_WFP],AX		; REN_WFP   = Source path
	MOV	[CURR_DIR_END],-1	; No current dir on dest
	MOV	WORD PTR [CREATING],DIRFREE*256+0ffh  ; Creating, not DEL *.*
					; A rename is like a CREATE_NEW as far
					; as the destination is concerned.
	invoke	GetPathNoSet
;   If this Getpath fails due to file not found, we know all renames will work
;   since no files match the destination name.	If it fails for any other
;   reason, the rename fails on a path not found, or whatever (also fails if
;   we find a device or directory).  If the Getpath succeeds, we aren't sure
;   if the rename should fail because we haven't built an explicit name by
;   substituting for the meta chars in it.  In this case the destination file
;   spec with metas is in [NAME1] and the explicit source name is at RENAMEDMA
;   in the directory entry part.
	JC	NODEST
;;	JZ	BAD_ACC 		; Dest string is a directory		;AC000;
	OR	AH,AH			; Device?
	JNS	SAVEDEST		; No, continue
BAD_ACC:
	MOV	AX,error_access_denied
	STC
RENAME_CLEAN:
	PUSHF				; Save carry state
	PUSH	AX			; and error code (if carry set)
	MOV	AL,[THISDRV]
	invoke	FLUSHBUF
	POP	AX
	CMP	[FAILERR],0
	JNZ	BAD_ERR 		; User FAILed to I 24
	POPF
	JMP	RENAME_POP

BAD_ERR:
	POP	AX			; Saved flags
	MOV	AX,error_path_not_found
	JMP	GOTERR

NODEST:
	JNZ	BAD_PATH
	CMP	[FAILERR],0
	JNZ	BAD_PATH	; Search for dest failed because user FAILed on
				;	I 24
	OR	CL,CL
	JNZ	SAVEDEST
BAD_PATH:
	MOV	AX,error_path_not_found
	STC
	JMP	RENAME_POP

SAVEDEST:
	Context ES

;hkn; NAME1 & NAME2 is in DOSDATA
	MOV	DI,OFFSET DOSDATA:NAME2
	MOV	SI,OFFSET DOSDATA:NAME1

	MOV	CX,11
	REP	MOVSB			; Save dest with metas at NAME2
	MOV	AX,[DIRSTART]
	MOV	[DESTSTART],AX
BUILDDEST:
	Context ES			; needed due to JMP BUILDDEST below

;hkn; RENAMEDMA, NAME1, NAME2 in DOSDATA
	MOV	BX,OFFSET DOSDATA:RENAMEDMA + 21   ; Source of replace chars
	MOV	DI,OFFSET DOSDATA:NAME1    ; Real dest name goes here
	MOV	SI,OFFSET DOSDATA:NAME2    ; Raw dest

ifdef DBCS
	mov	cx,8
	call	new_rename
	mov	cx,3
	call	new_rename
else
	MOV	CX,11
	CALL	NEW_RENAME		    ;IFS. replace ? chars		;AN000;
endif

	MOV	[ATTRIB],attr_all	; Stop duplicates with any attributes
	MOV	[CREATING],0FFH
	invoke	DEVNAME 		; Check if we built a device name
	ASSUME	ES:NOTHING
	JNC	BAD_ACC
	MOV	BX,[DESTSTART]
	LES	BP,[THISDPB]
	invoke	SetDirSrch		; Reset search to start of dir
	JC	BAD_ACC 		; Screw up
	invoke	FINDENTRY		; See if new name already exists
	JNC	BAD_ACC 		; Error if found
	CMP	[FAILERR],0
	JNZ	BAD_ACCJ		; Find failed because user FAILed to I 24
	MOV	AX,[DESTSTART]		; DIRSTART of dest
	CMP	AX,WORD PTR [RENAMEDMA + 15]	; DIRSTART of source
	JZ	SIMPLE_RENAME		; If =, just give new name

	MOV	AL,[RENAMEDMA + 21 + dir_attr]
	TEST	AL,attr_directory
	JNZ	BAD_ACCJ		; Can only do a simple rename on dirs,
					; otherwise the .  and ..  entries get
					; wiped.
	MOV	[ATTRIB],AL
	MOV	WORD PTR [THISSFT+2],DS

;hkn; AUXSTACK is in DOSDATA
	MOV	SI,OFFSET DOSDATA:AUXSTACK - SIZE SF_ENTRY
	MOV	WORD PTR [THISSFT],SI
	MOV	[SI].sf_mode,sharing_compat+open_for_both
	XOR	CX,CX			; Set "device ID" for call into makenode
	invoke	RENAME_MAKE		; This is in mknode
	JNC	GOT_DEST
BAD_ACCJ:
	JMP	BAD_ACC

GOT_DEST:
	SAVE	<BX>
	LES	DI,ThisSFT		; Rename_make entered this into sharing
	Invoke	ShareEnd		; we need to remove it.
	RESTORE <BX>
; A zero length entry with the correct new name has now been made at
;   [CURBUF+2]:BX.
	LES	DI,[CURBUF]
	Assert	ISBUF,<ES,DI>,"Got_Dest"

	TEST	ES:[DI.buf_flags],buf_dirty  ;LB. if already dirty		;AN000;
	JNZ	yesdirty		  ;LB.	  don't increment dirty count   ;AN000;
	invoke	INC_DIRTY_COUNT 	  ;LB.					;AN000;
	OR	ES:[DI.buf_flags],buf_dirty
yesdirty:
	MOV	DI,BX
	ADD	DI,dir_attr		; Skip name

;hkn; RENAMEDMA is in DOSDATA
	MOV	SI,OFFSET DOSDATA:RENAMEDMA + 21 + dir_attr
	MOV	CX,(SIZE dir_entry) - dir_attr
	REP	MOVSB
	CALL	GET_SOURCE
	DLJC	RENAME_OVER
	MOV	DI,BX
	MOV	ES,WORD PTR [CURBUF+2]
	MOV	AL,DIRFREE
	STOSB				; "free" the source
	JMP	SHORT DIRTY_IT

SIMPLE_RENAME:
	CALL	GET_SOURCE		; Get the source back
	JC	RENAME_OVER
	MOV	DI,BX
	MOV	ES,WORD PTR [CURBUF+2]

;hkn; NAME1 is in DOSDATA
	MOV	SI,OFFSET DOSDATA:NAME1    ; New Name
	MOV	CX,11
	REP	MOVSB
DIRTY_IT:
	MOV	DI,WORD PTR [CURBUF]

	TEST	ES:[DI.buf_flags],buf_dirty  ;LB. if already dirty		;AN000;
	JNZ	yesdirty2		  ;LB.	  don't increment dirty count   ;AN000;
	invoke	INC_DIRTY_COUNT 	  ;LB.					;AN000;
	OR	ES:[DI.buf_flags],buf_dirty

;------------------------------------------------------------------------------
; Check if the source is a directory of file.  If directory rename it to the
; the new name in the Fastopen cache buffer.   If file name it has been
; previously deleted.
;------------------------------------------------------------------------------
Yesdirty2:
	PUSH	SI
	LDS	SI,[DMAADD]		      ;;BN00XPTM. chek if source a dir	     ;AN000;
	ADD	SI,find_buf_attr	      ;;BN00XPTM.P5520				   ;AN000;
	TEST	[SI.dir_attr],attr_directory  ;;BN00XPTM.			      ;AN000;
	JZ	Not_dir2		      ;;BN00XPTM.			      ;AN000;
	INVOKE	FASTOPEN_RENAME 	      ;;BN00X rename dir entry in fastopen
	POP	SI
	JMP	SHORT NOT_DIRTY1

NOT_DIR2:				      ;;BN00X it is a file, delete the entry
	POP	SI

NOT_DIRTY1:				      ;;BN00X

	Assert	ISBUF,<ES,DI>,"Dirty_it"
NEXT_SOURCE:

;hkn; RENAMEDMA is in DOSDATA
	MOV	SI,OFFSET DOSDATA:RENAMEDMA + 1    ;Name
;
; WARNING!  Rename_Next leaves the disk critical section *ALWAYS*.  We need
; to enter it before going to RENAME_Next.
;
	EnterCrit   critDisk
	MOV	[CREATING],0	; Correct setting for search (we changed it
				;   to FF when we made the prev new file).
	invoke	RENAME_NEXT
;
; Note, now, that we have exited the previous ENTER and so are back to where
; we were before.
;
	JC	RENAME_OVER
	LEA	SI,[BX].dir_First
	invoke	REN_DEL_Check
	JNC	REN_OK2
	MOV	AX,error_sharing_violation
	JMP	RENAME_CLEAN

;------------------------------------------------------------------------------
; Check if file or directory. If file, delete file from the Fastopen cache,
; if directory, rename directory name in the Fastopen cache.
;-----------------------------------------------------------------------------
REN_OK2:
	MOV	AL,[RENAMEDMA + 21 + dir_attr]	   ; PTR P5622
	TEST	AL,attr_directory	;;BN00X directory
	JZ	Ren_Directory		;;BN00X no - file, delete it
	INVOKE	FASTOPEN_DELETE 	;;BN00X delete dir info in fastopen DOS 3.3
	JMP	BUILDDEST		;;BN00X

Ren_Directory:
	INVOKE	FASTOPEN_RENAME 	;;BN00X delete dir info in fastopen DOS 3.3
	JMP	BUILDDEST

RENAME_OVER:
	CLC
	JMP	RENAME_CLEAN
;----------------------------------------------------------------------------
;
; Procedure: GET_SOURCE
;
; Inputs:
;	RENAMEDMA has source info
; Function:
;	Re-find the source
; Output:
;	[CURBUF] set
;	[CURBUF+2]:BX points to entry
;	Carry set if error (currently user FAILed to I 24)
; DS preserved, others destroyed
;----------------------------------------------------------------------------

GET_SOURCE:
	DOSAssume   <DS>,"Get_Source"
	ASSUME	ES:NOTHING

	MOV	BX,WORD PTR [RENAMEDMA + 15]	; DirStart
	LES	BP,ThisDPB
	invoke	SetDirSrch
	JC	ret_label			; retc
	invoke	StartSrch
	MOV	AX,WORD PTR [RENAMEDMA + 13]	; Lastent
	invoke	GetEnt

ret_label:
	return

EndProc DOS_RENAME

;----------------------------------------------------------------------------
;
;Procedure: NEW_RENAME
;
;Input: DS:SI -> raw string with ?
;	ES:DI -> destination string
;	DS:BX -> source string
;Function: replace ? chars of raw string with chars in source string and
;	   put in destination string
;Output: ES:DI-> new string
;---------------------------------------------------------------------------

	ASSUME	ES:NOTHING
procedure   NEW_RENAME,NEAR
	DOSAssume   <DS>,"NEW_Rename"

ifdef DBCS				; ### if DBCS ###

	mov	ah,0			; reset DBCS flag
	mov	dl,cl			; reset counter
	mov	dh,cl			; save length to do
newren_loop:
	cmp	ah,1			; if it was lead byte
	jz	newren_dbcs
	mov	ah,0			; reset if it was single or tail byte
	mov	al,[bx]			; get source char
	invoke	testkanj
	jz	newren_load		; if not lead byte
newren_dbcs:
	inc	ah			; set dbcs flag
newren_load:
	lodsb				; get raw char
	cmp	al,'?'
	jnz	newren_store		; if not '?'
	cmp	ah,0
	jz	newren_conv		; if source is single
	cmp	ah,1
	jnz	newren_pass		; if source is not lead
	cmp	cl,dh
	jnz	newren_lead		; if this is not 1st char
	cmp	byte ptr [si],' '
	jz	newren_double		; if this is the end
newren_lead:
	cmp	byte ptr [si],'?'
	jnz	newren_pass		; if no '?' for tail byte
	cmp	cx,1
	jbe	newren_pass		; if no room for tail byte
newren_double:
	mov	al,[bx]
	stosb
	dec	dl
	inc	bx
	inc	si
	dec	cx
newren_conv:
	mov	al,[bx]
newren_store:
	stosb				; store in destination
	dec	dl			; decrese counter
newren_pass:
	inc	bx
	loop	newren_loop
	mov	cl,dl			; get stored length
	xor	ch,ch
	jcxz	newren_ret		; if all done
	mov	al,' '
	rep	stosb			; put sapce
newren_ret:
	return

else					; ### if Not DBCS ###

NEWNAM:
	LODSB
	CMP	AL,"?"
	JNZ	NOCHG
	MOV	AL,[BX] 		; Get replace char
NOCHG:
	STOSB
	INC	BX			; Next replace char
	LOOP	NEWNAM
	return
endif					; ### end if Not DBCS ###

EndProc NEW_RENAME


DOSCODE	ENDS
    END



