	TITLE	HANDLE - Handle-related system calls
	NAME	HANDLE

;
;	Microsoft Confidential
;	Copyright (C) Microsoft Corporation 1991
;	All Rights Reserved.
;

;**	Handle related system calls for MSDOS 2.X.  Only top-level system calls
;	are present.	I/O specs are defined in DISPATCH.  The system calls are:
;
;	$Close	   written
;	$Commit    written		  DOS 3.3  F.C. 6/4/86
;	$ExtHandle written		  DOS 3.3  F.C. 6/4/86
;	$Read	   written
;	Align_Buffer		  DOS 4.00
;	$Write	   written
;	$LSeek	   written
;	$FileTimes written
;	$Dup	   written
;	$Dup2	   written
;
;	Revision history:
;
;	    Created: MZ 28 March 1983
;		 MZ 15 Dec   1982 Jeff Harbers and Multiplan hard disk copy
;				  rely on certain values in AX when $CLOSE
;				  succeeds even though we document it as
;				  always trashing AX.
;
;	    A000	version 4.00  Jan. 1988

	.xlist
	.xcref
	include version.inc
	include dosseg.inc
	INCLUDE DOSSYM.INC
	INCLUDE DEVSYM.INC
	include sf.inc
	include mult.inc
	include pdb.inc
	include filemode.inc
	include syscall.inc
	include bugtyp.inc
	.cref
	.list


DOSDATA Segment

	extrn	ThisSFT:dword		; pointer to SFT entry
	extrn	DMAAdd:dword		; old-style DMA address
	extrn	EXTERR_LOCUS:byte	; Extended Error Locus
	extrn	FailErr:byte		; failed error flag
	extrn	User_ID:word		; current effective user_id
	extrn	JShare:dword		; jump table
	extrn	CurrentPDB:word 	; current process data block
	extrn	EXTOPEN_ON:byte 	; flag for extended open
	extrn	THISCDS:dword
	extrn	DUMMYCDS:byte
	extrn	SAVE_ES:word		; saved ES
	extrn	SAVE_DI:word		; saved DI
	extrn	SAVE_DS:word		; saved DS
	extrn	SAVE_SI:word		; saved SI
	extrn	SAVE_CX:word		; saved CX

;SR;
;   Flag to indicate WIN386 presence
;
	extrn	IsWin386:byte

DOSDATA ENDS

DOSCODE	SEGMENT
	ASSUME	SS:DOSDATA,CS:DOSCODE

	EXTRN	DOS_Read:NEAR
	EXTRN	DOS_Write:NEAR
	EXTRN	pJfnFromHandle:near


	BREAK <$Close - return a handle to the system>


;**	$Close - Close a file Handle
;
;	BUGBUG - close gets called a LOT with invalid handles - sizzle that
;		path
;
;	Assembler usage:
;	    MOV     BX, handle
;	    MOV     AH, Close
;	    INT     int_command
;
;	ENTRY	(bx) = handle
;	EXIT	<normal INT21 return convention>
;	USES	all

Procedure   $Close,NEAR
	ASSUME	CS:DOSCODE,SS:DOSDATA

	fmt TypSysCall,LevArgs,<"$p  Handle = $x\n">,<BX>

;	Grab the SFT pointer from the JFN.

	call	CheckOwner		; get system file entry
	DLJC	CloseError		; error return
	fmt TypAccess,LevSFN,<"$p  Close SFT $x:$x\n">,<es,di>
	context DS			; For DOS_CLOSE
	MOV	WORD PTR [ThisSFT],DI	; save offset of pointer
	MOV	WORD PTR [ThisSFT+2],ES ; save segment value

; DS:SI point to JFN table entry.
; ES:DI point to SFT
;
; We now examine the user's JFN entry; If the file was a 70-mode file (network
; FCB, we examine the ref count on the SFT;  if it was 1, we free the JFN.
; If the file was not a net FCB, we free the JFN too.

	CMP	ES:[DI].sf_ref_count,1	; will the SFT become free?
	JZ	FreeJFN 		; yes, free JFN anyway.
	MOV	AL,BYTE PTR ES:[DI].sf_mode
	AND	AL,sharing_mask
	CMP	AL,sharing_net_fcb
	JZ	PostFree		; 70-mode and big ref count => free it

; The JFN must be freed.  Get the pointer to it and replace the contents with
; -1.

FreeJFN:
	call	pJFNFromHandle		;   d = pJFN (handle);
	fmt TypAccess,LevSFN,<"$p  Close jfn pointer $x:$x\n">,<es,di>
	MOV	BYTE PTR ES:[DI],0FFh	; release the JFN
PostFree:

; ThisSFT is correctly set, we have DS = DOSDATA.  Looks OK for a DOS_CLOSE!

	invoke	DOS_Close

; DOS_Close may return an error.  If we see such an error, we report it but
; the JFN stays closed because DOS_Close always frees the SFT!

	JC	CloseError
	MOV	AH,close		; MZ Bogus multiplan fix
	transfer    Sys_Ret_OK
CloseError:
	ASSUME	DS:NOTHING
	transfer    Sys_Ret_Err
EndProc $Close

	BREAK <$Commit - commit the file>

;**	$Commit - Commit a File
;
;	$Commit "commits" a file to disk - all of it's buffers are
;	flushed out.  BUGBUG - I'm pretty sure that $Commit doesn't update
;	the directory entry, etc., so this commit is pretty useless.  check
;	and fix this!! jgl
;
;	Assembler usage:
;	    MOV     BX, handle
;	    MOV     AH, Commit
;	    INT     int_command
;
;	ENTRY	(bx) = handle
;	EXIT	none
;	USES	all

Procedure   $Commit,NEAR
	ASSUME	CS:DOSCODE,SS:DOSDATA

;	Grab the SFT pointer from the JFN.

	call	CheckOwner		; get system file entry
	JC	Commiterror		; error return
	context DS			; For DOS_COMMIT
	MOV	WORD PTR [ThisSFT],DI	; save offset of pointer
	MOV	WORD PTR [ThisSFT+2],ES ; save segment value

;	ThisSFT is correctly set, we have DS = DOSDATA.  Looks OK for a DOS_COMMIT
;
;	ES:DI point to SFT

	invoke	DOS_COMMIT
	JC	Commiterror
	MOV	AH,Commit		;
	transfer    Sys_Ret_OK
Commiterror:
	ASSUME	DS:NOTHING
	transfer    Sys_Ret_Err

EndProc $Commit


	BREAK <$ExtHandle - extend handle count>

;**	$ExtHandle - Extend Handle Count
;
;	Assembler usage:
;	    MOV     BX, Number of Opens Allowed (MAX=65534;66535 is
;	    MOV     AX, 6700H			 reserved to mark SFT
;	    INT     int_command 		 busy )
;
;	ENTRY	(bx) = new number of handles
;	EXIT	'C' clear if OK
;		'C' set iff err
;		  (ax) = error code
;			 AX = error_not_enough_memory
;			      error_too_many_open_files
;	USES	all

Procedure   $ExtHandle,NEAR
	ASSUME	CS:DOSCODE,SS:DOSDATA

	XOR	BP,BP			; 0: enlarge   1: shrink  2:psp
	CMP	BX,FilPerProc
	JAE	exth2			; Don't set less than FilPerProcno
	MOV	BX,FilPerProc

exth2:	MOV	ES,CurrentPDB		; get user process data block;smr;SS Override
	MOV	CX,ES:PDB_JFN_Length	; get number of handle allowed
	CMP	BX,CX			; the requested == current
	JE	ok_done 		; yes and exit
	JA	larger			; go allocate new table

;	We're going to shrink the # of handles available

	MOV	BP,1			; shrink
	MOV	DS,WORD PTR ES:[PDB_JFN_Pointer+2] ;
	MOV	SI,BX			;
	SUB	CX,BX			; get difference

;	BUGBUG - code a SCASB here, should be a bit smaller
chck_handles:
	CMP	BYTE PTR DS:[SI],-1	; scan through handles to ensure close
	JNZ	too_many_files		; status
	INC	SI
	LOOP	chck_handles
	CMP	BX,FilPerProc		; = 20
	JA	larger			; no

	MOV	BP,2			; psp
	MOV	DI,PDB_JFN_Table	; es:di -> jfn table in psp
	PUSH	BX
	JMP	short movhandl

larger:
	CMP	BX,-1			; 65535 is not allowed
	JZ	invalid_func
	CLC
	PUSH	BX			; save requested number
	ADD	BX,0FH			; adjust to paragraph boundary
	MOV	CL,4
	RCR	BX,CL			; DOS 4.00 fix				;AC000;
	AND	BX,1FFFH		; clear most 3 bits

	PUSH	BP
	invoke	$ALLOC			; allocate memory
	POP	BP
	JC	no_memory		; not enough meory

	MOV	ES,AX			; es:di points to new table memory
	XOR	DI,DI
movhandl:
	MOV	DS,[CurrentPDB] 	; get user PDB address	;smr;SS Override

	test	BP,3			; enlarge ?
	JZ	enlarge 		; yes
	POP	CX			; cx = the amount you shrink
	PUSH	CX
	JMP	short copy_hand

;	Done.  'C' clear

ok_done:transfer    Sys_Ret_OK

too_many_files:
	MOV	AL,error_too_many_open_files
	transfer    Sys_Ret_Err


enlarge:
	MOV	CX,DS:[PDB_JFN_Length]	  ; get number of old handles
copy_hand:
	MOV	DX,CX
	LDS	SI,DS:[PDB_JFN_Pointer]   ; get old table pointer
ASSUME DS:NOTHING
	REP	MOVSB			; copy infomation to new table

	POP	CX			; get new number of handles
	PUSH	CX			; save it again
	SUB	CX,DX			; get the difference
	MOV	AL,-1			; set availability to handles
	REP	STOSB

	MOV	DS,[CurrentPDB] 	; get user process data block;smr;SS Override
	CMP	WORD PTR DS:[PDB_JFN_Pointer],0  ; check if original table pointer
	JNZ	update_info		; yes, go update PDB entries
	PUSH	BP
	PUSH	DS			; save old table segment
	PUSH	ES			; save new table segment
	MOV	ES,WORD PTR DS:[PDB_JFN_Pointer+2] ; get old table segment
	invoke	$DEALLOC		; deallocate old table meomory
	POP	ES			; restore new table segment
	POP	DS			; restore old table segment
	POP	BP

update_info:
	test	BP,2			; psp?
	JZ	non_psp 		; no
	MOV	WORD PTR DS:[PDB_JFN_Pointer],PDB_JFN_Table   ; restore
	JMP	short final
non_psp:
	MOV	WORD PTR DS:[PDB_JFN_Pointer],0  ; new table pointer offset always 0
final:
	MOV	WORD PTR DS:[PDB_JFN_Pointer+2],ES  ; update table pointer segment
	POP	DS:[PDB_JFN_Length]	 ; restore new number of handles
	transfer   Sys_Ret_Ok
no_memory:
	POP	BX			; clean stack
	MOV	AL,error_not_enough_memory
	transfer    Sys_Ret_Err
invalid_func:
	MOV	AL,error_invalid_function
	transfer    Sys_Ret_Err
EndProc $ExtHandle

	BREAK <$READ - Read from a file handle>

;**	$Read - Read from a File Handle
;
;   Assembler usage:
;
;	LDS	DX, buf
;	MOV	CX, count
;	MOV	BX, handle
;	MOV	AH, Read
;	INT	int_command
;	  AX has number of bytes read
;
;	ENTRY	(bx) = file handle
;		(cx) = byte count
;		(ds:dx) = buffer address
;	EXIT	Through system call return so that to user:
;		  'C' clear if OK
;		    (ax) = bytes read
;		  'C' set if error
;		    (ax) = error code

procedure   $READ,NEAR
	ASSUME	CS:DOSCODE,SS:DOSDATA
	fmt TypSysCall,LevArgs,<" Handle $x Cnt $x Buf $x:$x\n">,<BX,CX,DS,DX>

	MOV	SI,OFFSET DOSCODE:DOS_Read
ReadDo:	call	pJFNFromHandle
	JC	ReadError
	MOV	AL,ES:[DI]
	call	CheckOwner		; get the handle
	JNC	ReadSetup		; no errors do the operation

;	Have an error.	'C' set

ReadError:
	transfer    SYS_RET_ERR 	; go to error traps

ReadSetup:
	MOV	WORD PTR [ThisSFT],DI	; save offset of pointer;smr;SS Override
	MOV	WORD PTR [ThisSFT+2],ES ; save segment value	;smr;SS Override
;; Extended Open
	TESTB	ES:[DI.sf_mode],INT_24_ERROR  ;AN000;;EO. need i24
	JZ	needi24 		      ;AN000;;EO. yes
	OR	[EXTOPEN_ON],EXT_OPEN_I24_OFF ;AN000;;EO. set it off;smr;SS Override
needi24:				      ;AN000;

;; Extended Open
	SAVE	<<WORD PTR [DMAAdd]>, <WORD PTR [DMAAdd+2]>>;smr;SS Override
;;;;;	BAD SPOT FOR 286!!! SEGMENT ARITHMETIC!!!
	CALL	Align_Buffer		;AN000;MS. align user's buffer
;;;;;	END BAD SPOT FOR 286!!! SEGMENT ARITHMETIC!!!
	context DS			; go for DOS addressability
	CALL	SI			; indirect call to operation
	RESTORE <<WORD PTR [DMAAdd+2]>, <WORD PTR [DMAAdd]>>

	JNC	READ_OK 		      ;AN002;
	JMP	READERROR		      ;AN002; if error, say bye bye

READ_OK:
	MOV	AX,CX			; get correct return in correct reg
	transfer    sys_ret_ok		; successful return

EndProc $READ

;
;   Input: DS:DX points to user's buffer addr
;   Function: rearrange segment and offset for READ/WRITE buffer
;   Output: [DMAADD] set
;
;

procedure   Align_Buffer,NEAR		;AN000;
	ASSUME	CS:DOSCODE,SS:DOSDATA  ;AN000;
	MOV	BX,DX			; copy offset
	SAVE	<CX>			; don't stomp on count
	MOV	CL,4			; bits to shift bytes->para
	SHR	BX,CL			; get number of paragraphs
	RESTORE <CX>		; get count back
	MOV	AX,DS			; get original segment
	ADD	AX,BX			; get new segment
	MOV	DS,AX			; in seg register
	AND	DX,0Fh			; normalize offset
	MOV	WORD PTR [DMAAdd],DX	; use user DX as offset	;smr;SS Override
	MOV	WORD PTR [DMAAdd+2],DS	; use user DS as segment for DMA;smr;SS Override
	return				;AN000;
EndProc Align_Buffer			;AN000;

BREAK <$WRITE - write to a file handle>

;
;   Assembler usage:
;	    LDS     DX, buf
;	    MOV     CX, count
;	    MOV     BX, handle
;	    MOV     AH, Write
;	    INT     int_command
;	  AX has number of bytes written
;   Errors:
;	    AX = write_invalid_handle
;	       = write_access_denied
;
;   Returns in register AX

procedure   $WRITE,NEAR
	ASSUME	CS:DOSCODE,SS:DOSDATA
	fmt TypSysCall,LevArgs,<" Handle $x Cnt $x Buf $x:$x\n">,<BX,CX,DS,DX>
	MOV	SI,OFFSET DOSCODE:DOS_Write
	JMP	ReadDo
EndProc $Write

BREAK <$LSEEK - move r/w pointer>

;
;   Assembler usage:
;	    MOV     DX, offsetlow
;	    MOV     CX, offsethigh
;	    MOV     BX, handle
;	    MOV     AL, method
;	    MOV     AH, LSeek
;	    INT     int_command
;	  DX:AX has the new location of the pointer
;   Error returns:
;	    AX = error_invalid_handle
;	       = error_invalid_function
;   Returns in registers DX:AX

procedure   $LSEEK,NEAR
	ASSUME	CS:DOSCODE,SS:DOSDATA
	call	CheckOwner		; get system file entry
LSeekError:
	JNC	CHKOWN_OK		;AN002;
	JMP	ReadError		;AN002; error return

CHKOWN_OK:				;AN002;
	CMP	AL,2			; is the seek value correct?
	JBE	LSeekDisp		; yes, go dispatch
	MOV	EXTERR_LOCUS,errLoc_Unk ; Extended Error Locus	;smr;SS Override
	error	error_invalid_function	; invalid method
LSeekDisp:
	CMP	AL,1			; best way to dispatch; check middle
	JB	LSeekStore		; just store CX:DX
	JA	LSeekEOF		; seek from end of file
	ADD	DX,WORD PTR ES:[DI.SF_Position]
	ADC	CX,WORD PTR ES:[DI.SF_Position+2]
LSeekStore:
	MOV	AX,CX			; AX:DX
	XCHG	AX,DX			; DX:AX is the correct value
LSeekSetpos:
	MOV	WORD PTR ES:[DI.SF_Position],AX
	MOV	WORD PTR ES:[DI.SF_Position+2],DX
	invoke	Get_user_stack
	MOV	DS:[SI.User_DX],DX	; return DX:AX
	transfer    SYS_RET_OK		; successful return

LSeekEOF:
	TESTB	ES:[DI].SF_FLAGS,sf_isnet
	JNZ	Check_LSeek_Mode	; Is Net
LOCAL_LSeek:
	ADD	DX,WORD PTR ES:[DI.SF_Size]
	ADC	CX,WORD PTR ES:[DI.SF_Size+2]
	JMP	LSeekStore		; go and set the position

Check_LSeek_Mode:
	TESTB	ES:[DI.sf_mode],sf_isfcb
	JNZ	LOCAL_LSeek		; FCB treated like local file
	MOV	AX,ES:[DI.sf_mode]
	AND	AX,sharing_mask
	CMP	AX,sharing_deny_none
	JZ	NET_LSEEK		; LSEEK exported in this mode
	CMP	AX,sharing_deny_read
	JNZ	LOCAL_LSeek		; Treated like local Lseek
NET_LSEEK:
;	 JMP	 LOCAL_LSeek
; REMOVE ABOVE INSTRUCTION TO ENABLE DCR 142
	CallInstall Net_Lseek,multNet,33
	JNC	LSeekSetPos
	transfer    SYS_RET_ERR

EndProc $LSeek

BREAK <FileTimes - modify write times on a handle>


;----------------------------------------------------------------------------
;   Assembler usage:
;	    MOV AH, FileTimes (57H)
;	    MOV AL, func
;	    MOV BX, handle
;	; if AL = 1 then then next two are mandatory
;	    MOV CX, time
;	    MOV DX, date
;	    INT 21h
;	; if AL = 0 then CX/DX has the last write time/date
;	; for the handle.
;
;	AL=02		 get extended attributes
;	   BX=handle
;	   CX=size of buffer (0, return max size )
;	   DS:SI query list (si=-1, selects all EA)
;	   ES:DI buffer to hold EA list
;
;	AL=03		 get EA name list
;	   BX=handle
;	   CX=size of buffer (0, return max size )
;	   ES:DI buffer to hold name list
;
;	AL=04		 set extended attributes
;	   BX=handle
;	   ES:DI buffer of EA list
;
;
;
;
;   Error returns:
;	    AX = error_invalid_function
;	       = error_invalid_handle
;----------------------------------------------------------------------------

procedure   $File_Times,NEAR
	ASSUME	CS:DOSCODE,SS:DOSDATA
	cmp	al, 2			; correct subfunction ?
	jae	inval_func

	call	CheckOwner		; get sft
	jc	LSeekError		; bad handle

	or	al, al			; get time/date ?
	jnz	ft_set_time

;------ here we get the time & date from the sft for the user

	cli				; is this cli/sti reqd ? BUGBUG
	mov	cx, es:[di].sf_Time	; get the time
	mov	dx, es:[di].sf_Date	;  & date
	sti
	invoke	Get_User_Stack
	mov	[si].user_CX, cx
	mov	[si].user_DX, dx
	jmp	short ok_ret

;------ here we set the time in sft

ft_set_time:
	EnterCrit critSFT
	mov	es:[di].sf_Time, cx	; drop in new time
	mov	es:[di].sf_Date, dx	;  and date

	xor	ax, ax
if installed
	Call	JShare + 14 * 4		; SS Override
else
	Call	ShSU
endif

;------ set the flags in SFT entry

	and	es:[di].sf_Flags,NOT DEVID_FILE_CLEAN	; mark file as dirty

	or	es:[di].sf_Flags,SF_CLOSE_NODATE	; ask close not to
							;   bother about date
							;   and time
	LeaveCrit   critSFT
ok_ret:
	transfer SYS_RET_OK

inval_func:
	mov	ExtErr_Locus,errLoc_Unk ; Extended Error Locus	;SS Override
	error	error_invalid_function	; give bad return

EndProc $File_Times

BREAK <$DUP - duplicate a jfn>
;
;   Assembler usage:
;	    MOV     BX, fh
;	    MOV     AH, Dup
;	    INT     int_command
;	  AX has the returned handle
;   Errors:
;	    AX = dup_invalid_handle
;	       = dup_too_many_open_files

Procedure   $DUP,NEAR
	ASSUME	CS:DOSCODE,SS:DOSDATA
	MOV	AX,BX			; save away old handle in AX
	invoke	JFNFree 		; free handle? into ES:DI, new in BX
DupErrorCheck:
	JC	DupErr			; nope, bye
	SAVE	<ES,DI> 		; save away SFT
	RESTORE <SI,DS>		; into convenient place DS:SI
	XCHG	AX,BX			; get back old handle
	call	CheckOwner		; get sft in ES:DI
	JC	DupErr			; errors go home
	invoke	DOS_Dup_Direct
	call	pJFNFromHandle		; get pointer
	MOV	BL,ES:[DI]		; get SFT number
	MOV	DS:[SI],BL		; stuff in new SFT
	transfer    SYS_RET_OK		; and go home
DupErr: transfer    SYS_RET_ERR

EndProc $Dup

BREAK <$DUP2 - force a dup on a particular jfn>
;
;   Assembler usage:
;	    MOV     BX, fh
;	    MOV     CX, newfh
;	    MOV     AH, Dup2
;	    INT     int_command
;   Error returns:
;	    AX = error_invalid_handle

Procedure   $Dup2,NEAR
	ASSUME	CS:DOSCODE,SS:DOSDATA
	SAVE	<BX,CX> 		; save source
	MOV	BX,CX			; get one to close
	invoke	$Close			; close destination handle
	RESTORE <BX,AX>		; old in AX, new in BX
	call	pJFNFromHandle		; get pointer
	JMP	DupErrorCheck		; check error and do dup
EndProc $Dup2

Break	<CheckOwner - verify ownership of handles from server>

;
;   CheckOwner - Due to the ability of the server to close file handles for a
;   process without the process knowing it (delete/rename of open files, for
;   example), it is possible for the redirector to issue a call to a handle
;   that it soes not rightfully own.  We check here to make sure that the
;   issuing process is the owner of the SFT.  At the same time, we do a
;   SFFromHandle to really make sure that the SFT is good.
;
;	ENTRY	BX has the handle
;		User_ID is the current user
;	EXIT	Carry Clear => ES:DI points to SFT
;		Carry Set => AX has error code
;	USES	none

Procedure   CheckOwner,NEAR
	ASSUME	CS:DOSCODE,SS:DOSDATA
	invoke	SFFromHandle
	jc	ret_label	; retc
	push	ax

;SR;
;SR; WIN386 patch - Do not check for USER_ID for using handles since these 
;SR; are shared across multiple VMs in win386.
;SR;
	test	[IsWin386],1
	jz	no_win386		;win386 is not present
	xor	ax,ax			;set the zero flag
	jmp	short skip_win386	

no_win386:

	mov	ax,user_id					;smr;SS Override
	cmp	ax,es:[di].sf_UID

skip_win386:

	pop	ax
	retz
	mov	al,error_invalid_handle
	stc

ret_label:
	return
EndProc CheckOwner

DOSCODE	ENDS
	END

