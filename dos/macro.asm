	TITLE	MACRO - Pathname and macro related internal routines
	NAME	MACRO

;
;	Microsoft Confidential
;	Copyright (C) Microsoft Corporation 1991
;	All Rights Reserved.
;

;**	MACRO.ASM
;
;	$AssignOper
;	FIND_DPB
;	InitCDS
;	$UserOper
;	GetVisDrv
;	GetThisDrv
;	GetCDSFromDrv
;
;   Revision history:
;
;	Created: MZ 4 April 1983
;		 MZ 18 April 1983   Make TransFCB handle extended FCBs
;		 AR 2 June 1983     Define/Delete macro for NET redir.
;		 MZ 3 Nov 83	    Fix InitCDS to reset length to 2
;		 MZ 4 Nov 83	    Fix NetAssign to use STRLEN only
;		 MZ 18 Nov 83	    Rewrite string processing for subtree
;				    aliasing.
;
;   MSDOS performs several types of name translation.  First, we maintain for
;   each valid drive letter the text of the current directory on that drive.
;   For invalid drive letters, there is no current directory so we pretend to
;   be at the root.  A current directory is either the raw local directory
;   (consisting of drive:\path) or a local network directory (consisting of
;   \\machine\path.  There is a limit on the point to which a ..  is allowed.
;
;   Given a path, MSDOS will transform this into a real from-the-root path
;   without .  or ..  entries.	Any component that is > 8.3 is truncated to
;   this and all * are expanded into ?'s.
;
;   The second part of name translation involves subtree aliasing.  A list of
;   subtree pairs is maintained by the external utility SUBST.	The results of
;   the previous 'canonicalization' are then examined to see if any of the
;   subtree pairs is a prefix of the user path.  If so, then this prefix is
;   replaced with the other subtree in the pair.
;
;   A third part involves mapping this "real" path into a "physical" path.  A
;   list of drive/subtree pairs are maintained by the external utility JOIN.
;   The output of the previous translation is examined to see if any of the
;   subtrees in this list are a prefix of the string.  If so, then the prefix
;   is replaced by the appropriate drive letter.  In this manner, we can
;   'mount' one device under another.
;
;   The final form of name translation involves the mapping of a user's
;   logical drive number into the internal physical drive.  This is
;   accomplished by converting the drive number into letter:CON, performing
;   the above translation and then converting the character back into a drive
;   number.
;
;   There are two main entry points:  TransPath and TransFCB.  TransPath will
;   take a path and form the real text of the pathname with all .  and ..
;   removed.  TransFCB will translate an FCB into a path and then invoke
;   TransPath.
;
;	A000	version 4.00  Jan. 1988

	.xlist
	.xcref
	include version.inc
	include dosseg.inc
	INCLUDE DOSSYM.INC
	INCLUDE DEVSYM.INC
	include mult.inc
	include dpb.inc
	include curdir.inc
	.cref
	.list

Installed = TRUE

	I_need	ThisCDS,DWORD		; pointer to CDS used
	I_need	CDSAddr,DWORD		; pointer to CDS table
	I_need	CDSCount,BYTE		; number of CDS entries
	I_need	CurDrv,BYTE		; current macro assignment (old
					; current drive)
	I_need	NUMIO,BYTE		; Number of physical drives
	I_need	fSharing,BYTE		; TRUE => no redirection allowed
	I_need	DummyCDS,80h		; buffer for dummy cds
	I_need	DIFFNAM,BYTE		; flag for MyName being set
	I_need	MYNAME,16		; machine name
	I_need	MYNUM,WORD		; machine number
	I_need	DPBHEAD,DWORD		; beginning of DPB chain
	I_need	EXTERR_LOCUS,BYTE	; Extended Error Locus
	I_need	DrvErr,BYTE		; drive error


DOSCODE	SEGMENT
	ASSUME	SS:DOSDATA,CS:DOSCODE

BREAK <$AssignOper -- Set up a Macro>

; Inputs:
;	AL = 00 get assign mode 		    (ReturnMode)
;	AL = 01 set assign mode 		    (SetMode)
;	AL = 02 get attach list entry		    (GetAsgList)
;	AL = 03 Define Macro (attch start)
;	    BL = Macro type
;	       = 0 alias
;	       = 1 file/device
;	       = 2 drive
;	       = 3 Char device -> network
;	       = 4 File device -> network
;	    DS:SI -> ASCIZ source name
;	    ES:DI -> ASCIZ destination name
;	AL = 04 Cancel Macro
;	    DS:SI -> ASCIZ source name
;	AL = 05 Modified get attach list entry
;	AL = 06 Get ifsfunc item
;	AL = 07 set in_use of a drive's CDS
;	     DL = drive number, 0=default  0=A,,
;	AL = 08 reset in_use of a drive's CDS
;	     DL = drive number, 0=A, 1=B,,,
; Function:
;	Do macro stuff
; Returns:
;	Std Xenix style error return

procedure   $AssignOper,NEAR

	CMP	AL,7			      ; set in_use ?		;AN000;
	JNZ	chk08			      ; no			;AN000;
srinuse:								;AN000;
	PUSH	AX			      ; save al 		;AN000;
	MOV	AL,DL			      ; AL= drive id		;AN000;
	CALL	GetCDSFromDrv		      ; ds:si -> cds		;AN000;
	POP	AX			      ; 			;AN000;
	JC	baddrv			      ; bad drive		;AN000;
	CMP	WORD PTR [SI.curdir_devptr],0 ; dpb ptr =0 ?		;AN000;
	JZ	baddrv			      ;     no			;AN000;
	CMP	AL,7			      ; set ?			;AN000;
	JNZ	resetdrv		      ; no			;AN000;
	OR	[SI.curdir_flags],curdir_inuse; set in_use		;AN000;
	JMP	SHORT okdone		      ; 			;AN000;
resetdrv:								;AN000;
	AND	[SI.curdir_flags],NOT curdir_inuse; reset in_use		;AN000;
	JMP	SHORT okdone			; 			;AN000;
baddrv: 								;AN000;
	MOV	AX,error_invalid_drive	      ; error			;AN000;
	JMP	SHORT ASS_ERR		      ; 			;AN000;
chk08:									;AN000;
	CMP	AL,8			      ; reset inuse ?		;AN000;
	JZ	srinuse 		      ; yes			;AN000;

	IF	NOT INSTALLED
	transfer NET_ASSOPER
	ELSE
	PUSH	AX
	MOV	AX,(multnet SHL 8) OR 30
	INT	2FH
	POP	BX			; Don't zap error code in AX
	JC	ASS_ERR
okdone:
	transfer SYS_RET_OK

ASS_ERR:
	transfer SYS_RET_ERR
	ENDIF

EndProc $AssignOper

Break <FIND_DPB - Find a DPB from a drive number>

;**	FIND_DPB - Find a DPB from a Drive #
;
;	ENTRY	AL has drive number A = 0
;	EXIT	'C' set
;		    No DPB for this drive number
;		'C' clear
;		    DS:SI points to DPB for drive
;	USES	SI, DS, Flags

Procedure FIND_DPB,NEAR
	ASSUME	CS:DOSCODE,SS:DOSDATA

	LDS	SI,DPBHEAD		;smr;SS Override
fdpb5:	CMP	SI,-1
	JZ	fdpb10
	CMP	AL,[SI].dpb_drive
	jz	ret_label		; Carry clear (retz)
	LDS	SI,[SI].dpb_next_dpb
	JMP	fdpb5

fdpb10:	STC

ret_label:
	return

EndProc FIND_DPB

	Break <InitCDS - set up an empty CDS>


;**	InitCDS - Setup an Empty CDS
;
;	ENTRY	ThisCDS points to CDS
;		AL has uppercase drive letter
;	EXIT	ThisCDS is now empty
;		(ES:DI) = CDS
;		'C' set if no DPB associated with drive
;	USES	AH,ES,DI, Flags

Procedure InitCDS,NEAR
	DOSASSUME <SS>,"InitCDS"
	ASSUME	CS:DOSCODE

	SAVE	<ax>			; save (AL) for caller
	LES	DI,THISCDS		; (es:di) = CDS address
	MOV	ES:[DI].curdir_flags,0	; "free" CDS
	SUB	AL,"A"-1                ; A = 1
	CMP	NUMIO,AL					;smr;SS Override
	JC	icdsx			; Drive does not map a physical drive
	dec	ax			; (AL) = 0 if A, 1 if B, etc.
	PUSH	AX			; save drive number for later
	add	al,"A"
	MOV	AH,':'
	MOV	WORD PTR ES:[DI.curdir_text],AX 	; set "x:"
	MOV	WORD PTR ES:[DI.curdir_text+2],"\"   ; NUL terminate
	.errnz	CURDIR_INUSE-4000h
	OR	byte ptr ES:[DI].curdir_flags+1,curdir_inuse SHR 8
	sub	ax,ax
	MOV	ES:[DI].curdir_ID,ax
	MOV	ES:[DI].curdir_ID+2,ax
	mov	al,2
	MOV	ES:[DI].curdir_END,ax
	pop	ax			; (al) = drive number
	SAVE	<ds,si>
	invoke	FIND_DPB
	JC	icds5			; OOOOPPPPPSSSS!!!!
	MOV	WORD PTR ES:[DI.curdir_devptr],SI
	MOV	WORD PTR ES:[DI.curdir_devptr+2],DS
icds5:	RESTORE <si,ds>
icdsx:	RESTORE <ax>
	return

EndProc InitCDS

Break <$UserOper - get/set current user ID (for net)>

;
;   $UserOper - retrieve or initiate a user id string.	MSDOS will only
;	maintain this string and do no verifications.
;
;   Inputs:	AL has function type (0-get 1-set 2-printer-set 3-printer-get
;				      4-printer-set-flags,5-printer-get-flags)
;		DS:DX is user string pointer (calls 1,2)
;		ES:DI is user buffer (call 3)
;		BX is assign index (calls 2,3,4,5)
;		CX is user number (call 1)
;		DX is flag word (call 4)
;   Outputs:	If AL = 0 then the current user string is written to DS:DX
;			and user CX is set to the user number
;		If AL = 3 then CX bytes have been put at input ES:DI
;		If AL = 5 then DX is flag word

Procedure   $UserOper,NEAR
	ASSUME	CS:DOSCODE,SS:DOSDATA
	PUSH	AX
	SUB	AL,1			; quick dispatch on 0,1
	POP	AX
	JB	UserGet 		; return to user the string
	JZ	UserSet 		; set the current user
	CMP	AL,5			; test for 2,3,4 or 5
	JBE	UserPrint		; yep
	MOV	EXTERR_LOCUS,errLoc_Unk ; Extended Error Locus	;smr;SS Override
	error	error_Invalid_Function	; not 0,1,2,3

UserGet:
; Transfer MYNAME to DS:DX
; Set Return CX to MYNUM
	PUSH	DS			; switch registers
	POP	ES
	MOV	DI,DX			; destination
	MOV	CX,[MYNUM]		; Get number		;smr;SS Override
	invoke	get_user_stack
	MOV	[SI.User_CX],CX 	; Set number return
	Context DS			; point to DOSDATA
ASSUME	DS:DOSDATA
	MOV	SI,OFFSET DOSDATA:MyName   ; point source to user string
UserMove:
ASSUME	DS:NOTHING
	MOV	CX,15
	REP	MOVSB			; blam.
	XOR	AX,AX			; 16th byte is 0
	STOSB
UserBye:
	transfer    sys_ret_ok		; no errors here

UserSet:
ASSUME	DS:NOTHING
; Transfer DS:DX to MYNAME
; CX to MYNUM
	MOV	[MYNUM],CX					;smr;SS Override
	MOV	SI,DX			; user space has source
	Context ES
	MOV	DI,OFFSET DOSDATA:MyName   ; point dest to user string
	INC	[DiffNam]		  ; signal change	;smr;SS Override
	JMP	UserMove

UserPrint:
	ASSUME	ES:NOTHING
IF NOT Installed
	transfer PRINTER_GETSET_STRING
ELSE
	PUSH	AX
	MOV	AX,(multNET SHL 8) OR 31
	INT	2FH
	POP	DX			; Clean stack
	JNC	OKPA
	transfer SYS_RET_ERR

OKPA:
	transfer SYS_RET_OK
ENDIF

EndProc $UserOper

Break	<GetVisDrv - return visible drive>

;
;   GetVisDrv - correctly map non-spliced inuse drives
;
;   Inputs:	AL has drive identifier (0=default)
;   Outputs:	Carry Set - invalid drive/macro
;		Carry Clear - AL has physical drive (0=A)
;		    ThisCDS points to CDS
;   Registers modified: AL

Procedure   GetVisDrv,NEAR
	ASSUME	CS:DOSCODE,SS:DOSDATA
	CALL	GetThisDrv		; get inuse drive
	retc
	SAVE	<DS,SI>
	LDS	SI,ThisCDS					;smr;SS Override
	TEST	[SI].curdir_flags,curdir_splice
	RESTORE <SI,DS>
	retz				; if not spliced, return OK
	MOV	[DrvErr],error_invalid_drive ;IFS.				;AN000;smr;SS Override
	STC				; signal error
	return
EndProc GetVisDrv

Break <Getthisdrv - map a drive designator (0=def, 1=A...)>

;
;   GetThisDrv - look through a set of macros and return the current drive and
;	macro pointer
;
;   Inputs:	AL has drive identifier (1=A, 0=default)
;   Outputs:
;		Carry Set - invalid drive/macro
;		Carry Clear - AL has physical drive (0=A)
;		   ThisCDS points to macro
;   Registers modified: AL

Procedure   GetThisDrv,NEAR
	ASSUME	CS:DOSCODE,SS:DOSDATA
	OR	AL,AL			; are we using default drive?
	JNZ	gtd10			; no, go get the CDS pointers
	MOV	AL,[CurDrv]		; get the current drive
	INC	ax			; Counteract next instruction
gtd10:	DEC	ax			; 0 = A
	SAVE	<DS,SI> 		; save world
	mov	[EXTERR_LOCUS],errLOC_Disk			;smr;SS Override
	TEST	fSharing,-1		; Logical or Physical?	;smr;SS Override
	JZ	gtd20			; Logical
	SAVE	<AX,ES,DI>
	MOV	WORD PTR ThisCDS,OFFSET DOSDATA:DummyCDS	;smr;SS Override
	MOV	WORD PTR ThisCDS+2,SS	;	ThisCDS = &DummyCDS;smr;
	ADD	AL,'A'
	CALL	InitCDS 		;	InitCDS(c);
	TEST	ES:[DI.curdir_flags],curdir_inuse	; Clears carry
	RESTORE <DI,ES,AX>
	JZ	gtd30			; Not a physical drive.
	JMP	SHORT gtdx		; carry clear

gtd20:	invoke	GetCDSFromDrv
	JC	gtd30			; Unassigned CDS -> return error already set
	TEST	[SI.curdir_flags],curdir_inuse	; Clears Carry
	JNZ	gtdx				; carry clear
gtd30:	MOV	AL,error_invalid_drive	; invalid FAT drive
	MOV	[DrvErr],AL		; save this for IOCTL
	mov	[EXTERR_LOCUS],errLOC_UNK
	STC
gtdx:	RESTORE <SI,DS>		; restore world
	return

EndProc GetThisDrv

Break <GetCDSFromDrv - convert a drive number to a CDS pointer>

;
;   GetCDSFromDrv - given a physical drive number, convert it to a CDS
;	pointer, returning an error if the drive number is greater than the
;	number of CDS's
;
;   Inputs:	AL is physical unit # A=0...
;   Outputs:	Carry Set if Bad Drive
;		Carry Clear
;		    DS:SI -> CDS
;		    [THISCDS] = DS:SI
;   Registers modified: DS,SI

Procedure   GetCDSFromDrv,NEAR
	ASSUME	CS:DOSCODE,SS:DOSDATA
	CMP	AL,[CDSCount]		; is this a valid designator;smr;SS Override
	JB	GetCDS			; yes, go get the macro
	STC				; signal error
	return				; bye
GetCDS:
	SAVE	<BX,AX>
	LDS	SI,[CDSAddr]		; get pointer to table	;smr;SS Override
	MOV	BL,SIZE CurDir_list	; size in convenient spot
	MUL	BL			; get net offset
	ADD	SI,AX			; convert to true pointer
	MOV	WORD PTR [ThisCDS],SI	; store convenient offset;smr;SS Override
	MOV	WORD PTR [ThisCDS+2],DS ; store convenient segment;smr;SS Override
	RESTORE <AX,BX>
	CLC				; no error
	return				; bye!
EndProc GetCDSFromDrv

DOSCODE ends
	END


