	TITLE	DEV - Device call routines
	NAME	Dev

;
;	Microsoft Confidential
;	Copyright (C) Microsoft Corporation 1991
;	All Rights Reserved.
;


;**	Misc Routines to do 1-12 low level I/O and call devices
;
;	IOFUNC
;	DEVIOCALL
;	SETREAD
;	SETWRITE
;	DEVIOCALL2
;	DEV_OPEN_SFT
;	DEV_CLOSE_SFT
;	RW_SC
;	IN_SC
;	INVALIDATE_SC
;	VIRREAD
;	SC2BUF
;
;	Revision history:
;
;		A000  version 4.00     Jan. 1988
;		A010  disable change line for SHARE /NC

	.xlist
	.xcref
	include version.inc
	include dosseg.inc
	INCLUDE DOSSYM.INC
	INCLUDE DEVSYM.INC
	include dpb.inc
	include sf.inc
	.cref
	.list

	i_need	IOXAD,DWORD
	i_need	IOSCNT,WORD
	i_need	DEVIOBUF,4
	i_need	IOCALL,BYTE
	i_need	IOMED,BYTE
	i_need	IORCHR,BYTE
	i_need	CALLSCNT,WORD
	i_need	DMAAdd,DWORD
	i_need	CallDevAd,DWORD
	i_need	CallXAD,DWORD
	i_need	DPBHead,DWORD
	i_need	ThisSFT,DWORD
	i_need	ThisDPB,DWORD
	i_need	DevCall,DWORD
	i_need	VerFlg,BYTE
	i_need	HIGH_SECTOR,WORD	       ;AN000;
	i_need	CALLSSEC,WORD		       ;AN000;
	i_need	CALLNEWSC,DWORD 	       ;AN000;
	i_need	SC_CACHE_COUNT,WORD	       ;AN000;
	i_need	SC_CACHE_PTR,DWORD	       ;AN000;
	i_need	CURSC_SECTOR,WORD	       ;AN000;
	i_need	SEQ_SECTOR,DWORD	       ;AN000;
	i_need	SC_SECTOR_SIZE,WORD	       ;AN000;
	i_need	CURSC_DRIVE,BYTE	       ;AN000;
	i_need	SC_DRIVE,BYTE		       ;AN000;
	i_need	SC_STATUS,WORD		       ;AN000;
	i_need	SC_FLAG,BYTE		       ;AN000;
	i_need	TEMP_VAR,WORD		       ;AN000;
	i_need	TEMP_VAR2,WORD		       ;AN000;
	i_need	InterChar,BYTE	;AN000; interim character flag 2/13/KK
	i_need	InterCon,BYTE	;AN000; Console mode flag(1:interim mode) 2/13/KK
	i_need	SaveCurFlg,BYTE ;AN000; Console out mode(1:print & don't adv cursor) 2 /13/KK
	i_need	DDMOVE,BYTE	;AN000; flag for DWORD move
	i_need	DOS34_FLAG,WORD ;AN000;
	i_need	fshare,BYTE	;AN010; share flag

	i_need 	IoStatFail,BYTE	;SR; set if user failed on I24 in IOFUNC


DOSCODE	Segment

	ASSUME	CS:DOSCODE

Public DEV001S, DEV001E 		; Pathgen labels
DEV001s:
;		length of packets
LenTab	DB	DRDWRHL, DRDNDHL, DRDWRHL, DSTATHL, DFLSHL, DRDNDHL

;		Error	Function

CmdTab	DB	86h,	DEVRD		; 0 input
	DB	86h,	DEVRDND 	; 1 input status
	DB	87h,	DEVWRT		; 2 output
	DB	87h,	DEVOST		; 3 output status
	DB	86h,	DEVIFL		; 4 input flush
	DB	86H,	DEVRDND 	; 5 input status with system WAIT
DEV001E:

	ASSUME	CS:DOSCODE, SS:DOSDATA

Break	<IOFUNC -- DO FUNCTION 1-12 I/O>
;----------------------------------------------------------------------------
;
; Procedure Name : IOFUNC
;
; Inputs:
;	DS:SI Points to SFT
;	AH is function code
;		= 0 Input
;		= 1 Input Status
;		= 2 Output
;		= 3 Output Status
;		= 4 Flush
;		= 5 Input Status - System WAIT invoked for K09 if no char
;				   present.
;	AL = character if output
; Function:
;	Perform indicated I/O to device or file
; Outputs:
;	AL is character if input
;	If a status call
;		zero set if not ready
;		zero reset if ready (character in AL for input status)
; For regular files:
;	Input Status
;		Gets character but restores position
;		Zero set on EOF
;	Input
;		Gets character advances position
;		Returns ^Z on EOF
;	Output Status
;		Always ready
; AX altered, all other registers preserved
;----------------------------------------------------------------------------

procedure   IOFUNC,NEAR

	Assert	ISSFT,<DS,SI>,"IOFUNC"

					
	MOV	WORD PTR [IOXAD+2],SS	; SS override for IOXAD, IOSCNT, 
					; DEVIOBUF

					; DEVIOBUF is in DOSDATA
	MOV	WORD PTR [IOXAD],OFFSET DOSDATA:DEVIOBUF
	MOV	WORD PTR [IOSCNT],1
	MOV	WORD PTR [DEVIOBUF],AX
	TESTB	[SI].SF_FLAGS,sf_isnet
	JZ	IOTO22								;AN000;
	JMP	IOTOFILE							;AN000;
IOTO22:
	TESTB	[SI].SF_FLAGS,devid_device
	JNZ	IOTo33								;AN000;
	JMP	IOTOFILE							;AN000;
IOTO33:
	save_world
	MOV	DX,DS
	MOV	BX,SS
	MOV	DS,BX
	MOV	ES,BX

	ASSUME	DS:DOSDATA

	XOR	BX,BX
	cmp	ah,5		    ; system wait enabled?
	jnz	no_sys_wait
	or	bx,0400H	    ; Set bit 10 in status word for driver
				    ; It is up to device driver to carry out
				    ; appropriate action.
no_sys_wait:
	MOV	[IOCALL.REQSTAT],BX
	XOR	BX,BX
	MOV	BYTE PTR [IOMED],BL

; moved out side this DOSCODE segment.
; Table	Segment
; Public DEV001S, DEV001E 		; Pathgen labels
; DEV001s:
; ;		length of packets
; LenTab	DB	DRDWRHL, DRDNDHL, DRDWRHL, DSTATHL, DFLSHL, DRDNDHL
; 
; ;		Error	Function
; 
; CmdTab	DB	86h,	DEVRD		; 0 input
; 	DB	86h,	DEVRDND 	; 1 input status
; 	DB	87h,	DEVWRT		; 2 output
; 	DB	87h,	DEVOST		; 3 output status
; 	DB	86h,	DEVIFL		; 4 input flush
; 	DB	86H,	DEVRDND 	; 5 input status with system WAIT
; DEV001E:
; Table	ENDS

	MOV	BL,AH			; get function
	MOV	AH,LenTab[BX]
	SHL	BX,1
	MOV	CX,WORD PTR CmdTab[BX]
	MOV	BX,OFFSET DOSDATA:IOCALL; IOCALL is in DOSDATA
	MOV	[IOCALL.REQLEN],AH
	MOV	[IOCALL.REQFUNC],CH
 IFDEF  DBCS				;AN000;
;----------------------------- Start of DBCS 2/13/KK
	PUSH	CX			;AN000;
	MOV	CL, [InterCon]		;AN000;
	CMP	CH, DEVRD		;AN000; 0 input
	JZ	SETIN			;AN000;
	CMP	CH, DEVRDND		;AN000; 1(5) input status without(with) system WAIT
	JZ	SETIN			;AN000;
	MOV	CL, [SaveCurflg]	;AN000;
	CMP	CH, DEVWRT		;AN000; 2 output
	JZ	CHKERROUT		;AN000;
	XOR	CL,CL			;AN000; else, do normal
SETIN:					;AN000;
	MOV	BYTE PTR [IoMed], CL	;AN000; set interim I/O indication
	POP	CX			;AN000;
;----------------------------- End of DBCS 2/13/KK
 ENDIF					;AN000;
	MOV	DS,DX
ASSUME	DS:NOTHING
	CALL	DEVIOCALL
	MOV	DI,[IOCALL.REQSTAT]	; SS override
	.errnz	STERR-8000h
	and	di,di
	js	DevErr
OkDevIO:
	MOV	AX,SS
	MOV	DS,AX

	ASSUME	DS:DOSDATA

 IFDEF  DBCS			;AN000;
	MOV	[InterChar],0	;AN000; reset interim character flag  2/13/KK
	TEST	DI,Ddkey	;AN000; is this a dead key (interim char)? 2/13/KK
	JZ	NotInterim	;AN000; no, flag already reset...	   2/13/KK
	INC	[InterChar]	;AN000; yes, set flag for future	   2/13/KK
NotInterim:			;AN000; 				   2/13/KK
 ENDIF				;AN000;
	CMP	CH,DEVRDND
	JNZ	DNODRD
	MOV	AL,BYTE PTR [IORCHR]
	MOV	[DEVIOBUF],AL

DNODRD: MOV	AH,BYTE PTR [IOCALL.REQSTAT+1]
	NOT	AH			; Zero = busy, not zero = ready
	AND	AH,STBUI SHR 8

QuickReturn:				;AN000; 2/13/KK
	restore_world
ASSUME	DS:NOTHING

	; SR;
	; We return ax = -1 if the user failed on I24. This is the case if 
	; IoStatFail = -1 (set after return from the I24)
	;

	pushf
	mov	al,ss:IoStatFail	;assume fail error
	cbw				;sign extend to word
	cmp	ax,-1
	jne	not_fail_ret
	inc	ss:IoStatFail
	popf
	ret
not_fail_ret:

	MOV	AX,WORD PTR [DEVIOBUF]	;ss override
	popf
	return

;IOTOFILEJ:
;	JMP	SHORT IOTOFILE
 IFDEF  DBCS				;AN000;
;------------------------------ Start of DBCS 2/13/KK
CHKERROUT:				;AN000;
	MOV	DS, DX			;AN000;
	TESTB	[SI].SF_FLAGS, devid_device_con_out	;AN000; output to console ?
	JNZ	GOOD			;AN000; yes
	CMP	CL, 01			;AN000; write interim ?
	JNZ	GOOD			;AN000; no,
	POP	CX			;AN000;
	JMP	SHORT QuickReturn	;AN000; avoid writting interims to other than
					;AN000; console device
GOOD:					;AN000;
	PUSH	SS			;AN000;
	POP	DS			;AN000;
	JMP	SETIN			;AN000;
;------------------------------ End of DBCS 2/13/KK
 ENDIF					;AN000;
DevErr:
	MOV	AH,CL
	invoke	CHARHARD
	CMP	AL,1
	JNZ	NO_RETRY
	restore_world			;hkn; use macro
	JMP	IOFUNC

NO_RETRY:
	
	;
	; Know user must have wanted Ignore OR Fail.	Make sure device shows ready
	; ready so that DOS doesn't get caught in a status loop when user 
	; simply wants to ignore the error.
	;
	; SR; If fail wanted by user set ax to special value (ax = -1). This 
	; should be checked by the caller on return
	;

					; SS override
	AND	BYTE PTR [IOCALL.REQSTAT+1], NOT (STBUI SHR 8)

	; SR;
	; Check if user failed
	;

	cmp	al,3
	jnz	not_fail
	dec	IoStatFail		;set flag indicating fail on I24
not_fail:
	JMP	OKDevIO

IOTOFILE:
ASSUME	DS:NOTHING
	OR	AH,AH
	JZ	IOIN
	DEC	AH
	JZ	IOIST
	DEC	AH
	JZ	IOUT
	return				; NON ZERO FLAG FOR OUTPUT STATUS

IOIST:
	PUSH	WORD PTR [SI.sf_position]   ; Save position
	PUSH	WORD PTR [SI.sf_position+2]
	CALL	IOIN
	POP	WORD PTR [SI.sf_position+2] ; Restore position
	POP	WORD PTR [SI.sf_position]
	return

IOUT:
	CALL	SETXADDR
	invoke	DOS_WRITE
	CALL	RESTXADDR		; If you change this into a jmp don't
	return				; come crying to me when things don't
					; work ARR

IOIN:
	CALL	SETXADDR

					; SS override for DOS34_FLAG
	OR	[DOS34_FLAG],Disable_EOF_I24   ;AN000;
	invoke	DOS_READ
	AND	[DOS34_FLAG],NO_Disable_EOF_I24   ;AN000;
	OR	CX,CX			; Check EOF
	CALL	RESTXADDR

					; SS override
	MOV	AL,[DEVIOBUF]		; Get byte from trans addr
	retnz
	MOV	AL,1AH			; ^Z if no bytes
	return

SETXADDR:

					; SS override
	POP	WORD PTR [CALLSCNT]	; Return address

	save_world			;hkn; use macro

					; SS override for DMAADD and THISSFT
	PUSH	WORD PTR [DMAADD]	; Save Disk trans addr
	PUSH	WORD PTR [DMAADD+2]
	MOV	WORD PTR [THISSFT+2],DS

	Context DS
	MOV	WORD PTR [THISSFT],SI	; Finish setting SFT pointer
	MOV	CX,WORD PTR [IOXAD+2]
	MOV	WORD PTR [DMAADD+2],CX
	MOV	CX,WORD PTR [IOXAD]
	MOV	WORD PTR [DMAADD],CX	; Set byte trans addr
	MOV	CX,[IOSCNT]		; ioscnt specifies length of buffer
	JMP	SHORT RESTRET		; RETURN ADDRESS

RESTXADDR:
	DOSAssume   <DS>,"RestXAddr"
	POP	WORD PTR [CALLSCNT]	; Return address
	POP	WORD PTR [DMAADD+2]	; Restore Disk trans addr
	POP	WORD PTR [DMAADD]

	restore_world			; use macro

ASSUME	DS:NOTHING

					; SS override
RESTRET:JMP	WORD PTR [CALLSCNT]	; Return address
EndProc IOFUNC

Break <DEV_OPEN_SFT, DEV_CLOSE_SFT - OPEN or CLOSE A DEVICE>


;**	Dev_Open_SFT - Open the Device for an SFT
;
;	Dev_Open_SFT issues an open call to the device associated with
;	the SFT.
;
;	ENTRY	(ES:DI) = SFT
;	EXIT	none
;	USES	all

procedure   DEV_OPEN_SFT,NEAR

	Assert	ISSFT,<ES,DI>,"Dev_Open_SFT"

	Save_World			; use macro
	MOV	AL,DEVOPN
	JMP	SHORT DO_OPCLS

EndProc DEV_OPEN_SFT

;---------------------------------------------------------------------------
;
; Procedure Name : DEV_CLOSE_SFT
;
; Inputs:
;	ES:DI Points to SFT
; Function:
;	Issue a CLOSE call to the correct device
; Outputs:
;	None
; ALL preserved
;
;---------------------------------------------------------------------------

procedure   DEV_CLOSE_SFT,NEAR

	Assert	ISSFT,<ES,DI>,"Dev_Close_SFT"

	Save_World			; use macro

	MOV	AL,DEVCLS

	;
	; Main entry for device open and close.  AL contains the function 
	; requested. Subtlety:  if Sharing is NOT loaded then we do NOT issue 
	; open/close to block devices.  This allows networks to function but 
	; does NOT hang up with bogus change-line code.
	;

	entry	DO_OPCLS

	; Is the SFT for the net?  If so, no action necessary.

	TESTB	es:[di].sf_Flags, SF_ISNET
	JNZ	OPCLS_DONE		; NOP on net SFTs
	XOR	AH,AH			; Unit
	TESTB	ES:[DI].SF_FLAGS,devid_device
	LES	DI,ES:[DI.sf_devptr]	; Get DPB or device
	JNZ	Got_Dev_Addr

	; We are about to call device open/close on a block driver.  If no 
	; sharing then just short circuit to done.

;;;;;	invoke	CheckShare

					; SS override
	CMP	fshare,1		;AN010; /NC or no SHARE
	JBE	opCLs_Done		;AN010; yes
	MOV	AH,ES:[DI.dpb_UNIT]	; (ah) = unit
	MOV	CL,ES:[DI.dpb_drive]	; (cl) = drive
	LES	DI,ES:[DI.dpb_driver_addr]  ; Get device
GOT_DEV_ADDR:				; ES:DI -> device
	TESTB	ES:[DI.SDEVATT],DEVOPCL
	JZ	OPCLS_DONE		; Device can't
	PUSH	ES
	POP	DS
	MOV	SI,DI			; DS:SI -> device
OPCLS_RETRY:
	Context ES

					; DEVCALL is in DOSDATA
	MOV	DI,OFFSET DOSDATA:DEVCALL

	MOV	BX,DI
	PUSH	AX
	MOV	AL,DOPCLHL
	STOSB				; Length
	POP	AX
	XCHG	AH,AL
	STOSB				; Unit
	XCHG	AH,AL
	STOSB				; Command
	MOV	WORD PTR ES:[DI],0	; Status
	PUSH	AX			; Save Unit,Command
	invoke	DEVIOCALL2
	MOV	DI,ES:[BX.REQSTAT]
	.errnz	STERR-8000h
	and	di,di
	Jns	OPCLS_DONEP		; No error
	TESTB	[SI.SDEVATT],DEVTYP
	JZ	BLKDEV
	MOV	AH,86H			; Read error in data, Char dev
	JMP	SHORT HRDERR

BLKDEV:
	MOV	AL,CL			; Drive # in AL
	MOV	AH,6			; Read error in data, Blk dev
HRDERR:
	invoke	CHARHARD
	CMP	AL,1
	JNZ	OPCLS_DONEP		; IGNORE or FAIL
					;  Note that FAIL is essentually IGNORED
	POP	AX			; Get back Unit, Command
	JMP	OPCLS_RETRY

OPCLS_DONEP:
	POP	AX			; Clean stack
OPCLS_DONE:

	Restore_World			;hkn; use macro
	return

EndProc DEV_CLOSE_SFT

Break	<DEVIOCALL, DEVIOCALL2 - CALL A DEVICE>


;**	DevIoCall  - Call Device
;
;	ENTRY	DS:SI Points to device SFT
;		ES:BX Points to request data
;	EXIT	DS:SI -> Device driver
;	USES	DS:SI,AX

;**	DevIoCall2 - Call Device
;
;	ENTRY	DS:SI Points to DPB
;		ES:BX Points to request data
;	EXIT	DS:SI -> Device driver
;	USES	DS:SI,AX

procedure   DEVIOCALL,NEAR

					; SS override for CALLSSEC, 
					; CALLNEWSC, HIGH_SECTOR & CALLDEVAD
	Assert	ISSFT,<DS,SI>,"DevIOCall"
	LDS	SI,[SI.sf_devptr]

entry	DEVIOCALL2

	EnterCrit   critDevice

	TESTB	[SI.SDEVATT],DevTyp    ;AN000; >32mb   block device ?
	JNZ	chardev2	       ;AN000; >32mb   no
	CMP	ES:[BX.REQFUNC],DEVRD  ;AN000; >32mb   read ?
	JZ	chkext		       ;AN000; >32mb   yes
	CMP	ES:[BX.REQFUNC],DEVWRT ;AN000; >32mb   write ?
	JZ	chkext		       ;AN000; >32mb   yes
	CMP	ES:[BX.REQFUNC],DEVWRTV;AN000; >32mb   write/verify ?
	JNZ	chardev2	       ;AN000; >32mb   no
chkext:
	CALL	RW_SC		       ;AN000;LB. use secondary cache if there
	JC	dev_exit	       ;AN000;LB. done

	TESTB	[SI.SDEVATT],EXTDRVR   ;AN000;>32mb   extended driver?
	JZ	chksector	       ;AN000;>32mb   no
	ADD	BYTE PTR ES:[BX],8     ;AN000;>32mb   make length to 30
	MOV	AX,[CALLSSEC]	       ;AN000;>32mb
	MOV	[CALLSSEC],-1	       ;AN000;>32mb   old sector  =-1
	MOV	WORD PTR [CALLNEWSC],AX   ;AN000;>32mb	 new sector  =
	MOV	AX,[HIGH_SECTOR]       ;AN000; >32mb   low sector,high sector
	MOV	WORD PTR [CALLNEWSC+2],AX  ;AN000; >32mb
	JMP	short chardev2	       ;AN000; >32mb
chksector:			       ;AN000; >32mb
	CMP	[HIGH_SECTOR],0        ;AN000; >32mb   if >32mb
	JZ	chardev2	       ;AN000; >32mb   then fake error
	MOV	ES:[BX.REQSTAT],STERR+STDON+ERROR_I24_NOT_DOS_DISK  ;AN000; >32mb
	JMP	SHORT dev_exit	       ;AN000; >32mb

chardev2:			       ;AN000;

	;
	; As above only DS:SI points to device header on entry, and DS:SI is 
	; preserved
	;

	MOV	AX,[SI.SDEVSTRAT]
	MOV	WORD PTR [CALLDEVAD],AX
	MOV	WORD PTR [CALLDEVAD+2],DS
	CALL	DWORD PTR [CALLDEVAD]
	MOV	AX,[SI.SDEVINT]
	MOV	WORD PTR [CALLDEVAD],AX
	CALL	DWORD PTR [CALLDEVAD]
	CALL	VIRREAD 		;AN000;LB. move data from SC to buffer
	JC	chardev2		;AN000;LB. bad sector or exceeds max sec
dev_exit:
	LeaveCrit   critDevice
	return
EndProc DEVIOCALL

Break	<SETREAD, SETWRITE -- SET UP HEADER BLOCK>
;---------------------------------------------------------------------------
;
; Procedure Name : SETREAD, SETWRITE
;
; Inputs:
;	DS:BX = Transfer Address
;	CX = Record Count
;	DX = Starting Record
;	AH = Media Byte
;	AL = Unit Code
; Function:
;	Set up the device call header at DEVCALL
; Output:
;	ES:BX Points to DEVCALL
; No other registers effected
;
;---------------------------------------------------------------------------

procedure   SETREAD,NEAR

	PUSH	DI
	PUSH	CX
	PUSH	AX
	MOV	CL,DEVRD
SETCALLHEAD:
	MOV	AL,DRDWRHL
	PUSH	SS
	POP	ES

					; DEVCALL is in DOSDATA
	MOV	DI,OFFSET DOSDATA:DEVCALL

	STOSB				; length
	POP	AX
	STOSB				; Unit
	PUSH	AX
	MOV	AL,CL
	STOSB				; Command code
	XOR	AX,AX
	STOSW				; Status
	ADD	DI,8			; Skip link fields
	POP	AX
	XCHG	AH,AL
	STOSB				; Media byte
	XCHG	AL,AH
	PUSH	AX
	MOV	AX,BX
	STOSW
	MOV	AX,DS
	STOSW				; Transfer addr
	POP	CX			; Real AX
	POP	AX			; Real CX
	STOSW				; Count
	XCHG	AX,DX			; AX=Real DX, DX=real CX, CX=real AX
	STOSW				; Start
	XCHG	AX,CX
	XCHG	DX,CX
	POP	DI
					; DEVCALL is in DOSDATA
	MOV	BX,OFFSET DOSDATA:DEVCALL
	return

	entry	SETWRITE
ASSUME	DS:NOTHING,ES:NOTHING

; Inputs:
;	DS:BX = Transfer Address
;	CX = Record Count
;	DX = Starting Record
;	AH = Media Byte
;	AL = Unit Code
; Function:
;	Set up the device call header at DEVCALL
; Output:
;	ES:BX Points to DEVCALL
; No other registers effected

	PUSH	DI
	PUSH	CX
	PUSH	AX
	MOV	CL,DEVWRT
	ADD	CL,[VERFLG]		; SS override
	JMP	SHORT SETCALLHEAD
EndProc SETREAD


Break	<RW_SC -- Read Write Secondary Cache>
;---------------------------------------------------------------------------
;
; Procedure Name : RW_SC
;
; Inputs:
;	 [SC_CACHE_COUNT]= secondary cache count
;	 [SC_STATUS]= SC validity status
;	 [SEQ_SECTOR]= last sector read
; Function:
;	Read from or write through secondary cache
; Output:
;	ES:BX Points to DEVCALL
;	carry clear, I/O is not done
;		     [SC_FLAG]=1 if continuos sectors will be read
;	carry set, I/O is done
;
;----------------------------------------------------------------------------


procedure   RW_SC,NEAR		;AN000;

	; SS override for all variables used.

	CMP	[SC_CACHE_COUNT],0	;AN000;LB. secondary cache exists?
	JZ	scexit4 		;AN000;LB. no, do nothing
	CMP	[CALLSCNT],1		;AN000;LB. sector count = 1 (buffer I/O)
	JNZ	scexit4 		;AN000;LB. no, do nothing
	PUSH	CX			    ;AN000;;LB.
	PUSH	DX			    ;AN000;;LB. yes
	PUSH	DS			    ;AN000;;LB. save registers
	PUSH	SI			    ;AN000;;LB.
	PUSH	ES			    ;AN000;;LB.
	PUSH	DI			    ;AN000;;LB.
	MOV	DX,WORD PTR [CALLSSEC]	    ;AN000;;LB.  starting sector
	CMP	BYTE PTR [DEVCALL.REQFUNC],DEVRD ;AN000;LB. read ?		      ;AN000;
	JZ	doread			;AN000;LB. yes				      ;AN000;
	CALL	INVALIDATE_SC		    ;AN000;LB. invalidate SC		      ;AN000;
	JMP	scexit2 		    ;AN000;LB. back to normal		      ;AN000;
scexit4:				    ;AN000;				       ;AN000;
	CLC				    ;AN000;LB. I/O not done yet 	  ;AN000;
	return				    ;AN000;LB.				  ;AN000;
doread: 				    ;AN000;				       ;AN000;
	CALL	SC2BUF			    ;AN000;LB. check if in SC		      ;AN000;
	JC	readSC			    ;AN000;LB.					  ;AN000;
	MOV	[DEVCALL.REQSTAT],STDON     ;AN000;LB. fake done and ok 	  ;AN000;
	STC				    ;AN000;LB. set carry		  ;AN000;
	JMP	short saveseq 		    ;AN000;LB. save seq. sector #	  ;AN000;
readSC: 				    ;AN000;
	MOV	AX,WORD PTR [HIGH_SECTOR]   ;AN000;;LB. subtract sector num from
	MOV	CX,WORD PTR [CALLSSEC]	    ;AN000;;LB. saved sequential sector
	SUB	CX,WORD PTR [SEQ_SECTOR]    ;AN000;;LB. number
	SBB	AX,WORD PTR [SEQ_SECTOR+2]  ;AN000;;LB.
	CMP	AX,0			    ;AN000;;LB. greater than 64K
	JNZ	saveseq2		    ;AN000;;LB. yes,save seq. sector #
chklow: 									;AN000;
	CMP	CX,1			    ;AN000;;LB. <= 1
	JA	saveseq2		    ;AN000;;LB. no, not sequential
	MOV	[SC_STATUS],-1		    ;AN000;;LB. prsume all SC valid
	MOV	AX,[SC_CACHE_COUNT]	    ;AN000;;LB. yes, sequential
	MOV	[CALLSCNT],AX		    ;AN000;;LB. read continuous sectors
readsr:
	MOV	AX,WORD PTR [CALLXAD+2]     ;AN000;;LB. save buffer addr
	MOV	[TEMP_VAR2],AX		    ;AN000;;LB. in temp vars
	MOV	AX,WORD PTR [CALLXAD]	    ;AN000;;LB.
	MOV	[TEMP_VAR],AX		    ;AN000;;LB.
										;AN000;
	MOV	AX,WORD PTR [SC_CACHE_PTR]  ;AN000;LB. use SC cache addr as	      ;AN000;
	MOV	WORD PTR [CALLXAD],AX	    ;AN000;LB. transfer addr		      ;AN000;
	MOV	AX,WORD PTR [SC_CACHE_PTR+2]  ;AN000;LB.			      ;AN000;
	MOV	WORD PTR [CALLXAD+2],AX       ;AN000;LB.			      ;AN000;
	MOV	[SC_FLAG],1		      ;AN000;LB. flag it for later	      ;AN000;
	MOV	AL,[SC_DRIVE]		    ;AN000;;LB. current drive
	MOV	[CURSC_DRIVE],AL	    ;AN000;;LB. set current drive
	MOV	AX,WORD PTR [CALLSSEC]	    ;AN000;;LB. current sector
	MOV	[CURSC_SECTOR],AX	    ;AN000;;LB. set current sector
	MOV	AX,WORD PTR [HIGH_SECTOR]   ;AN000;;LB.
	MOV	[CURSC_SECTOR+2],AX	    ;AN000;;LB.
saveseq2:				    ;AN000;
	CLC				    ;AN000;LB. clear carry		      ;AN000;
saveseq:				    ;AN000;				       ;AN000;
	MOV	AX,[HIGH_SECTOR]	    ;AN000;LB. save current sector #	  ;AN000;
	MOV	WORD PTR [SEQ_SECTOR+2],AX  ;AN000;LB. for access mode ref.	  ;AN000;
	MOV	AX,[CALLSSEC]		    ;AN000;LB.				  ;AN000;
	MOV	WORD PTR [SEQ_SECTOR],AX    ;AN000;LB.				  ;AN000;
	JMP	short scexit 		    ;AN000;LB.				  ;AN000;
										;AN000;
scexit2:				    ;AN000;LB.				      ;AN000;
	CLC				    ;AN000;LB.	       clear carry	      ;AN000;
scexit: 				    ;AN000;				       ;AN000;
	POP	DI			    ;AN000;;LB.
	POP	ES			    ;AN000;;LB. restore registers
	POP	SI			    ;AN000;;LB.
	POP	DS			    ;AN000;;LB.
	POP	DX			    ;AN000;;LB.
	POP	CX			    ;AN000;;LB.
	return				    ;AN000;;LB.
										;AN000;
EndProc RW_SC				    ;AN000;

Break	<IN_SC -- check if in secondary cache>
;--------------------------------------------------------------------------
;
; Procedure Name : IN_SC
;
; Inputs:  [SC_DRIVE]= requesting drive
;	   [CURSC_DRIVE]= current SC drive
;	   [CURSC_SECTOR] = starting scetor # of SC
;	   [SC_CACHE_COUNT] = SC count
;	   [HIGH_SECTOR]:DX= sector number
; Function:
;	Check if the sector is in secondary cache
; Output:
;	carry clear, in SC
;	   CX= the index in the secondary cache
;	carry set, not in SC
;
;---------------------------------------------------------------------------

procedure   IN_SC,NEAR		    ;AN000;

	; SS override for all variables used

	MOV	AL,[SC_DRIVE]		    ;AN000;;LB. current drive
	CMP	AL,[CURSC_DRIVE]	    ;AN000;;LB. same as SC drive
	JNZ	outrange2		    ;AN000;;LB. no
	MOV	AX,WORD PTR [HIGH_SECTOR]   ;AN000;;LB. subtract sector num from
	MOV	CX,DX			    ;AN000;;LB. secondary starting sector
	SUB	CX,WORD PTR [CURSC_SECTOR]    ;AN000;;LB. number
	SBB	AX,WORD PTR [CURSC_SECTOR+2]  ;AN000;;LB.
	CMP	AX,0			    ;AN000;;LB. greater than 64K
	JNZ	outrange2		    ;AN000;;LB. yes
	CMP	CX,[SC_CACHE_COUNT]	    ;AN000;;LB. greater than SC count
	JAE	outrange2		    ;AN000;;LB. yes
	CLC				    ;AN000;;LB. clear carry
	JMP	short inexit		    ;AN000;;LB. in SC
outrange2:				    ;AN000;;LB. set carry
	STC				    ;AN000;;LB.
inexit: 				    ;AN000;;LB.
	return				    ;AN000;;LB.

EndProc IN_SC				    ;AN000;

Break	<INVALIDATE_SC - invalide secondary cache>
;---------------------------------------------------------------------------
;
; Procedure Name : Invalidate_Sc
;
; Inputs:  [SC_DRIVE]= requesting drive
;	   [CURSC_DRIVE]= current SC drive
;	   [CURSC_SECTOR] = starting scetor # of SC
;	   [SC_CACHE_COUNT] = SC count
;	   [SC_STAUS] = SC status word
;	   [HIGH_SECTOR]:DX= sceotor number
;
; Function:
;	invalidate secondary cache if in there
; Output:
;	[SC_STATUS] is updated
;---------------------------------------------------------------------------

procedure   INVALIDATE_SC,NEAR	    ;AN000;

	; SS override for all variables used

	CALL	IN_SC			    ;AN000;;LB. in secondary cache
	JC	outrange		    ;AN000;;LB. no
	MOV	AX,1			    ;AN000;;LB. invalidate the sector
	SHL	AX,CL			    ;AN000;;LB. in the secondary cache
	NOT	AX			    ;AN000;;LB.
	AND	[SC_STATUS],AX		    ;AN000;;LB. save the status
outrange:				    ;AN000;;LB.
	return				    ;AN000;;LB.

EndProc INVALIDATE_SC			    ;AN000;


Break	<VIRREAD- virtually read data into buffer>
;--------------------------------------------------------------------------
;
; Procedure Name : SC_FLAG
;
; Inputs:  SC_FLAG = 0 , no sectors were read into SC
;		     1, continous sectors were read into SC
; Function:
;	   Move data from SC to buffer
; Output:
;	 carry clear, data is moved to buffer
;	 carry set, bad sector or exceeds maximum sector
;	   SC_FLAG =0
;	   CALLSCNT=1
;	   SC_STATUS= -1 if succeeded
;     
;		       0 if failed
;--------------------------------------------------------------------------

procedure   VIRREAD,NEAR	    ;AN000;

	; SS override for all variables used

	CMP	[SC_FLAG],0		    ;AN000;;LB.  from SC fill
	JZ	sc2end			    ;AN000;;LB.  no
	MOV	AX,[TEMP_VAR2]		    ;AN000;;LB. restore buffer addr
	MOV	WORD PTR [CALLXAD+2],AX     ;AN000;;LB.
	MOV	AX,[TEMP_VAR]		    ;AN000;;LB.
	MOV	WORD PTR [CALLXAD],AX	    ;AN000;;LB.
	MOV	[SC_FLAG],0		    ;AN000;;LB.  reset sc_flag
	MOV	[CALLSCNT],1		    ;AN000;;LB.  one sector transferred

	TESTB	[DEVCALL.REQSTAT],STERR     ;AN000;;LB.  error?
	JNZ	scerror 		    ;AN000;;LB. yes
	PUSH	DS			    ;AN000;;LB.
	PUSH	SI			    ;AN000;;LB.
	PUSH	ES			    ;AN000;;LB.
	PUSH	DI			    ;AN000;;LB.
	PUSH	DX			    ;AN000;;LB.
	PUSH	CX			    ;AN000;;LB.
	XOR	CX,CX			    ;AN000;;LB. we want first sector in SC
	CALL	SC2BUF2 		    ;AN000;;LB. move data from SC to buffer
	POP	CX			    ;AN000;;LB.
	POP	DX			    ;AN000;;LB.
	POP	DI			    ;AN000;;LB.
	POP	ES			    ;AN000;;LB.
	POP	SI			    ;AN000;;LB.
	POP	DS			    ;AN000;;LB.
	JMP	SHORT sc2end		    ;AN000;;LB. return

scerror:				    ;AN000;
	MOV	[CALLSCNT],1		    ;AN000;;LB. reset sector count to 1
	MOV	[SC_STATUS],0		    ;AN000;;LB. invalidate all SC sectors
	MOV	[CURSC_DRIVE],-1	    ;AN000;;LB. invalidate drive
	STC				    ;AN000;;LB. carry set
	return				    ;AN000;;LB.

sc2end: 				    ;AN000;
	CLC				    ;AN000;;LB. carry clear
	return				    ;AN000;;LB.

EndProc VIRREAD 			    ;AN000;

Break	<SC2BUF- move data from SC to buffer>
;----------------------------------------------------------------------------
;
; Procedure Name : SC2BUF
;
; Inputs:  [SC_STATUS] = SC validity status
;	   [SC_SECTOR_SIZE] = request sector size
;	   [SC_CACHE_PTR] = pointer to SC
; Function:
;	   Move data from SC to buffer
; Output:
;	   carry clear, in SC  and data is moved
;	   carry set, not in SC and data is not moved
;---------------------------------------------------------------------------

procedure   SC2BUF,NEAR 	    ;AN000;

	; SS override for all variables used

	CALL	IN_SC			    ;AN000;;LB. in secondary cache
	JC	noSC			    ;AN000;;LB. no
	MOV	AX,1			    ;AN000;;LB. check if valid sector
	SHL	AX,CL			    ;AN000;;LB. in the secondary cache
	TEST	[SC_STATUS],AX		    ;AN000;;LB.
	JZ	noSC			    ;AN000;;LB. invalid
entry SC2BUF2				    ;AN000;
	MOV	AX,CX			    ;AN000;;LB. times index with
	MUL	[SC_SECTOR_SIZE]	    ;AN000;;LB. sector size
	ADD	AX,WORD PTR [SC_CACHE_PTR]  ;AN000;;LB. add SC starting addr
	ADC	DX,WORD PTR [SC_CACHE_PTR+2];AN000;;LB.
	MOV	DS,DX			    ;AN000;    ;LB. DS:SI-> SC sector addr
	MOV	SI,AX			    ;AN000;    ;LB.
	MOV	ES,WORD PTR [CALLXAD+2]     ;AN000;    ;LB. ES:DI-> buffer addr
	MOV	DI,WORD PTR [CALLXAD]	    ;AN000;    ;LB.
	MOV	CX,[SC_SECTOR_SIZE]	    ;AN000;    ;LB. count= sector size
	SHR	CX,1			    ;AN000;    ;LB. may use DWORD move for 386
entry MOVWORDS				    ;AN000;
	CMP	[DDMOVE],0		    ;AN000;    ;LB. 386 ?
	JZ	nodd			    ;AN000;    ;LB. no
	SHR	CX,1			    ;AN000;    ;LB. words/2
	DB	66H			    ;AN000;    ;LB. use double word move
nodd:
	REP	MOVSW			    ;AN000;    ;LB. move to buffer
	CLC				    ;AN000;    ;LB. clear carry
	return				    ;AN000;    ;LB. exit
noSC:					    ;AN000;
	STC				    ;AN000;    ;LB. set carry
sexit:					    ;AN000;
	return				    ;AN000;    ;LB.

EndProc SC2BUF
DOSCODE	ENDS
	END

