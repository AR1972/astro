	TITLE	DISK2 - Disk utility routines
	NAME	Disk2

;
;	Microsoft Confidential
;	Copyright (C) Microsoft Corporation 1991
;	All Rights Reserved.
;

;**	Low level Read and write routines for local SFT I/O on files and devs
;
;	DskRead
;	DWRITE
;	DSKWRITE
;	HarderrRW
;	SETUP
;	BREAKDOWN
;	READ_LOCK_VIOLATION
;	WRITE_LOCK_VIOLATION
;	DISKREAD
;	SET_ACC_ERR_DS
;	SET_ACC_ERR
;	SETSFT
;	SETCLUS
;	AddRec
;
;	Revision history:
;
;		AN000 version 4.00 Jan. 1988
;		M039 DB 10/17/90 - Disk read/write optimization

	extrn	GetCurHead:near
	extrn	ScanPlace:near

	.xlist
	.xcref
	include version.inc
	include dosseg.inc
	INCLUDE DOSSYM.INC
	INCLUDE DEVSYM.INC
	include sf.inc
	include dpb.inc
	include filemode.inc
        include dosmac.inc              ;M039
	.cref
	.list

Installed = TRUE

	i_need	THISSFT,DWORD
	i_need	DMAADD,DWORD
	i_need	NEXTADD,WORD
	i_need	ThisDrv,BYTE
	i_need	SecClusPos,BYTE
	i_need	ClusNum,WORD
	i_need	ReadOp,BYTE
	i_need	Trans,BYTE
	i_need	BytPos,4
	i_need	SecPos,DWORD		 ; DOS 4.00 >32mb			;AN000;
	i_need	BytSecPos,WORD
	i_need	BytCnt1,WORD
	i_need	BytCnt2,WORD
	i_need	SecCnt,WORD
	i_need	ThisDPB,DWORD
	i_need	LastPos,WORD
	i_need	EXTERRPT,DWORD
	i_need	CALLVIDRW,DWORD
	i_need	ALLOWED,BYTE
	i_need	DEVCALL,BYTE
	i_need	CALLSCNT,WORD

;**RMFHFE**	i_need	DISK_FULL,BYTE		  ; disk full flag for ran blk wrt

	i_need	HIGH_SECTOR,WORD	  ; F.C. >32mb	  ;AN000;
	i_need	TEMP_VAR2,WORD		  ; LB. 	  ;AN000;
	i_need	TEMP_VAR,WORD		  ; LB. 	  ;AN000;
	i_need	DirtyBufferCount,WORD
	i_need	FIRST_BUFF_ADDR,WORD	     ; first buffer address		;AN000;


DOSCODE	Segment
	ASSUME	SS:DOSDATA, CS:DOSCODE


Break	<DSKREAD -- PHYSICAL DISK READ>
;---------------------------------------------------------------------------
;
; Procedure Name : DSKREAD
;
; Inputs:
;	DS:BX = Transfer addr
;	CX = Number of sectors
;	[HIGH_SECTOR] = Absolute record number (HIGH)
;	DX = Absolute record number	       (LOW)
;	ES:BP = Base of drive parameters
; Function:
;	Call BIOS to perform disk read
; Outputs:
;	DI = CX on entry
;	CX = Number of sectors unsuccessfully transfered
;	AX = Status word as returned by BIOS (error code in AL if error)
;	Zero set if OK (from BIOS) (carry clear)
;	Zero clear if error (carry clear)
; SI Destroyed, others preserved
;---------------------------------------------------------------------------

procedure   DskRead,NEAR

	Assert	ISDPB,<ES,BP>,"DskRead"
	PUSH	CX
	MOV	AH,ES:[BP.dpb_media]
	MOV	AL,ES:[BP.dpb_UNIT]
	PUSH	BX
	PUSH	ES
	invoke	SETREAD
	JMP	short DODSKOP

Break	<DWRITE -- SEE ABOUT WRITING>
;--------------------------------------------------------------------------
;
; Procedure Name : DWRITE
;
; Inputs:
;	DS:BX = Transfer address
;	CX = Number of sectors
;	[HIGH_SECTOR] = Absolute record number (HIGH)
;	DX = Absolute record number	       (LOW)
;	ES:BP = Base of drive parameters
;	[ALLOWED] must be set in case HARDERR called
; Function:
;	Calls BIOS to perform disk write. If BIOS reports
;	errors, will call HARDERRRW for further action.
; Output:
;	Carry set if error (currently, user FAILed to I 24)
; BP preserved. All other registers destroyed.
;----------------------------------------------------------------------------


	entry	DWRITE
ASSUME	DS:NOTHING,ES:NOTHING

	Assert	ISDPB,<ES,BP>,"DWrite"
	CALL	DSKWRITE
	jz	ret_label	; Carry clear (retz)

;hkn; SS override
	MOV	BYTE PTR [READOP],1
	invoke	HARDERRRW
	CMP	AL,1		; Check for retry
	JZ	DWRITE
	CMP	AL,3		; Check for FAIL
	CLC
	JNZ	NO_CAR2 	; Ignore
	STC
NO_CAR2:

ret_label:
	return

Break	<DSKWRITE -- PHYSICAL DISK WRITE>
;---------------------------------------------------------------------------
;
; Procedure Name : DSKWRITE
;
; Inputs:
;	DS:BX = Transfer addr
;	CX = Number of sectors
;	DX = Absolute record number	       (LOW)
;	[HIGH_SECTOR] = Absolute record number (HIGH)
;	ES:BP = Base of drive parameters
; Function:
;	Call BIOS to perform disk read
; Outputs:
;	DI = CX on entry
;	CX = Number of sectors unsuccessfully transfered
;	AX = Status word as returned by BIOS (error code in AL if error)
;	Zero set if OK (from BIOS) (carry clear)
;	Zero clear if error (carry clear)
; SI Destroyed, others preserved
;
;----------------------------------------------------------------------------

	entry	DSKWRITE
ASSUME	DS:NOTHING,ES:NOTHING

	Assert	ISDPB,<ES,BP>,"DskWrite"
	PUSH	CX
	MOV	AH,ES:[BP.dpb_media]
	MOV	AL,ES:[BP.dpb_UNIT]
	PUSH	BX
	PUSH	ES
	invoke	SETWRITE
DODSKOP:
	MOV	CX,DS		; Save DS
	POP	DS		; DS:BP points to DPB
	PUSH	DS
	LDS	SI,DS:[BP.dpb_driver_addr]
	invoke	DEVIOCALL2
	MOV	DS,CX		; Restore DS
	POP	ES		; Restore ES
	POP	BX

;hkn; SS override
	MOV	CX,[CALLSCNT]	; Number of sectors transferred
	POP	DI
	SUB	CX,DI
	NEG	CX		; Number of sectors not transferred

;hkn; SS override
	MOV	AX,[DEVCALL.REQSTAT]
	test	AX,STERR
	return
EndProc DskRead



Break	<HardErrRW - map extended errors and call harderr>
;---------------------------------------------------------------------------
;
; Procedure Name : HardErrRW
;
; Inputs:
;	AX is error code from read or write
;	Other registers set as per HARDERR
; Function:
;	Checks the error code for special extended
;	errors and maps them if needed. Then invokes
;	Harderr
; Outputs:
;	Of HARDERR
; AX may be modified prior to call to HARDERR.
; No other registers altered.
;
;---------------------------------------------------------------------------

procedure   HARDERRRW,near

	CMP	AL,error_I24_wrong_disk
	JNZ	DO_ERR				; Nothing to do

	push	ax
	mov	ax, word ptr callvidrw		; get ptr lo	;smr;SS Override
	mov	word ptr exterrpt, ax		; set ext err ptr lo
	mov	ax, word ptr callvidrw+2	; get ptr hi from dev
	mov	word ptr exterrpt+2, ax		; set ext err ptr hi
	pop	ax

;	PUSH	DS
;	PUSH	SI

;hkn; SS override for CALLVIDRW & EXTERRPT
;	LDS	SI,[CALLVIDRW]			; Get pointer from dev
;	MOV	WORD PTR [EXTERRPT+2],DS	; Set ext err pointer
;	MOV	WORD PTR [EXTERRPT],SI
;	POP	SI
;	POP	DS
DO_ERR:
	invoke	HARDERR
	return

EndProc HARDERRRW

Break	<SETUP -- SETUP A DISK READ OR WRITE FROM USER>
;----------------------------------------------------------------------------
;
; Procedure Name : SETUP
;
; Inputs:
;	ES:DI point to SFT (value also in THISSFT)
;	DMAAdd contains transfer address
;	CX = Byte count
;	DS = DOSDATA
;   WARNING Stack must be clean, two ret addrs on stack, 1st of caller,
;		2nd of caller of caller.
; Outputs:
;	    CX = byte count
;	    [THISDPB] = Base of drive parameters if file
;		      = Pointer to device header if device or NET
;	    ES:DI Points to SFT
;	    [NEXTADD] = Displacement of disk transfer within segment
;	    [TRANS] = 0 (No transfers yet)
;	    BytPos = Byte position in file
;
;	The following fields are relevant to local files (not devices) only:
;
;	    SecPos = Position of first sector (local files only)
;	    [BYTSECPOS] = Byte position in first sector (local files only)
;	    [CLUSNUM] = First cluster (local files only)
;	    [SECCLUSPOS] = Sector within first cluster (local files only)
;	    [THISDRV] = Physical unit number (local files only)
;
;      RETURNS ONE LEVEL UP WITH:
;	   CX = 0
;	   CARRY = Clear
;	IF AN ERROR IS DETECTED
; All other registers destroyed
;----------------------------------------------------------------------------

;hkn; called from disk.asm. DS has been set up to DOSDATA.

procedure   SETUP,NEAR
	DOSAssume   <DS>,"SetUp"

	Assert	    ISSFT,<ES,DI>,"SetUp"
	LDS	SI,ES:[DI.sf_devptr]
ASSUME	DS:NOTHING

;hkn; SS override
	MOV	WORD PTR [THISDPB+2],DS

;hkn; SS is DOSDATA
	context DS

	MOV	WORD PTR [THISDPB],SI
	MOV	BX,WORD PTR DMAAdd
	MOV	[NEXTADD],BX		;Set NEXTADD to start of Xaddr
	MOV	BYTE PTR [TRANS],0	;No transferes
	MOV	AX,WORD PTR ES:[DI.sf_Position]
	MOV	DX,WORD PTR ES:[DI.sf_Position+2]
	MOV	WORD PTR [BYTPOS+2],DX	;Set it
	MOV	WORD PTR BytPos,AX
	TESTB	ES:[DI].SF_FLAGS,<sf_isnet + devid_device>
	JNZ	NOSETSTUFF		;Following not done on devs or NET
	PUSH	ES
	LES	BP,[THISDPB]		;Point at the DPB
	Assert	ISDPB,<ES,BP>,"Setup"
	MOV	BL,ES:[BP.dpb_drive]
	MOV	[THISDRV],BL		;Set THISDRV
	MOV	BX,ES:[BP.dpb_sector_size]

;M039: Optimized this section.
        PUSH    CX                            ;SHR32 and DIV32 use CX.
	invoke	DIV32			      ;DX:AX/BX = CX:AX + DX (rem)
	MOV	[BYTSECPOS],DX
	MOV	WORD PTR SecPos,AX
	MOV	WORD PTR SecPos+2,CX
	MOV	DX,CX

	MOV	BX,AX
	AND	BL,ES:[BP.dpb_cluster_mask]
	MOV	[SECCLUSPOS],BL

	invoke	SHR32			     ;(DX:AX SHR dpb_cluster_shift)
	POP	CX			     ;CX = byte count.
	JNZ	EOFERR			     ;cluster number above 64k
	CMP	AX,ES:[BP.dpb_max_cluster]   ;>32mb    if > disk size ;AN000;	;AN000;
	JA	EOFERR			     ;>32mb    then EOF     ;AN000;	;AN000;

	MOV	[CLUSNUM],AX
	POP	ES			     ; ES:DI point to SFT
;M039

NOSETSTUFF:
	MOV	AX,CX		; AX = Byte count.
	ADD	AX,WORD PTR DMAAdd	 ; See if it will fit in one segment
	JNC	OK		; Must be less than 64K
	MOV	AX,WORD PTR DMAAdd
	NEG	AX		; Amount of room left in segment (know
				;    less than 64K since max value of CX
				;    is FFFF).
	JNZ	NoDec
	DEC	AX
NoDec:
	MOV	CX,AX		; Can do this much
	JCXZ	NOROOM		; Silly user gave Xaddr of FFFF in segment
OK:
	return

EOFERR:
	POP	ES		; ES:DI point to SFT
	XOR	CX,CX		; No bytes read
;;;;;;;;;;; 7/18/86
;	MOV	BYTE PTR [DISK_FULL],1	    ; set disk full flag
;;;;;;;;;;;
NOROOM:
	POP	BX		; Kill return address
	CLC
	return			; RETURN TO CALLER OF CALLER
EndProc SETUP

Break	<BREAKDOWN -- CUT A USER READ OR WRITE INTO PIECES>
;---------------------------------------------------------------------------
;
; Procedure Name : BREAKDOWN
;
; Inputs:
;	CX = Length of disk transfer in bytes
;	ES:BP = Base of drive parameters
;	[BYTSECPOS] = Byte position witin first sector
;	DS = DOSDATA
; Outputs:
;	[BYTCNT1] = Bytes to transfer in first sector
;	[SECCNT] = No. of whole sectors to transfer
;	[BYTCNT2] = Bytes to transfer in last sector
; AX, BX, DX destroyed. No other registers affected.
;---------------------------------------------------------------------------

procedure   BREAKDOWN,near
	DOSAssume   <DS>,"BreakDown"

	Assert	    ISDPB,<ES,BP>,"BreakDown"
	MOV	AX,[BYTSECPOS]
	MOV	BX,CX
	OR	AX,AX
	JZ	SAVFIR		; Partial first sector?
	SUB	AX,ES:[BP.dpb_sector_size]
	NEG	AX		; Max number of bytes left in first sector
	SUB	BX,AX		; Subtract from total length
	JAE	SAVFIR
	ADD	AX,BX		; Don't use all of the rest of the sector
	XOR	BX,BX		; And no bytes are left
SAVFIR:
	MOV	[BYTCNT1],AX
	MOV	AX,BX
	XOR	DX,DX
	DIV	ES:[BP.dpb_sector_size]  ; How many whole sectors?
	MOV	[SECCNT],AX
	MOV	[BYTCNT2],DX	; Bytes remaining for last sector
;	OR	DX,[BYTCNT1]	; SMR ONESECTORFIX BUGBUG
;	retnz			; NOT (BYTCNT1 = BYTCNT2 = 0)
;	CMP	AX,1
;	retnz
;	MOV	AX,ES:[BP.dpb_sector_size]	 ; Buffer EXACT one sector I/O
;	MOV	[BYTCNT2],AX
;	MOV	[SECCNT],DX		; DX = 0
;RET45:
	return
EndProc BreakDown

;----------------------------------------------------------------------------
;
; Procedure Name : READ_LOCK_VIOLATION
;
; ES:DI points to SFT. This entry used by NET_READ
; Carry set if to return error (CX=0,AX=error_sharing_violation).
; Else do retrys.
; ES:DI,DS,CX preserved
;
;----------------------------------------------------------------------------

procedure READ_LOCK_VIOLATION,NEAR
	DOSAssume   <DS>,"Read_Lock_Viol"

	Assert	    ISSFT,<ES,DI>,"ReadLockViolation"

	MOV	[READOP],0
ERR_ON_CHECK:
	TESTB	ES:[DI.sf_mode],sf_isfcb
	JNZ	HARD_ERR
	PUSH	CX
	MOV	CL,BYTE PTR ES:[DI.sf_mode]
	AND	CL,sharing_mask
	CMP	CL,sharing_compat
	POP	CX
	JNE	NO_HARD_ERR
HARD_ERR:
	invoke	LOCK_VIOLATION
	retnc				; User wants Retrys
NO_HARD_ERR:
	XOR	CX,CX			;No bytes transferred
	MOV	AX,error_lock_violation
	STC
	return

EndProc READ_LOCK_VIOLATION

;----------------------------------------------------------------------------
;
; Procedure Name : WRITE_LOCK_VIOLATION
;
; Same as READ_LOCK_VIOLATION except for READOP.
; This entry used by NET_WRITE
;
;----------------------------------------------------------------------------

procedure WRITE_LOCK_VIOLATION,NEAR
	DOSAssume   <DS>,"Write_Lock_Viol"
	Assert	    ISSFT,<ES,DI>,"WriteLockViolation"

	MOV	[READOP],1
	JMP	ERR_ON_CHECK

EndProc WRITE_LOCK_VIOLATION


Break	<DISKREAD -- PERFORM USER DISK READ>
;----------------------------------------------------------------------------
;
; Procedure Name : DISKREAD
;
; Inputs:
;	Outputs of SETUP
; Function:
;	Perform disk read
; Outputs:
;    Carry clear
;	CX = No. of bytes read
;	ES:DI point to SFT
;	SFT offset and cluster pointers updated
;    Carry set
;	CX = 0
;	ES:DI point to SFT
;	AX has error code
;----------------------------------------------------------------------------

;hkn; called from disk.asm. DS already set up.

procedure   DISKREAD,NEAR
	DOSAssume   <DS>,"DiskRead"

	Assert	ISSFT,<ES,DI>,"DISKREAD"
	MOV	AX,WORD PTR ES:[DI.sf_size]
	MOV	BX,WORD PTR ES:[DI.sf_size+2]
	SUB	AX,WORD PTR BytPos
	SBB	BX,WORD PTR [BYTPOS+2]
	JB	RDERR			;Read starts past EOF
	JNZ	ENUF			;More than 64k to EOF
	OR	AX,AX
	JZ	RDERR			;Read starts at EOF
	CMP	AX,CX
	JAE	ENUF			;I/O fits
	MOV	CX,AX			;Limit read to up til EOF
ENUF:
	invoke	CHECK_READ_LOCK 	;IFS. check read lock			 ;AN000;
	JNC	Read_Ok 		; There are no locks
	return

READ_OK:
	LES	BP,[THISDPB]
	Assert	ISDPB,<ES,BP>,"DISKREAD/ReadOK"


	CALL	BREAKDOWN
	MOV	CX,[CLUSNUM]
	invoke	FNDCLUS
                                        ;M022 conditional removed here
	JC	SET_ACC_ERR_DS		; fix to take care of I24 fail
					; migrated from 330a - HKN
	OR	CX,CX
	JZ	SKIPERR
RDERR:

;**RMFHFE**	MOV	[DISK_FULL],1		;MS. EOF detection  ;AN000;

	MOV	AH,0EH			;MS. read/data/fail ;AN000;
	transfer WRTERR22

;RDLASTJ: JMP	RDLAST                  ;M039

SETSFTJ2: JMP	SETSFT

CANOT_READ:
;	POP	CX		;M039.
	POP	CX              ;Clean stack.
	POP	BX

	entry	SET_ACC_ERR_DS
ASSUME	DS:NOTHING,ES:NOTHING

;hkn; SS is DOSDATA
	Context DS

entry	SET_ACC_ERR
	DOSAssume   <DS>,"SET_ACC_ERR"

	XOR	CX,CX
	MOV	AX,error_access_denied
	STC
	return

SKIPERR:
	MOV	[LASTPOS],DX
	MOV	[CLUSNUM],BX
	CMP	[BYTCNT1],0
	JZ	RDMID
	invoke	BUFRD
	JC	SET_ACC_ERR_DS
RDMID:
	CMP	[SECCNT],0
	JZ	RDLAST
	invoke	NEXTSEC
	JC	SETSFTJ2
	MOV	BYTE PTR [TRANS],1	; A transfer is taking place
ONSEC:
	MOV	DL,[SECCLUSPOS]
	MOV	CX,[SECCNT]
	MOV	BX,[CLUSNUM]
RDLP:
	invoke	OPTIMIZE
	JC	SET_ACC_ERR_DS
	PUSH	DI                      ;DI = Next physical cluster.
	PUSH	AX                      ;AX = # of sectors remaining.
        PUSH	BX			;[DMAADD+2]:BX = Transfer address.
	MOV	[ALLOWED],allowed_RETRY + allowed_FAIL + allowed_IGNORE
	MOV	DS,WORD PTR DmaAdd+2
ASSUME	DS:NOTHING
	PUSH	DX                      ;[HIGH_SECTOR]:DX = phys. sector #.
	PUSH	CX                      ;CX = # of contiguous sectors to read.
	invoke	SET_RQ_SC_PARMS 	 ;LB. do this for SC		       ;AN000;

	invoke	DREAD

;M039
        pop     cx
        pop     dx
        pop     WORD PTR [TEMP_VAR]
	jc      CANOT_READ

        mov     WORD PTR [TEMP_VAR2],ds

;       CX = # of contiguous sectors read.  (These constitute a block of
;            sectors, also termed an "Extent".)
;       [HIGH_SECTOR]:DX = physical sector # of first sector in extent.
;       [TEMP_VAR2]:[TEMP_VAR] = Transfer address (destination data address).
;       ES:BP -> Drive Parameter Block (DPB).
;
;	The Buffer Queue must now be scanned: the contents of any dirty
;	buffers must be "read" into the transfer memory block, so that the
;       transfer memory reflects the most recent data.

        call    DskRdBufScan

	Context DS
        pop     cx
        pop     bx

;       CX = # of sector remaining.
;       BX = Next physical cluster.
;M039

	JCXZ	RDLAST
	invoke	IsEOF			; test for eof on fat size
	JAE	SETSFT
	MOV	DL,0
	INC	[LASTPOS]		; We'll be using next cluster
	JMP	RDLP

RDLAST:
	MOV	AX,[BYTCNT2]
	OR	AX,AX
	JZ	SETSFT
	MOV	[BYTCNT1],AX
	invoke	NEXTSEC
	JC	SETSFT
	MOV	[BYTSECPOS],0
	invoke	BUFRD
	JNC	SETSFT
	JMP	SET_ACC_ERR_DS

;------------------------------------------------------------------------------
;
; Procedure Name : SETSFT
; Inputs:
;	[NEXTADD],[CLUSNUM],[LASTPOS] set to determine transfer size
;		and set cluster fields
; Function:
;	Update [THISSFT] based on the transfer
; Outputs:
;	sf_position, sf_lstclus, and sf_cluspos updated
;	ES:DI points to [THISSFT]
;	CX No. of bytes transferred
;	Carry clear
;
;----------------------------------------------------------------------------

entry	SETSFT
	DOSAssume   <DS>,"SetSFT"
	ASSUME	ES:NOTHING

	LES	DI,[THISSFT]

; Same as SETSFT except ES:DI already points to SFT
	entry	SETCLUS
	DOSAssume   <DS>,"SetClus"
	ASSUME	ES:NOTHING

	Assert	ISSFT,<ES,DI>,"SetClus"
	MOV	CX,[NEXTADD]
	SUB	CX,WORD PTR DMAAdd	 ; Number of bytes transfered
	TESTB	ES:[DI].SF_FLAGS,devid_device
	JNZ	ADDREC			 ; don't set clusters if device
	MOV	AX,[CLUSNUM]
	MOV	ES:[DI.sf_lstclus],AX
	MOV	AX,[LASTPOS]
	MOV	ES:[DI.sf_cluspos],AX

;----------------------------------------------------------------------------
;
; Procedure : AddRec
; Inputs:
;	ES:DI points to SFT
;	CX is No. Bytes transferred
; Function:
;	Update the SFT offset based on the transfer
; Outputs:
;	sf_position updated to point to first byte after transfer
;	ES:DI points to SFT
;	CX No. of bytes transferred
;	Carry clear
;----------------------------------------------------------------------------

	ASSUME	ES:NOTHING
entry	AddRec
	DOSAssume   <DS>,"AddRec"

	Assert	ISSFT,<ES,DI>,"AddRec"
	JCXZ	RET28		; If no records read,  don't change position
	ADD	WORD PTR ES:[DI.sf_position],CX  ; Update current position
	ADC	WORD PTR ES:[DI.sf_position+2],0
RET28:	CLC
	return
EndProc DISKREAD


Break   <DskRdBufScan -- Disk Read Buffer Scan>
;----------------------------------------------------------------------------
;
; Procedure Name : DskWrtBufScan
;
; Inputs:
;       CX = # of contiguous sectors read.  (These constitute a block of
;            sectors, also termed an "Extent".)
;       [HIGH_SECTOR]:DX = physical sector # of first sector in extent.
;       [TEMP_VAR2]:[TEMP_VAR] = Transfer address (destination data address).
;       ES:BP -> Drive Parameter Block (DPB).
;
; Function:
;	The Buffer Queue is scanned: the contents of any dirty buffers are
;	"read" into the transfer memory block, so that the transfer memory
;	reflects the most recent data.
;
; Outputs:
;       Transfer memory updated as required.
;
; Uses:
;       DS,AX,BX,CX,SI,DI destroyed.
;       SS override for all global variables.
;
; Notes:
;       FIRST_BUFF_ADDR is set-up to contain the LAST buffer to check, rather
;	than the FIRST.
;----------------------------------------------------------------------------
;M039: Created

procedure   DskRdBufScan,NEAR

ASSUME  DS:NOTHING

	cmp	[DirtyBufferCount],0	;Any dirty buffers?
	je	bufx			; -no, skip all work.

        mov     bx,[HIGH_SECTOR]
        mov     si,bx
        add     cx,dx
        adc     si,0

	call	GETCURHEAD		;DS:DI -> 1st buf in queue.
        mov     ax,[di].buf_prev
        mov     [FIRST_BUFF_ADDR],ax

        Assert  ISDPB,<ES,BP>,"DISKREAD/bufchk"
	mov	al,es:[bp].dpb_drive

;            BX:DX = Extent start.
;            SI:CX = Extent end + 1.
;               AL = Drive #.
;            DS:DI-> 1st buffer in queue.
;[FIRST_BUFF_ADDR] = Address offset of last buffer in queue.

bufq:	cmp     al,BYTE PTR [di.buf_ID] ;Same drive?
	jne     bufq1        		;  -no, jump.

        Cmp32   bx,dx,<WORD PTR [di.buf_sector+2]>,<WORD PTR [di.buf_sector]>
        ja	bufq1			;Jump if Extent start > buffer sector.
        Cmp32   si,cx,<WORD PTR [di.buf_sector+2]>,<WORD PTR [di.buf_sector]>
        ja	bufq2                   ;Jump if Extent end >= buffer sector.

bufq1:  cmp     di,[FIRST_BUFF_ADDR]    ;Scanned entire buffer queue?
        mov     di,[di].buf_next        ; Set-up for next buffer.
        jne     bufq                    ; -no, do next buffer

bufx:   return                          ;Exit.

;       Buffer's sector is in Extent: if it is dirty, copy its contents to
;	transfer memory; otherwise, just re-position it in the buffer queue
;       as MRU (Most Recently Used).

bufq2:  SaveReg <ax>
        testb   [di.buf_flags],buf_dirty ;Buffer dirty?
        jz      bufq3                    ; -no, jump.

        SaveReg <cx,dx,si,di,es>

        mov     ax,dx
	sub	ax, word ptr [di].buf_sector
	neg	ax

;       AX = offset (in sectors) of buffer sector within Transfer memory
;            block.  (Note: the upper word of the sector # may be ignored
;	     since no more than 64k bytes will ever be read.  This 64k limit
;            is imposed by the input parameters of the disk read operation.)

	lea	si,[di].BUFINSIZ	;DS:SI -> buffer data.
        mov     cx,es:[bp].dpb_sector_size ;CX = sector size (in bytes).
	mul     cx			;AX = offset (in bytes) of buf. sector
        mov     di,WORD PTR [TEMP_VAR]
	add	di,ax
	mov	es,WORD PTR [TEMP_VAR2]
	shr	cx,1

;	   CX = sector size (in WORDs); CF=1 if odd # of bytes.
;       DS:SI-> Buffer sector data.
;       ES:DI-> Destination within Transfer memory block.

	rep	movsw			;Copy buffer sector to Transfer memory
	adc	cx,0                    ;CX=1 if odd # of bytes, else CX=0.
	rep	movsb                   ;Copy last byte.
	RestoreReg <es,di,si,dx,cx>

;       DS:DI -> current buffer.

bufq3:  mov     ax,di			;DS:AX -> Current buffer.
        invoke  SCANPLACE
        cmp	ax,[FIRST_BUFF_ADDR]    ;Last buffer?
        RestoreReg <ax>
        jnz    	bufq                   	; -no, jump.
        jmp     short bufx              ; -yes, exit.

EndProc DskRdBufScan


DOSCODE	ENDS
	END


