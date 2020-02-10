	page	,164
	TITLE   DISK3 - Disk utility routines
	NAME    Disk3

;
;	Microsoft Confidential
;	Copyright (C) Microsoft Corporation 1991
;	All Rights Reserved.
;

;**     Low level Read and write routines for local SFT I/O on files and devs
;
;       DISKWRITE
;       WRTERR
;
;       Revision history:
;
;           1.  AN000 version 4.00 Jan. 1988
;           2.  SR; 10/12/89; Fixed bug in DISKWRITE that caused incorrect
;               calculation of filesizes when they cross the 32M boundary
;               if the filesize is not a multiple of 512 bytes
;             AN002 - Sept. 1988, PTM#4977 fix (writing cluster bug)  Bill Lawton
;		M017 MD 8/30/90 - Rewrite SHR32 for better speed
;        	M039 DB 10/17/90 - Disk write optimization

	.xlist
	.xcref
	include version.inc
	include dosseg.inc
	INCLUDE DOSSYM.INC
	INCLUDE DEVSYM.INC
	include dpb.inc
	include sf.inc
        include dosmac.inc              ;M039
	.cref
	.list

Installed = TRUE

	i_need	READOP,BYTE
	i_need	DISK_FULL,BYTE
	i_need  THISSFT,DWORD
	i_need  DMAADD,DWORD
	i_need  SecClusPos,BYTE
	i_need  ClusNum,WORD
	i_need  Trans,BYTE
	i_need  BytPos,4
	i_need  SecPos,DWORD              ;F.C. >32mb   ;AN000;
	i_need  BytSecPos,WORD
	i_need  BytCnt1,WORD
	i_need  BytCnt2,WORD
	i_need  SecCnt,WORD
	i_need  ThisDPB,DWORD
	i_need  LastPos,WORD
	i_need  ValSec,WORD               ;F.C. >32mb          ;AN000;
	i_need  GrowCnt,DWORD
	i_need  ALLOWED,BYTE
	I_need  JShare,DWORD


	I_need  HIGH_SECTOR,WORD          ;F.C. >32mb          ;AN000;
;	I_need  HIGH_SECTOR_TEMP,WORD     ;M039: Removed; F.C. >32mb ;AN000;

	I_need  EXTERR,WORD                        ; DOS 4.00   ;AN000;
	I_need  EXTERR_LOCUS,BYTE                  ; DOS 4.00   ;AN000;
	I_need  EXTERR_ACTION,BYTE                 ; DOS 4.00   ;AN000;
	I_need  EXTERR_CLASS,BYTE                  ; DOS 4.00   ;AN000;
	I_need  EXITHOLD,DWORD                     ; DOS 4.00   ;AN000;

;**RMFHFE**     I_need  DISK_FULL,BYTE                     ; DOS 4.00   ;AN000;

	I_need  SC_DRIVE,BYTE                      ; DOS 4.00   ;AN000;
	I_need  SC_CACHE_COUNT,WORD                ; DOS 4.00   ;AN000;

        I_need  SC_STATUS,WORD                     ;M039
        I_need  CURSC_SECTOR,WORD                  ;M039
        I_need  CURSC_DRIVE,BYTE                   ;M039

	I_need  ThisDRV,BYTE                       ; DOS 4.00   ;AN000;
	I_need  User_In_AX,WORD                    ; DOS 4.00   ;AN000;
	I_need  DOS34_FLAG,WORD                    ; DOS 4.00   ;AN000;
	I_need  FIRST_BUFF_ADDR,WORD               ; DOS 4.00   ;AN000;

        I_need  TEMP_VAR,WORD                      ;M039
        I_need  TEMP_VAR2,WORD                     ;M039


DOSCODE Segment
	ASSUME  SS:DOSDATA,CS:DOSCODE

Break   <DISKWRITE -- PERFORM USER DISK WRITE>
;----------------------------------------------------------------------------
;
; Procedure Name : DISKWRITE
;
; Inputs:
;       Outputs of SETUP
; Function:
;       Perform disk write
; Outputs:
;    Carry clear
;       CX = No. of bytes read
;       ES:DI point to SFT
;       SFT offset and cluster pointers updated
;    Carry set
;       CX = 0
;       ES:DI point to SFT
;       AX has error code
;----------------------------------------------------------------------------

;hkn; called by DOS_WRITE. DS already set up at this point.

procedure   DISKWRITE,NEAR
	DOSAssume   <DS>,"DiskWrite"

	Assert  ISSFT,<ES,DI>,"DiskWrite"


	invoke  CHECK_WRITE_LOCK          ;IFS. check write lock                 ;AN000;
	JNC     WRITE_OK                  ;IFS. lock check ok                    ;AN000;
	return

WRTEOFJ:
	JMP     WRTEOF

WRITE_OK:
	AND     ES:[DI].SF_FLAGS,NOT (sf_close_nodate OR devid_file_clean)
				; Mark file as dirty, clear no date on close

	MOV	AX,WORD PTR ES:[DI.sf_size]		;M039
        MOV     [TEMP_VAR],AX                           ;M039
	MOV	AX,WORD PTR ES:[DI.sf_size+2]		;M039
        MOV     [TEMP_VAR2],AX                          ;M039

;	TEMP_VAR2:TEMP_VAR = Current file size (sf_size);M039

	LES     BP,[THISDPB]
	Assert  ISDPB,<ES,BP>,"DiskWrite/WriteOk"

	invoke  BREAKDOWN
	MOV     AX,WORD PTR [BYTPOS]
	MOV     DX,WORD PTR [BYTPOS+2]
	JCXZ    WRTEOFJ                 ;Make the file length = sf_position
	ADD     AX,CX
	ADC     DX,0                    ;DX:AX = last byte to write + 1.

	MOV     BX,ES:[BP.dpb_sector_size]

	CALL    DIV32                   ;DX:AX/BX = CX:AX + DX (rem.).
	MOV     SI,AX
        MOV     [HIGH_SECTOR],CX

;       [HIGH_SECTOR]:SI = Last full sector to write.

	OR	DX,DX
	PUSH	DX			;M039: Free DX for use by SHR32
	MOV	DX,CX			;M039
	JNZ     CALCLUS
	SUB     AX,1                    ;AX must be zero base indexed          ;AC000;
	SBB	DX,0			;M039 ;F.C. >32mb			       ;AN000;

CALCLUS:
	CALL    SHR32                   ;F.C. >32mb                             ;AN000;
	POP	DX

;       AX = Last cluster to write.
;       DX = # of bytes in last sector to write (the "tail").
;       BX = ES:[BP.dpb_sector_size]

	PUSH    AX
	PUSH	DX

;M039
        mov     dx,[TEMP_VAR2]
        mov     ax,[TEMP_VAR]           ;DX:AX = current file size (in bytes).
        call    DIV32           	;DX:AX/BX = CX:AX + DX (rem.)
        mov     [TEMP_VAR2],cx
        mov     [VALSEC+2],cx
        mov     cx,ax
        mov     bx,si

;       [HIGH_SECTOR]:BX = Last full sector to write.
;          [VALSEC+2]:CX = Last full sector of current file.
;         [TEMP_VAR2]:CX = Last full sector of current file.
;                     DX = # of bytes in last sector of current file.
;M039

	OR      DX,DX
	JZ      NORND
	ADD     AX,1            	;Round up if any remainder                     ;AC000;
	ADC     [VALSEC+2],0
NORND:	MOV     WORD PTR [VALSEC],AX

;       [VALSEC] = Last sector of current file.

	XOR     AX,AX
	MOV     WORD PTR [GROWCNT],AX
	MOV     WORD PTR [GROWCNT+2],AX
	POP     AX

	MOV     DI,[HIGH_SECTOR]        ;F.C. >32mb                             ;AN000;
	CMP     DI,[TEMP_VAR2]		;M039; F.C. >32mb				;AN000;
	DLJB    NOGROW                  ;F.C. >32mb                             ;AN000;
	JZ      lowsec                  ;F.C. >32mb                             ;AN000;
	SUB     BX,CX                   ;F.C. >32mb                             ;AN000;
	SBB     DI,[TEMP_VAR2]   	;M039; F.C. >32mb di:bx no. of sectors    ;AN000;
	JMP     short yesgrow           ;F.C. >32mb                             ;AN000;
lowsec:
	MOV     DI,0            ;F.C. >32mb
	SUB     BX,CX           ; Number of full sectors
	JB      NOGROW
	JZ      TESTTAIL
yesgrow:
	MOV     CX,DX
	XCHG    AX,BX
	MUL     ES:[BP.dpb_sector_size]  ; Bytes of full sector growth
	MOV     [HIGH_SECTOR],DX         ;F.C. >32mb save dx                    ;AN000;
	MOV     [TEMP_VAR2],AX    	 ;M039; F.C. >32mb save ax                ;AN000;
	MOV     AX,DI                    ;F.C. >32mb                            ;AN000;
	MUL     ES:[BP.dpb_sector_size]  ;F.C. >32mb do higher word multiply    ;AN000;
	ADD     AX,[HIGH_SECTOR]         ;F.C. >32mb add lower value            ;AN000;
	MOV     DX,AX                    ;F.C. >32mb DX:AX is the result of     ;AN000;
	MOV     AX,[TEMP_VAR2]    	 ;M039; F.C. >32mb a 32 bit multiply      ;AN000;

	SUB     AX,CX           ; Take off current "tail"
	SBB     DX,0            ; 32-bit extension
	ADD     AX,BX           ; Add on new "tail"
	ADC     DX,0            ; ripple tim's head off
	JMP     SHORT SETGRW
HAVSTART:
;int 3
	MOV     CX,AX
	invoke  SKPCLP
	JCXZ    DOWRTJ


	invoke  ALLOCATE
	JNC     DOWRTJ

entry   WRTERR
	DOSAssume   <DS>,"DiskWrite/WrtErr"
	ASSUME  ES:NOTHING

	MOV     AH,0FH                          ;MS. write/data/fail/abort      ;AN000;
entry WRTERR22
	MOV     AL,[THISDRV]                    ;MS.                            ;AN000;

;**RMFHFE**     CALL    File_Handle_Fail_Error  ;MS. issue disk full I24

;**RMFHFE**     MOV     CX,0                    ;No bytes transferred
	XOR     CX,CX                   ; will be deleted

	LES     DI,[THISSFT]
	Assert  ISSFT,<ES,DI>,"DiskWrite/WrtErr"
	CLC
	return

DOWRTJ: JMP     short DOWRT

ACC_ERRWJ:
	JMP     SET_ACC_ERRW

TESTTAIL:
	SUB     AX,DX
	JBE     NOGROW
	XOR     DX,DX
SETGRW:
	MOV     WORD PTR [GROWCNT],AX
	MOV     WORD PTR [GROWCNT+2],DX
NOGROW:
	POP     AX
	MOV     CX,[CLUSNUM]    ; First cluster accessed
	invoke  FNDCLUS
	JC      ACC_ERRWJ
	MOV     [CLUSNUM],BX
	MOV     [LASTPOS],DX


	SUB     AX,DX           ; Last cluster minus current cluster
	JZ      DOWRT           ; If we have last clus, we must have first
	JCXZ    HAVSTART        ; See if no more data
	PUSH    CX              ; No. of clusters short of first
	MOV     CX,AX

	invoke  ALLOCATE
	POP     CX
	JC      WRTERR
	MOV     DX,[LASTPOS]
	INC     DX
	DEC     CX
	JZ      NOSKIP
	invoke  SKPCLP
	JC      ACC_ERRWJ
NOSKIP:
	MOV     [CLUSNUM],BX
	MOV     [LASTPOS],DX
DOWRT:
	CMP     [BYTCNT1],0
	JZ      WRTMID
	MOV     BX,[CLUSNUM]
	invoke  BUFWRT
	JC      ACC_ERRWJ
WRTMID:
	MOV     AX,[SECCNT]
	OR      AX,AX
	JZ      WRTLAST                  ;M039
	ADD     WORD PTR [SECPOS],AX
	ADC     WORD PTR [SECPOS+2],0    ;F.C. >32mb                            ;AN000;
	invoke  NEXTSEC
	JC      SET_ACC_ERRW             ;M039
	MOV     BYTE PTR [TRANS],1       ; A transfer is taking place
	MOV     DL,[SECCLUSPOS]
	MOV     BX,[CLUSNUM]
	MOV     CX,[SECCNT]
WRTLP:
	invoke  OPTIMIZE
	JC     	SET_ACC_ERRW

;M039
;       DI = Next physical cluster.
;       AX = # sectors remaining.
;       [DMAADD+2]:BX = transfer address (source data address).
;       CX = # of contiguous sectors to write. (These constitute a block of
;	     sectors, also termed an "Extent".)
;       [HIGH_SECTOR]:DX = physical sector # of first sector in extent.
;       ES:BP -> Drive Parameter Block (DPB).
;
;       Purge the Buffer Queue and the Secondary Cache of any buffers which
;	are in Extent; they are being over-written.

	push    di
	push    ax
        call    DskWrtBufPurge          ;DS trashed.
ASSUME DS:NOTHING
;M039

;hkn; SS override for DMAADD and ALLOWED
	MOV     DS,WORD PTR [DMAADD+2]
	MOV	[ALLOWED],allowed_RETRY + allowed_FAIL + allowed_IGNORE

;	put logic from DWRITE in-line here so we can modify it
;	for DISK FULL conditions.

DWRITE_LUP:
	invoke	DSKWRITE
	jz	DWRITE_OKAY

;	int	3
	cmp	al,error_handle_Disk_Full	; compressed volume full?
	jz	DWRITE_DISK_FULL

;hkn; SS override
	MOV	BYTE PTR [READOP],1
	invoke	HARDERRRW
	CMP	AL,1		; Check for retry
	JZ	DWRITE_LUP
	CMP	AL,3		; Check for FAIL
	CLC
	JNZ	DWRITE_OKAY 	; Ignore
	STC

DWRITE_OKAY:


	POP     CX
	POP     BX

;       CX = # sectors remaining.
;       BX = Next physical cluster.

;hkn; SS override
	Context DS

	JC      SET_ACC_ERRW
	JCXZ    WRTLAST
	MOV     DL,0
	INC     [LASTPOS]       ; We'll be using next cluster
	JMP     WRTLP

DWRITE_DISK_FULL:
        Context DS		;SQ 3-5-93 DS must be setup on return!
	pop	cx		; unjunk stack
	pop	bx
	mov	[DISK_FULL],1
	stc
	jmp	WRTERR		; go to disk full exit

SET_ACC_ERRW:
	transfer SET_ACC_ERR_DS

WRTLAST:
	MOV     AX,[BYTCNT2]
	OR      AX,AX
	JZ      FINWRT
	MOV     [BYTCNT1],AX
	invoke  NEXTSEC
	JC      SET_ACC_ERRW
	MOV     [BYTSECPOS],0
	invoke  BUFWRT
	JC      SET_ACC_ERRW
FINWRT:
	LES     DI,[THISSFT]
	Assert  ISSFT,<ES,DI>,"DiskWrite/FinWrt"
	MOV     AX,WORD PTR [GROWCNT]
	MOV     CX,WORD PTR [GROWCNT+2]
	OR      AX,AX
	JNZ     UPDATE_size
	JCXZ    SAMSIZ
Update_size:
	ADD     WORD PTR ES:[DI.sf_size],AX
	ADC     WORD PTR ES:[DI.sf_size+2],CX
;
; Make sure that all other SFT's see this growth also.
;
	MOV     AX,1
if installed
	call    JShare + 14 * 4
else
	Call    ShSU
endif
SAMSIZ:
	transfer SETCLUS                  ; ES:DI already points to SFT

WRTEOF:
	MOV     CX,AX
	OR      CX,DX
	JZ      KILLFIL
	SUB     AX,1
	SBB     DX,0

	PUSH    BX
	MOV     BX,ES:[BP.dpb_sector_size]    ;F.C. >32mb                       ;AN000;
	CALL    DIV32                         ;F.C. >32mb                       ;AN000;
	POP	BX			      ;F.C. >32mb			;AN000;
	MOV	DX,CX			      ;M039
        MOV     [HIGH_SECTOR],CX              ;M039: Probably extraneous, but not sure.
	CALL    SHR32                         ;F.C. >32mb                       ;AN000;

	MOV     CX,AX
	invoke  FNDCLUS
SET_ACC_ERRWJ2:
	JC      SET_ACC_ERRW


	JCXZ    RELFILE
	invoke  ALLOCATE
	JC      WRTERRJ              ;;;;;;;;; disk full
UPDATE:
	LES     DI,[THISSFT]
	Assert  ISSFT,<ES,DI>,"DiskWrite/update"
	MOV     AX,WORD PTR [BYTPOS]
	MOV     WORD PTR ES:[DI.sf_size],AX
	MOV     AX,WORD PTR [BYTPOS+2]
	MOV     WORD PTR ES:[DI.sf_size+2],AX
;
; Make sure that all other SFT's see this growth also.
;
	MOV     AX,2
if installed
	Call    JShare + 14 * 4
else
	Call    ShSU
endif
	XOR     CX,CX
	transfer ADDREC

WRTERRJ: JMP     WRTERR
;;;;;;;;;;;;;;;; 7/18/86
;;;;;;;;;;;;;;;;;
RELFILE:
	PUSH    ES                    ;AN002; BL   Reset Lstclus and cluspos to
	LES     DI,[THISSFT]          ;AN002; BL   beginning of file if current
	CMP     DX,ES:[DI.sf_cluspos] ;AN002; BL   cluspos is past EOF.
	JAE     SKIPRESET             ;AN002; BL
	MOV     ES:[DI.sf_cluspos],0  ;AN002; BL
	MOV     DX,ES:[DI.sf_firclus] ;AN002; BL
	MOV     ES:[DI.sf_lstclus],DX ;AN002; BL
SKIPRESET:                            ;AN002; BL
	POP     ES                    ;AN002; BL
;
	MOV     DX,0FFFFH
	invoke  RELBLKS
Set_Acc_ERRWJJ:
	JC      SET_ACC_ERRWJ2
	JMP     SHORT UPDATE

KILLFIL:
	XOR     BX,BX
	PUSH    ES
	LES     DI,[THISSFT]
	Assert  ISSFT,<ES,DI>,"DiskWrite/KillFil"
	MOV     ES:[DI.sf_cluspos],BX
	MOV     ES:[DI.sf_lstclus],BX
	XCHG    BX,ES:[DI.sf_firclus]
	POP     ES

	OR      BX,BX
	JZ      UPDATEJ
;; 10/23/86 FastOpen update
	PUSH    ES              ; since first cluster # is 0
	PUSH    BP              ; we must delete the old cache entry
	PUSH    AX
	PUSH    CX
	PUSH    DX
	LES     BP,[THISDPB]             ; get current DPB
	MOV     DL,ES:[BP.dpb_drive]     ; get current drive
	MOV     CX,BX                    ; first cluster #
	MOV     AH,2                     ; delete cache entry by drive:firclus
	invoke  FastOpen_Update          ; call fastopen
	POP     DX
	POP     CX
	POP     AX
	POP     BP
	POP     ES
;; 10/23/86 FastOpen update

	invoke  RELEASE
	JC      SET_ACC_ERRWJJ
UpDateJ:
	JMP     UPDATE
EndProc DISKWRITE



Break   <DskWrtBufPurge -- Disk Write Buffer Purge>
;----------------------------------------------------------------------------
;
; Procedure Name : DskWrtBufPurge
;
; Inputs:
;       CX = # of contiguous sectors to write. (These constitute a block of
;	     sectors, also termed an "Extent".)
;       [HIGH_SECTOR]:DX = physical sector # of first sector in extent.
;       ES:BP -> Drive Parameter Block (DPB).
;
; Function:
;       Purge the Buffer Queue and the Secondary Cache of any buffers which
;	are in Extent; they are being over-written.
;
; Outputs:
;       (Same as Input.)
; Uses:
;       All registers except DS,AX,SI,DI preserved.
;       SS override for all global variables.
;----------------------------------------------------------------------------
;M039: Created

procedure   DskWrtBufPurge,NEAR

ASSUME  DS:NOTHING

        SaveReg <bx,cx>
        mov	bx,[HIGH_SECTOR]	;BX:DX = Extent start (sector #).
        mov     si,bx
        add     cx,dx
        adc     si,0                    ;SI:CX = Extent end + 1.

	Assert  ISDPB,<ES,BP>,"DskWrtBufPurge"
	mov     al,es:[bp.dpb_drive]

;       BX:DX = Extent start.
;       SI:CX = Extent end + 1.
;          AL = Drive #

	cmp     [SC_CACHE_COUNT],0      ;Secondary cache in-use?
	je      nosc                    ; -no, jump.

;       If any of the sectors to be written are in the secondary cache (SC),
;       invalidate the entire SC. (This is an optimization; we really only
;	need to invalidate those sectors which intersect, but that's slower.)

        cmp     al,[CURSC_DRIVE]        ;Same drive?
        jne     nosc                    ; -no, jump.

        push    ax
        mov     ax,[CURSC_SECTOR]
        mov     di,[CURSC_SECTOR+2]     ;DI:AX = SC start.
        Cmp32   si,cx,di,ax             ;Extent end < SC start?
        jbe     sc5                     ; -yes, jump.
        add     ax,[SC_CACHE_COUNT]
        adc     di,0                    ;DI:AX = SC end + 1.
        Cmp32   bx,dx,di,ax             ;Extent start > SC end?
        jae     sc5                     ; -yes, jump.
        mov     [SC_STATUS],0           ;Extent intersects SC: invalidate SC.
sc5:	pop     ax

;       Free any buffered sectors which are in Extent; they are being over-
;       written.

nosc:	invoke  GETCURHEAD              ;DS:DI -> first buffer in queue.

bufq:   cmp     al,BYTE PTR [di.buf_ID] ;Same drive?
        jne     bufq5                   ; -no, jump.

        Cmp32   bx,dx,<WORD PTR [di.buf_sector+2]>,<WORD PTR [di.buf_sector]>
        ja	bufq5			;Jump if Extent start > buffer sector.
        Cmp32   si,cx,<WORD PTR [di.buf_sector+2]>,<WORD PTR [di.buf_sector]>
        jbe	bufq5                   ;Jump if Extent end < buffer sector.

;       Buffer's sector is in Extent, so free it; it is being over-written.

        testb   [di.buf_flags],buf_dirty ;Buffer dirty?
        jz      bufq2                    ; -no, jump.
        invoke  DEC_DIRTY_COUNT          ; -yes, decrement dirty count.
bufq2:  mov     WORD PTR [di.buf_ID],((buf_visit SHL 8) OR 0FFh)

        invoke  SCANPLACE
        jmp     short bufq6

bufq5:  mov     di,[di.buf_next]
bufq6:  cmp     di,[FIRST_BUFF_ADDR]    ;Scanned entire buffer queue?
        jne     bufq	                ; --no, go do next buffer.

        RestoreReg <cx,bx>
        return

EndProc DskWrtBufPurge


Break   <DIV32 -- PERFORM 32 BIT DIVIDE>
;----------------------------------------------------------------------------
;
; Procedure Name : DIV32
;
; Inputs:
;       DX:AX = 32 bit dividend   BX= divisor
; Function:
;       Perform 32 bit division:  DX:AX/BX = CX:AX + DX (rem.)
; Outputs:
;       CX:AX = quotient , DX= remainder
; Uses:
;       All registers except AX,CX,DX preserved.
;----------------------------------------------------------------------------
;M039: DIV32 optimized for divisor of 512 (common sector size).

procedure   DIV32,NEAR

        cmp     bx,512
        jne    	short div5

	mov     cx,dx
        mov     dx,ax           ; CX:AX = Dividend
        and     dx,(512-1)      ; DX = Remainder
        mov     al,ah
        mov     ah,cl
        mov     cl,ch
        xor     ch,ch
        shr     cx,1
        rcr     ax,1
        return

div5:	mov     cx,ax
        mov     ax,dx
        xor     dx,dx
        div     bx              ; 0:AX/BX
        xchg    cx,ax
        div     bx              ; DX:AX/BX
        return

EndProc DIV32

Break   <SHR32 -- PERFORM 32 BIT SHIFT RIGHT>
;----------------------------------------------------------------------------
;
; Proedure Name : SHR32
;
; Inputs:
;	DX:AX = 32 bit sector number
; Function:
;       Perform 32 bit shift right
; Outputs:
;	AX = cluster number
;	ZF = 1 if no error
;	   = 0 if error (cluster number > 64k)
; Uses:
;       DX,CX
;---------------------------------------------------------------------------
; M017	- SHR32 rewritten for better performance
; M039	- Additional optimization

procedure   SHR32,NEAR

	mov     cl,es:[bp.dpb_cluster_shift]
	xor	ch,ch	    ;ZF=1
	jcxz	norota

rotashft2:
	shr	dx,1	    ;ZF reflects state of DX.
	rcr	ax,1	    ;ZF not affected.
	loop	rotashft2

norota:
	return

EndProc SHR32

COMMENT @

;**RMFHFE** Remove File_Handle_Fail_Error support

;---------------------------------------------------------------------------
;
; Procedure Name : File_Handle_Fail_Error
;
; Issue File Handle Fail INT 24 Critical Error
; Input: Disk_Full=0  ok
;                  1  disk full or EOF
; Function: issue critical error for disk full or EOF error
;
; OutPut: carry clear , no I24
;         carry set, fail from I24
;---------------------------------------------------------------------------

procedure File_Handle_Fail_Error,NEAR                                           ;AN000;

;hkn; SS override for all variables in this procedure
										;AN000;
	CMP     [DISK_FULL],0    ;MS. disk full or EOF                          ;AN000;
	JZ      Fexit            ;MS. no                                        ;AN000;
	TESTB   [DOS34_FLAG],Disable_EOF_I24   ;MS. check input status ?        ;AN000;
	JNZ     Fexit            ;MS. yes                                       ;AN000;
										;AN000;
	LES     DI,[THISSFT]     ;MS. get current SFT                           ;AN000;
;       LES     DI,ES:[DI.sf_DEVPTR];MS. get device header                      ;AN000;
	TESTB   ES:[DI].SF_FLAGS,Handle_Fail_I24  ;MS. gen I24 ?                ;AN000;
	JZ      Fexit            ;MS. no                                        ;AN000;
	PUSH    DS               ;MS. save DS                                   ;AN000;
	test    AH,1                            ;MS. READ ?                     ;AN000;
	JZ      readeof                         ;MS. yes                        ;AN000;
	MOV     [EXTERR],error_Handle_Disk_Full ;MS. set extended error         ;AN000;
	JMP     SHORT errset                    ;MS. set extended error         ;AN000;
readeof:
	MOV     [EXTERR],error_Handle_EOF       ;MS. set extended error         ;AN000;
errset:
	MOV     [EXTERR_CLASS],errCLASS_OutRes  ;MS. set class                  ;AN000;
	MOV     [EXTERR_ACTION],errACT_Abort    ;MS. set action                 ;AN000;
	MOV     [EXTERR_LOCUS],errLOC_Unk       ;MS. set locus                  ;AN000;
	MOV     word ptr [EXITHOLD + 2],ES      ;MS. save es:bp in exithold     ;AN000;
	MOV     word ptr [EXITHOLD],BP          ;MS.                            ;AN000;
	TESTB   ES:[DI].SF_FLAGS,devid_device     ;MS. device  ?                ;AN000;
	JNZ     chardev2                          ;MS. yes                      ;AN000;
	LDS     SI,ES:[DI.sf_DEVPTR]              ;MS. get dpb                  ;AN000;
	LDS     SI,[SI.dpb_driver_addr]           ;MS. get drive device haeder  ;AN000;
	JMP     SHORT doi24                       ;MS. gen I24 ?                ;AN000;
chardev2:
	LDS     SI,ES:[DI.sf_DEVPTR]              ;MS. get chr dev header       ;AN000;
doi24:
	MOV     BP,DS                             ;MS. bp:si -> device header   ;AN000;
	MOV     DI,error_I24_gen_failure        ;MS. general error              ;AN000;
	invoke  NET_I24_ENTRY                   ;MS. issue I24                  ;AN000;
	STC                                     ;MS. must be fail               ;AN000;
	POP     DS                              ;MS. restore DS                 ;AN000;
	MOV     AX,[EXTERR]                     ;MS. set error                  ;AN000;
	JMP     SHORT Fend                      ;MS. exit                       ;AN000;
Fexit:                                                                          ;AN000;
	CLC                                     ;MS. clear carry                ;AN000;
Fend:                                                                           ;AN000;
	return                                  ;MS.                            ;AN000;
										;AN000;
EndProc File_Handle_Fail_Error                                                  ;AN000;

@       ;End COMMENT.

DOSCODE ENDS
	END
