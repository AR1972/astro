	TITLE	FAT - FAT maintenance routines
	NAME	FAT

;
;	Microsoft Confidential
;	Copyright (C) Microsoft Corporation 1991
;	All Rights Reserved.
;

;**	FAT.ASM
;
;	Low level local device routines for performing disk change sequence,
;	setting cluster validity, and manipulating the FAT
;
;	IsEof
;	UNPACK
;	PACK
;	MAPCLUSTER
;	FATREAD_SFT
;	FATREAD_CDS
;	FAT_operation
;
;	Revision history:
;
;	  AN000  version Jan. 1988
;	   A001  PTM	      -- disk changed for look ahead buffers
;
;	M014 - if a request for pack\unpack cluster 0 is made we write\read
;	       from CL0FATENTRY rather than disk.
;

	extrn	GetCurHead:near, ScanPlace:near

	.xlist
	.xcref
	include version.inc
	include dosseg.inc
	INCLUDE DOSSYM.INC
	INCLUDE DEVSYM.INC
	include sf.inc
	include dpb.inc
	include curdir.inc
	.cref
	.list

FastDiv = TRUE

	i_need	CURBUF,DWORD
	i_need	CLUSSPLIT,BYTE
	i_need	CLUSSAVE,WORD
	i_need	CLUSSEC,DWORD		;F.C. >32mb  ;AN000;
	i_need	THISDRV,BYTE
	i_need	THISDPB,DWORD
	i_need	DEVCALL,BYTE
	i_need	CALLMED,BYTE
	i_need	CALLRBYT,BYTE
	i_need	CALLXAD,DWORD
	i_need	CALLBPB,DWORD
	i_need	CDSADDR,DWORD
	i_need	CDSCOUNT,BYTE
	i_need	EXTERR,WORD
	i_need	EXTERRPT,DWORD
	i_need	CALLVIDM,DWORD
	i_need	ReadOp,BYTE
	i_need	FAILERR,BYTE
	i_need	ALLOWED,BYTE
	i_need	VOLCHNG_FLAG,BYTE
	i_need	HIGH_SECTOR,WORD
	i_need	DirtyBufferCount,WORD
	i_need	FIRST_BUFF_ADDR,WORD
	i_need	SC_CACHE_COUNT,WORD	       ;AN001;
	i_need	CURSC_DRIVE,BYTE	       ;AN001;

	i_need	CL0FATENTRY,WORD

DOSCODE	SEGMENT
	ASSUME	SS:DOSDATA,CS:DOSCODE


Break <IsEOF - check the quantity in BX for EOF>
;----------------------------------------------------------------------------
;
; Procedure Name : IsEOF
;
; IsEOF - check the fat value in BX for eof.
;
;   Inputs:	ES:BP point to DPB
;		BX has fat value
;   Outputs:	JAE eof
;   Registers modified: none
;
;---------------------------------------------------------------------------

Procedure IsEof,NEAR
	ASSUME	SS:DOSDATA,CS:DOSCODE

	Assert	    ISDPB,<ES,BP>,"IsEOF"
	CMP	ES:[BP.dpb_max_cluster],4096-10 ; is this 16 bit fat?
	JAE	EOF16			; yes, check for eof there
;J.K. 8/27/86
;Modified to accept 0FF0h as an eof. This is to handle the diskfull case
;of any media that has "F0"(Other) as a MediaByte.
;Hopely, this does not create any side effect for those who may use any value
;other than "FF8-FFF" as an Eof for their own file.
	cmp	bx,0FF0h
	je	IsEOF_other
	CMP	BX,0FF8h		; do the 12 bit compare
IsEOF_other:
	return
EOF16:
	CMP	BX,0FFF8h		; 16 bit compare
	return
EndProc IsEof

Break	<UNPACK -- UNPACK FAT ENTRIES>
;---------------------------------------------------------------------------
;
; Procedur Name : UNPACK
;
; Inputs:
;	BX = Cluster number (may be full 16-bit quantity)
;	ES:BP = Base of drive parameters
; Outputs:
;	DI = Contents of FAT for given cluster (may be full 16-bit quantity)
;	Zero set means DI=0 (free cluster)
;	Carry set means error (currently user FAILed to I 24)
; SI Destroyed, No other registers affected. Fatal error if cluster too big.
;
; NOTE: if BX = 0 then DI = contents of CL0FATENTRY
;
;----------------------------------------------------------------------------

procedure   UNPACK,NEAR
	DOSAssume   <DS>,"UnPack"
					; M014 - Start
	or	bx, bx			; Q: are we unpacking cluster 0
	jnz	up_cont			; N: proceed with normal unpack
	mov	di, [CL0FATENTRY]	; Y: return value in CL0FATENTRY
	or	di,di 			; return z if di=0
	ret				; done
up_cont:				; M014 - End
	
	Assert	    ISDPB,<ES,BP>,"Unpack"
	CMP	BX,ES:[BP.dpb_max_cluster]
	JA	HURTFAT
	CALL	MAPCLUSTER
ASSUME	DS:NOTHING
	jc	DoContext
	MOV	DI,[DI]
	JNZ	High12			; MZ if high 12 bits, go get 'em
	MOV	SI,ES:[BP.dpb_max_cluster]  ; MZ is this 16-bit fat?
	CMP	SI,4096-10
	JB	Unpack12		; MZ No, go 'AND' off bits
	OR	DI,DI			; MZ set zero condition code, clears carry
	JMP	SHORT DoContext 	; MZ go do context

High12:
	SHR	DI,1
	SHR	DI,1
	SHR	DI,1
	SHR	DI,1
Unpack12:
	AND	DI,0FFFH		; Clears carry
DoContext:
	PUSH	SS
	POP	DS
	return

HURTFAT:
	MOV	ES:[BP.dpb_free_cnt],-1 ; Err in FAT must force recomp of freespace
	PUSH	AX
	MOV	AH,allowed_fail + 80h

;hkn; SS override
	MOV	Allowed,allowed_fail
;
; Signal Bad FAT to INT int_fatal_abort handler.  We have an invalid cluster.
;
	MOV	DI,0FFFH		; In case INT int_fatal_abort returns (it shouldn't)
	invoke	FATAL
	CMP	AL,3
	CLC
	JNZ	OKU_RET 		; Try to ignore bad FAT
	STC				; User said FAIL
OKU_RET:
	POP	AX
	return
EndProc UNPACK

Break	<PACK -- PACK FAT ENTRIES>
;----------------------------------------------------------------------------
;
; Procedure Name : PACK
;
; Inputs:
;	BX = Cluster number
;	DX = Data
;	ES:BP = Pointer to drive DPB
; Outputs:
;	The data is stored in the FAT at the given cluster.
;	SI,DX,DI all destroyed
;	Carry set means error (currently user FAILed to I 24)
;	No other registers affected
;
; NOTE: if BX = 0 then data in DX is atored in CL0FATENTRY.
;
;---------------------------------------------------------------------------

procedure   PACK,NEAR
	DOSAssume   <DS>,"Pack"

	Assert	    ISDPB,<ES,BP>,"Pack"
					; M014 - start
	or	bx, bx			; Q: are we packing cluster 0
	jnz	p_cont			; N: proceed with normal pack
	mov	[CL0FATENTRY], dx	; Y: place value in CL0FATENTRY
	ret				; done
p_cont:					; M014 - end

	CALL	MAPCLUSTER
    ASSUME DS:NOTHING
	JC	DoContext
	MOV	SI,[DI]
	JZ	Aligned 		; byte (not nibble) aligned
	PUSH	CX			; move data to upper 12 bits
	MOV	CL,4
	SHL	DX,CL
	POP	CX
	AND	SI,0FH			; leave in original low 4 bits
	JMP	SHORT PACKIN
ALIGNED:
	CMP	ES:[BP.dpb_max_cluster],4096-10 ; MZ 16 bit fats?
	JAE	Pack16			; MZ yes, go clobber original data
	AND	SI,0F000H		; MZ leave in upper 4 bits of original
	AND	DX,0FFFh		; MZ store only 12 bits
	JMP	SHORT PackIn		; MZ go store
Pack16:
	XOR	SI,SI			; MZ no original data
PACKIN:
	OR	SI,DX
	MOV	[DI],SI

;hkn; SS override
	LDS	SI,[CURBUF]
	TESTB	[SI.buf_flags],buf_dirty  ;LB. if already dirty		;AN000;
	JNZ	yesdirty		  ;LB.	  don't increment dirty count   ;AN000;
	invoke	INC_DIRTY_COUNT 	  ;LB.					;AN000;
	OR	[SI.buf_flags],buf_dirty  ;LB.					;AN000;
yesdirty:				  ;LB.					;AN000;

;hkn; SS override
	CMP	BYTE PTR [CLUSSPLIT],0

;hkn; SS is DOSDATA
	Context DS
	retz				; Carry clear
	PUSH	AX
	PUSH	BX
	PUSH	CX
	MOV	AX,[CLUSSAVE]
	MOV	DS,WORD PTR [CURBUF+2]
ASSUME	DS:NOTHING
	ADD	SI,BUFINSIZ
	MOV	[SI],AH

;hkn; SS is DOSDATA
	Context DS
	PUSH	AX
	MOV	DX,WORD PTR [CLUSSEC+2] 	   ;F.C. >32mb		       ;AN000;
	MOV	WORD PTR [HIGH_SECTOR],DX	   ;F.C. >32mb		       ;AN000;

	MOV	DX,WORD PTR [CLUSSEC]
	MOV	SI,1
	XOR	AL,AL
	invoke	GETBUFFRB
	POP	AX
	JC	POPP_RET
	LDS	DI,[CURBUF]
ASSUME	DS:NOTHING
	TESTB	[DI.buf_flags],buf_dirty  ;LB. if already dirty 		;AN000;
	JNZ	yesdirty2		  ;LB.	  don't increment dirty count   ;AN000;
	invoke	INC_DIRTY_COUNT 	  ;LB.					;AN000;
	OR	[DI.buf_flags],buf_dirty
yesdirty2:
	ADD	DI,BUFINSIZ
	DEC	DI
	ADD	DI,ES:[BP.dpb_sector_size]
	MOV	[DI],AL
	CLC
POPP_RET:
	PUSH	SS
	POP	DS
	POP	CX
	POP	BX
	POP	AX
	return

EndProc PACK

Break	<MAPCLUSTER - BUFFER A FAT SECTOR>
;---------------------------------------------------------------------------
;
; Procedure Name : MAPCLUSTER
;
; Inputs:
;	ES:BP Points to DPB
;	BX Is cluster number
; Function:
;	Get a pointer to the cluster
; Outputs:
;	DS:DI Points to contents of FAT for given cluster
;	DS:SI Points to start of buffer
;	Zero Not set if cluster data is in high 12 bits of word
;	Zero set if cluster data is in low 12 or 16 bits
;	Carry set if failed.
; SI is destroyed.
;
;---------------------------------------------------------------------------

procedure   MAPCLUSTER,NEAR
	DOSAssume   <DS>,"MapCluster"

	Assert	    ISDPB,<ES,BP>,"MapCluster"
	MOV	BYTE PTR [CLUSSPLIT],0
	SAVE	<AX,BX,CX,DX>
	MOV	AX,BX			; AX = BX
	CMP	ES:[BP.dpb_max_cluster],4096-10  ; MZ 16 bit fat?
	JAE	Map16			; MZ yes, do 16 bit algorithm
	SHR	AX,1			; AX = BX/2
Map16:					; MZ skip prev => AX=2*BX
	XOR	DI,DI			; >32mb     fat 			;AN000;
	ADD	AX,BX			; AX = 1.5*fat = byte offset in fat
	ADC	DI,DI			; >32mb fat ;DI is zero before op;AN000;
	MOV	CX,ES:[BP.dpb_sector_size]

IF FastDiv
;
; Gross hack:  99% of all disks have 512 bytes per sector.  We test for this
; case and apply a really fast algorithm to get the desired results
;
; Divide method takes 157+4*4=173 (MOV and DIV)
; Fast method takes 39+20*4=119
;
; This saves a bunch.
;
	CMP	CX,512			; 4	Is this 512 byte sector?
	jne	DoDiv			; 4     for no jump
	MOV	DX,AX			; 2	get set for remainder
	AND	DX,512-1		; 4	Form remainder
	MOV	AL,AH			; 2     Quotient in formation in AL
	shr	di, 1			; 2
	rcr	al, 1			; 2
	xor	ah, ah			; 3
	jmp	short DivDone		; 16
DoDiv:
ENDIF
	mov	dx, di			; 2
	DIV	CX			; 155 AX is FAT sector # DX is sector index
IF FastDiv
DivDone:
ENDIF
	ADD	AX,ES:[BP.dpb_first_FAT]
	DEC	CX			; CX is sector size - 1
	SAVE	<AX,DX,CX>
	MOV	DX,AX
	MOV	[HIGH_SECTOR],0 	;F.C. >32mb  low sector #
	XOR	AL,AL
	MOV	SI,1
	invoke	GETBUFFRB
	RESTORE <CX,AX,DX>		; CX is sec siz-1, AX is offset in sec
	JC	MAP_POP
	LDS	SI,[CURBUF]
ASSUME	DS:NOTHING
	LEA	DI,[SI.BufInSiz]
	ADD	DI,AX
	CMP	AX,CX
	JNZ	MAPRET
	MOV	AL,[DI]
	Context DS		 	;hkn; SS is DOSDATA
	INC	BYTE PTR [CLUSSPLIT]
	MOV	BYTE PTR [CLUSSAVE],AL
	MOV	WORD PTR [CLUSSEC],DX
	MOV	WORD PTR [CLUSSEC+2],0	      ;F.C. >32mb			;AN000;
	INC	DX
	MOV	[HIGH_SECTOR],0 	      ;F.C. >32mb  FAT sector <32mb	;AN000;
	XOR	AL,AL
	MOV	SI,1
	invoke	GETBUFFRB
	JC	MAP_POP
	LDS	SI,[CURBUF]
ASSUME	DS:NOTHING
	LEA	DI,[SI.BufInSiz]
	MOV	AL,[DI]
	Context DS			;hkn; SS is DOSDATA
	MOV	BYTE PTR [CLUSSAVE+1],AL

;hkn; CLUSSAVE is in DOSDATA
	MOV	DI,OFFSET DOSDATA:CLUSSAVE
MAPRET:
	RESTORE <DX,CX,BX>
	XOR	AX,AX			; MZ allow shift to clear carry
	CMP	ES:[BP.dpb_max_cluster],4096-10 ; MZ is this 16-bit fat?
	JAE	MapSet			; MZ no, set flags
	MOV	AX,BX
MapSet:
	TEST	AL,1			; set zero flag if not on boundary
	RESTORE <AX>
	return

MAP_POP:
	RESTORE <DX,CX,BX,AX>
	return

EndProc MAPCLUSTER, NoCheck

Break	<FATREAD_SFT/FATREAD_CDS -- CHECK DRIVE GET FAT>
;----------------------------------------------------------------------------
;
; Procedure Name : FATREAD_SFT
;
; Inputs:
;	ES:DI points to an SFT for the drive of intrest (local only,
;		giving a NET SFT will produce system crashing results).
;	DS DOSDATA
; Function:
;	Can be used by an SFT routine (like CLOSE) to invalidate buffers
;	if disk changed.
;	In other respects, same as FATREAD_CDS.
;	(note ES:DI destroyed!)
; Outputs:
;	Carry set if error (currently user FAILed to I 24)
; NOTE: This routine may cause FATREAD_CDS to "miss" a disk change
;	as far as invalidating curdir_ID is concerned.
;	Since getting a true disk changed on this call is a screw up
;	anyway, that's the way it goes.
;
;---------------------------------------------------------------------------

procedure  FATREAD_SFT,NEAR
	DOSAssume   <DS>,"FATRead_SFT"

	LES	BP,ES:[DI.sf_devptr]
	Assert	ISDPB,<ES,BP>,"FatReadSFT"
	MOV	AL,ES:[BP.dpb_drive]
	MOV	[THISDRV],AL
	invoke	GOTDPB			;Set THISDPB
	CALL	FAT_GOT_DPB
	return

EndProc FATREAD_SFT

;----------------------------------------------------------------------------
;
; Procedure Name : FATREAD_CDS
;
; Inputs:
;	DS:DOSDATA
;	ES:DI points to an CDS for the drive of intrest (local only,
;		giving a NET or NUL CDS will produce system crashing results).
; Function:
;	If disk may have been changed, media is determined and buffers are
;	flagged invalid. If not, no action is taken.
; Outputs:
;	ES:BP = Drive parameter block
;	THISDPB = ES:BP
;	THISDRV set
;	Carry set if error (currently user FAILed to I 24)
; DS preserved , all other registers destroyed
;
;---------------------------------------------------------------------------

procedure   FATREAD_CDS,NEAR
	DOSAssume   <DS>,"FATRd_CDS"

	PUSH	ES
	PUSH	DI
	LES	BP,ES:[DI.curdir_devptr]
	Assert	ISDPB,<ES,BP>,"FatReadCDS"
	MOV	AL,ES:[BP.dpb_drive]
	MOV	[THISDRV],AL
	invoke	GOTDPB			;Set THISDPB
	CALL	FAT_GOT_DPB
	POP	DI			;Get back CDS pointer
	POP	ES
	retc
	JNZ	NO_CHANGE		;Media NOT changed

;	Media changed. We now need to find all CDS structures which use this
;	DPB and invalidate their ID pointers.

MED_CHANGE:
	XOR	AX,AX
	DEC	AX			;AX = -1
	PUSH	DS
	MOV	CL,[CDSCOUNT]
	XOR	CH,CH			; CX is number of structures
	LDS	SI,ES:[DI.curdir_devptr] ; Find all CDS with this devptr
ASSUME	DS:NOTHING

;hkn; SS override

;	Find all CDSs with this DevPtr
;
;	(ax) = -1
;	(ds:si) = DevPtr

	LES	DI,CDSADDR		; (es:di) = CDS pointer
frcd20: TESTB	ES:[DI.curdir_flags],curdir_isnet
	JNZ	frcd25			; Leave NET guys alone!!
	cmp	si,word ptr es:[di].curdir_devptr
	jne	frcd25			; no match
	mov	bx,ds
	cmp	bx,word ptr es:[di].curdir_devptr+2
	jne	frcd25			; CDS not for this drive
	test	ES:[DI].curdir_ID,AX
	JZ	frcd25			; If root (0), leave root
	MOV	ES:[DI].curdir_ID,AX	; else invalid
frcd25:	ADD	DI,SIZE curdir_list	; Point to next CDS
	LOOP	frcd20
	POP	DS
	DOSAssume   <DS>,"FrCd25"
NO_CHANGE:
	LES	BP,THISDPB
	CLC
	return

EndProc FATREAD_CDS

Break	<Fat_Operation - miscellaneous fat stuff>

	procedure   FAT_operation,NEAR
FATERR:
	DOSAssume   <DS>,"FATERR"
	MOV	ES:[BP.dpb_free_cnt],-1 ; Err in FAT must force recomp of freespace
	AND	DI,STECODE		; Put error code in DI
	MOV	[ALLOWED],allowed_FAIL + allowed_RETRY
	MOV	AH,2 + allowed_FAIL + allowed_RETRY ; While trying to read FAT
	MOV	AL,BYTE PTR [THISDRV]	 ; Tell which drive
	invoke	FATAL1
	LES	BP,THISDPB
	CMP	AL,3
	JNZ	FAT_GOT_DPB		; User said retry
	STC				; User said FAIL
	return

	public	fat_got_dpb

FAT_GOT_DPB:
	Context DS			;hkn; SS is DOSDATA
	MOV	AL,DMEDHL
	MOV	AH,ES:[BP.dpb_UNIT]
	MOV	WORD PTR [DEVCALL],AX
	MOV	BYTE PTR [DEVCALL.REQFUNC],DEVMDCH
	MOV	[DEVCALL.REQSTAT],0
	MOV	AL,ES:[BP.dpb_media]
	MOV	BYTE PTR [CALLMED],AL
	PUSH	ES
	PUSH	DS

;hkn; DEVCALL is in DOSDATA
	MOV	BX,OFFSET DOSDATA:DEVCALL
	LDS	SI,ES:[BP.dpb_driver_addr]  ; DS:SI Points to device header
ASSUME	DS:NOTHING
	POP	ES			; ES:BX Points to call header
	invoke	DEVIOCALL2
	Context DS		 	;hkn; SS is DOSDATA
	POP	ES			; Restore ES:BP
	MOV	DI,[DEVCALL.REQSTAT]
	.errnz	STERR-8000h
	or	di,di
	js	FATERR			; have error
	XOR	AH,AH
	XCHG	AH,ES:[BP.dpb_first_access] ; Reset dpb_first_access
	MOV	AL,BYTE PTR [THISDRV]	; Use physical unit number
; See if we had changed volume id by creating one on the diskette
	cmp	[VOLCHNG_FLAG],AL
	jnz	CHECK_BYT
	mov	[VOLCHNG_FLAG],-1
	jmp	GOGETBPB		; Need to get device driver to read in
					; new volume label.
CHECK_BYT:
	OR	AH,BYTE PTR [CALLRBYT]
	JNS	CHECK_ZR		; ns = 0 or 1
	JMP	short NEWDSK

CHECK_ZR:
	JZ	CHKBUFFDIRT		; jump if I don't know
	CLC
	return				; If Media not changed (NZ)

DISK_CHNG_ERR:
ASSUME	DS:NOTHING
	PUSH	ES
	PUSH	BP
	LES	BP,ES:[BP.dpb_driver_addr]  ; Get device pointer
	TESTB	ES:[BP.SDEVATT],DEVOPCL ; Did it set vol id?
	POP	BP
	POP	ES
	JZ	FAIL_OPJ2		; Nope, FAIL
	PUSH	DS			; Save buffer pointer for ignore
	PUSH	DI
	Context DS			;hkn; SS is DOSDATA
	MOV	[ALLOWED],allowed_FAIL + allowed_RETRY
	PUSH	ES
	LES	DI,[CALLVIDM]		; Get volume ID pointer
	MOV	WORD PTR [EXTERRPT+2],ES
	POP	ES
	MOV	WORD PTR [EXTERRPT],DI
	MOV	AX,error_I24_wrong_disk
	MOV	[READOP],1		; Write
	invoke	HARDERR
	POP	DI			; Get back buffer for ignore
	POP	DS
ASSUME	DS:NOTHING
	CMP	AL,3
FAIL_OPJ2:
	JZ	FAIL_OP
	JMP	FAT_GOT_DPB		; Retry

CHKBUFFDIRT:
	DOSAssume   <DS>,"ChkBuffDirt"
	assume	DS:NOTHING	; BUGBUG - why this assume after the DOSASSUME?
	cmp	[DirtyBufferCount], 0		; any dirty buffers ? ;hkn;
	je	NEWDSK				; no, skip the check
	call	GetCurHead			; get pointer to first buffer
nbuffer:
	cmp	[di].buf_ID, al			; Unit OK ?
	jne	lfnxt				; no, go for next buffer
	TESTB	[di].buf_flags, buf_dirty	; is the buffer dirty ?
	jz	lfnxt				; no, go for next buffer
;	pop	di				; There is a dirty buffer
;	pop	ds				;  assume Media OK ( NZ )
	Context	DS
	clc
	ret

FAIL_OP:					; This label & code is here
	Context	DS				;  for reachability
	STC
	return

	assume	DS:NOTHING
lfnxt:
	mov	di, [di].buf_next		; get next buffer
	cmp	[FIRST_BUFF_ADDR], di		; is this where we started ?;hkn;
	jne	nbuffer				; no, check this guy also

; If no dirty buffers, assume Media changed
NEWDSK:
	mov	ES:[bp].dpb_free_cnt, -1	; Media changed, must
						;  recompute
	call	GetCurHead
nxbuffer:
	cmp	[di].buf_ID, al			; This drive ?
	jne	lfnxt2
	TESTB	[di].buf_flags, buf_dirty
	DLJNZ	DISK_CHNG_ERR
	mov	word ptr [di].buf_ID, (buf_visit SHL 8) OR 0FFh ; free up
	call	ScanPlace
	jmp	short skpbuff
lfnxt2:
	mov	di, [di].buf_next
skpbuff:
	cmp	di, [FIRST_BUFF_ADDR]					;hkn;
	jne	nxbuffer

	CMP	[SC_CACHE_COUNT],0	;LB.  look ahead buffers ?		  ;AN001;
	JZ	GOGETBPB		;LB.  no				  ;AN001;
	CMP	AL,[CURSC_DRIVE]	;LB.  same as changed drive		  ;AN001;
	JNZ	GOGETBPB		;LB.  no				  ;AN001;
	MOV	[CURSC_DRIVE],-1	;LB.  invalidate look ahead buffers	  ;AN000;
GOGETBPB:
	LDS	DI,ES:[BP.dpb_driver_addr]
	TESTB	[DI.SDEVATT],ISFATBYDEV
	JNZ	GETFREEBUF
	context DS	    		;hkn; SS is DOSDATA
	MOV	BX,2
	CALL	UNPACK			; Read the first FAT sector into CURBUF
FAIL_OPJ:
	JC	FAIL_OP
	LDS	DI,[CURBUF]
ASSUME	DS:NOTHING
	JMP	SHORT GOTGETBUF

GETFREEBUF:
ASSUME	DS:NOTHING
	PUSH	ES			; Get a free buffer for BIOS to use
	PUSH	BP
;	LDS	DI,[BUFFHEAD]
	XOR	DX,DX			     ;LB.  fake to get 1st		  ;AN000;

;hkn; SS override
	MOV	[HIGH_SECTOR],DX	     ;LB.  buffer addr			  ;AN000;
	invoke	GETCURHEAD		     ;LB.				  ;AN000;

	invoke	BUFWRITE
	POP	BP
	POP	ES
	JC	FAIL_OPJ
GOTGETBUF:
	ADD	DI,BUFINSIZ

;hkn; SS override
	MOV	WORD PTR [CALLXAD+2],DS
	Context DS			;hkn; SS is DOSDATA
	MOV	WORD PTR [CALLXAD],DI
	MOV	AL,DBPBHL
	MOV	AH,BYTE PTR ES:[BP.dpb_UNIT]
	MOV	WORD PTR [DEVCALL],AX
	MOV	BYTE PTR [DEVCALL.REQFUNC],DEVBPB
	MOV	[DEVCALL.REQSTAT],0
	MOV	AL,BYTE PTR ES:[BP.dpb_media]
	MOV	[CALLMED],AL
	PUSH	ES
	PUSH	DS
	PUSH	WORD PTR ES:[BP.dpb_driver_addr+2]
	PUSH	WORD PTR ES:[BP.dpb_driver_addr]

;hkn; DEVCALL is in DOSDATA
	MOV	BX,OFFSET DOSDATA:DEVCALL
	POP	SI
	POP	DS			; DS:SI Points to device header
ASSUME	DS:NOTHING
	POP	ES			; ES:BX Points to call header
	invoke	DEVIOCALL2
	POP	ES			; Restore ES:BP
	Context DS		 	;hkn; SS is DOSDATA
	MOV	DI,[DEVCALL.REQSTAT]

	.errnz	STERR-8000h
	or	di,di
	js	FATERRJ 		; have error
	MOV	AL,BYTE PTR ES:[BP.dpb_media]
	LDS	SI,[CALLBPB]
ASSUME	DS:NOTHING
	MOV	ES:[BP].DPB_next_free,0 ; recycle scanning pointer
	invoke	$SETDPB

;hkn; SS override
	LDS	DI,[CALLXAD]		; Get back buffer pointer
	MOV	AL,BYTE PTR ES:[BP.dpb_FAT_count]
	MOV	[DI.buf_wrtcnt-BUFINSIZ],AL   ;>32mb				;AN000;
	MOV	AX,ES:[BP.dpb_FAT_size]       ;>32mb				;AC000;
	MOV	[DI.buf_wrtcntinc-BUFINSIZ],AX	 ;>32mb Correct buffer info	;AC000;

	Context DS			;hkn; SS is DOSDATA
	XOR	AL,AL			;Media changed (Z), Carry clear
	return

FATERRJ: JMP	FATERR

EndProc FAT_operation

DOSCODE	ENDS
	END
