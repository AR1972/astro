	page	,164
	TITLE	ROM - Miscellaneous routines
	NAME	ROM

;
;	Microsoft Confidential
;	Copyright (C) Microsoft Corporation 1991
;	All Rights Reserved.
;

;**	Misc Low level routines for doing simple FCB computations, Cache
;       reads and writes, I/O optimization, and FAT allocation/deallocation
;
;	SKPCLP
;	FNDCLUS
;	BUFSEC
;	BUFRD
;	BUFWRT
;	NEXTSEC
;	OPTIMIZE
;	FIGREC
;	ALLOCATE
;	RESTFATBYT
;	RELEASE
;	RELBLKS
;	GETEOF
;
;	Modification history:
;
;		Created: ARR 30 March 1983
;               M039: DB 10/25/90 - Disk read/write optimization.

	.xlist
	.xcref
	include version.inc
	include dosseg.inc
	include fastxxxx.inc
	INCLUDE DOSSYM.INC
	INCLUDE DEVSYM.INC
	include sf.inc
	include dpb.inc
	.cref
	.list

        i_need  CLUSNUM,WORD
        i_need  NEXTADD,WORD
        i_need  LASTPOS,WORD
        i_need  SECCLUSPOS,BYTE
        i_need  FATBYT,WORD
        i_need  THISSFT,DWORD
        i_need  TRANS,BYTE
        i_need  BYTCNT1,WORD
        i_need  CURBUF,DWORD
        i_need  BYTSECPOS,WORD
        i_need  DMAADD,WORD
        i_need  SECPOS,DWORD                         ;F.C. >32mb
        i_need  VALSEC,DWORD                         ;F.C. >32mb
        i_need  ALLOWED,BYTE

        i_need  HIGH_SECTOR,WORD                     ; DOS 3.4
        i_need  DISK_FULL,BYTE                       ; DOS 3.4
        i_need  Temp_VAR2,WORD                       ; DOS 3.4

        i_need  BufferQueue,DWORD                    ;M039

	i_need  OffsetMagicPatch,WORD		;scottq 8-6-92

DOSCODE SEGMENT
        ASSUME  SS:DOSDATA,CS:DOSCODE


Break   <FNDCLUS -- Skip over allocation units>
;--------------------------------------------------------------------------
;
; Procedure Name : FNDCLUS
;
; Inputs:
;       CX = No. of clusters to skip
;       ES:BP = Base of drive parameters
;       [THISSFT] point to SFT
; Outputs:
;       BX = Last cluster skipped to
;       CX = No. of clusters remaining (0 unless EOF)
;       DX = Position of last cluster
;       Carry set if error (currently user FAILed to I 24)
; DI destroyed. No other registers affected.
;--------------------------------------------------------------------------

procedure FNDCLUS,NEAR
	DOSAssume   <DS>,"FndClus"

        Assert      ISDPB,<ES,BP>,"FndCLus"

	PUSH	ES
        LES     DI,[THISSFT]			; setup addressability to SFT

        Assert      ISSFT,<ES,DI>,"FndClus"

	MOV	BX,ES:[DI.sf_lstclus]
        MOV     DX,ES:[DI.sf_cluspos]
	OR	BX,BX
	JZ	NOCLUS

        SUB     CX,DX
        JNB     FINDIT

        ADD     CX,DX
        XOR     DX,DX
        MOV     BX,ES:[DI.sf_firclus]
FINDIT:
        POP     ES
	JCXZ	RET9

entry   SKPCLP

        invoke  UNPACK
        JC	ret_label		; retc

	xchg	bx,di
	invoke	IsEOF
	xchg	bx,di
	jae	ret9

;	push	bx
;        MOV     BX,DI
;        Invoke  IsEOF
;	pop	bx
;        JAE     RET9

        XCHG    BX,DI
        INC     DX
	LOOP	SKPCLP				; RMFS
RET9:										;AN000;
	CLC
        return
NOCLUS:
        POP     ES
        INC     CX
        DEC     DX
        CLC

ret_label:
        return

EndProc FNDCLUS

Break  <BUFSEC -- BUFFER A SECTOR AND SET UP A TRANSFER>
;--------------------------------------------------------------------------
;
; Procedure Name : BUFSEC
;
; Inputs:
;       AH = priority of buffer
;       AL = 0 if buffer must be read, 1 if no pre-read needed
;       ES:BP = Base of drive parameters
;       [CLUSNUM] = Physical cluster number
;       [SECCLUSPOS] = Sector position of transfer within cluster
;       [BYTCNT1] = Size of transfer
; Function:
;       Insure specified sector is in buffer, flushing buffer before
;       read if necessary.
; Outputs:
;       ES:DI = Pointer to buffer
;       SI = Pointer to transfer address
;       CX = Number of bytes
;       [NEXTADD] updated
;       [TRANS] set to indicate a transfer will occur
;       Carry set if error (user FAILed to I 24)
;--------------------------------------------------------------------------

procedure BUFSEC,NEAR
	DOSAssume   <DS>,"BufSec"

        Assert      ISDPB,<ES,BP>,"BufSec"
        MOV     DX,[CLUSNUM]
        MOV     BL,[SECCLUSPOS]
        MOV     [ALLOWED],allowed_FAIL + allowed_RETRY + allowed_IGNORE
        CALL    FIGREC
        invoke  GETBUFFR
        retc

        MOV     BYTE PTR [TRANS],1      ; A transfer is taking place
        MOV     SI,[NEXTADD]
        MOV     DI,SI
        MOV     CX,[BYTCNT1]
        ADD     DI,CX
        MOV     [NEXTADD],DI
        LES     DI,[CURBUF]
        Assert  ISBUF,<ES,DI>,"BufSec"
        OR      ES:[DI.buf_flags],buf_isDATA
        LEA     DI,[DI].BUFINSIZ        ; Point to buffer
        ADD     DI,[BYTSECPOS]
        CLC
        return
EndProc BUFSEC

Break   <BUFRD, BUFWRT -- PERFORM BUFFERED READ AND WRITE>
;---------------------------------------------------------------------------
;
; Procedure Name : BUFRD
;
; Do a partial sector read via one of the system buffers
; ES:BP Points to DPB
; Carry set if error (currently user FAILed to I 24)
;
; DS - set to DOSDATA
;
;----------------------------------------------------------------------------

procedure BUFRD,NEAR
	DOSAssume   <DS>,"BufRd"

        Assert      ISDPB,<ES,BP>,"BufRd"
        PUSH    ES
        xor	ax, ax			; pre-read sector
        CALL    BUFSEC
        JNC     BUF_OK

BUF_IO_FAIL:				; this label used by BUFWRT also
        POP     ES
        JMP     SHORT RBUFPLACED

BUF_OK:
        MOV     BX,ES
        MOV     ES,[DMAADD+2]
        MOV     DS,BX
	ASSUME  DS:NOTHING
        XCHG    DI,SI
        SHR     CX,1

;M039
;        JNC     EVENRD
;        MOVSB
;EVENRD:
;        REP     MOVSW

;	   CX = # of whole WORDs; CF=1 if odd # of bytes.
;       DS:SI-> Source within Buffer.
;       ES:DI-> Destination within Transfer memory block.

	rep	movsw			;Copy Buffer to Transfer memory.
	adc	cx,0                    ;CX=1 if odd # of bytes, else CX=0.
	rep	movsb                   ;Copy last byte.
;M039

        POP     ES
;hkn; SS override
        LDS     DI,[CURBUF]
        Assert  ISBUF,<DS,DI>,"BufRD/EvenRD"
        LEA     BX,[DI.BufInSiz]
        SUB     SI,BX                   ; Position in buffer
        invoke  PLACEBUF
        Assert  ISDPB,<ES,BP>,"BufRD/EvenRD"
        CMP     SI,ES:[BP.dpb_sector_size] ; Read Last byte?
        JB      RBUFPLACEDC             ; No, leave buf where it is

;M039
;       invoke  PLACEHEAD               ; Make it prime candidate for chucking
                                        ;  even though it is MRU.
        MOV	WORD PTR [BufferQueue],DI ; Make it prime candidate for
;M039                                     ; chucking even though it is MRU.

RBUFPLACEDC:
        CLC
RBUFPLACED:
	Context	<DS>
        return
EndProc BUFRD
;
;----------------------------------------------------------------------------
;
; Procedure : BUFWRT
;
; Do a partial sector write via one of the system buffers
; ES:BP Points to DPB
; Carry set if error (currently user FAILed to I 24)
;
; DS - set to DOSDATA
;
;----------------------------------------------------------------------------
;

procedure BUFWRT,NEAR
	DOSAssume   <DS>,"BufWrt"

	Assert	ISDPB,<ES,BP>,"BufWrt"
        MOV     AX,WORD PTR [SECPOS]
        ADD     AX,1            ; Set for next sector
        MOV     WORD PTR [SECPOS],AX      ;F.C. >32mb                           ;AN000;
        ADC     WORD PTR [SECPOS+2],0     ;F.C. >32mb                           ;AN000;
        MOV     AX,WORD PTR [SECPOS+2]    ;F.C. >32mb                           ;AN000;
        CMP     AX,WORD PTR [VALSEC+2]    ;F.C. >32mb                           ;AN000;
        MOV     AL,1                      ;F.C. >32mb                           ;AN000;
        JA      NOREAD                    ;F.C. >32mb                           ;AN000;
        JB      doread                    ;F.C. >32mb                           ;AN000;
        MOV     AX,WORD PTR [SECPOS]      ;F.C. >32mb                           ;AN000;
        CMP     AX,WORD PTR [VALSEC]     ; Has sector been written before?
        MOV     AL,1
        JA      NOREAD          	 ; Skip preread if SECPOS>VALSEC
doread:
        XOR     AL,AL
NOREAD:
        PUSH    ES
        CALL    BUFSEC
	DLJC	 BUF_IO_FAIL
        MOV     DS,[DMAADD+2]
	ASSUME  DS:NOTHING
        SHR     CX,1

;M039
;       JNC     EVENWRT
;       MOVSB
;EVENWRT:
;       REP     MOVSW

;	   CX = # of whole WORDs; CF=1 if odd # of bytes.
;       DS:SI-> Source within Transfer memory block.
;       ES:DI-> Destination within Buffer.

	rep	movsw			;Copy Transfer memory to Buffer.
	adc	cx,0                    ;CX=1 if odd # of bytes, else CX=0.
	rep	movsb                   ;Copy last byte.
;M039

        POP     ES

;hkn; SS override
        LDS     BX,[CURBUF]
        Assert  ISBUF,<DS,BX>,"BufWrt/NOREAD"

        TEST    [BX.buf_flags],buf_dirty  ;LB. if already dirty                 ;AN000;
        JNZ     yesdirty                  ;LB.    don't increment dirty count   ;AN000;
        invoke  INC_DIRTY_COUNT           ;LB.                                  ;AN000;
        OR      [BX.buf_flags],buf_dirty
yesdirty:
        LEA     SI,[BX.BufInSiz]
        SUB     DI,SI                   ; Position in buffer

;M039
;       MOV     SI,DI
;       MOV     DI,BX
;       invoke  PLACEBUF
;       Assert  ISDPB,<ES,BP>,"BufWrt/EvenWrt"
;       CMP     SI,ES:[BP.dpb_sector_size]  ; Written last byte?
;       JB      WBUFPLACED              ; No, leave buf where it is
;       invoke  PLACEHEAD               ; Make it prime candidate for chucking
                                        ;  even though it is MRU.

        Assert  ISDPB,<ES,BP>,"BufWrt/NOREAD"
        CMP     DI,ES:[BP.dpb_sector_size]  ; Written last byte?
        JB      WBUFPLACED                  ; No, leave buf where it is
        MOV	WORD PTR [BufferQueue],BX   ; Make it prime candidate for
                                            ; chucking even though it is MRU.
;M039

WBUFPLACED:
        CLC
	Context	<DS>
        return
EndProc BUFWRT

Break   <NEXTSEC -- Compute next sector to read or write>
;---------------------------------------------------------------------------
;
; Procedure Name : NEXTSEC
;
; Compute the next sector to read or write
; ES:BP Points to DPB
;
;---------------------------------------------------------------------------

procedure NEXTSEC,NEAR
	DOSAssume   <DS>,"NextSec"

	Assert	ISDPB,<ES,BP>,"NextSec"
	test	byte ptr Trans, -1 
        JZ      CLRET

        MOV     AL,[SECCLUSPOS]
        INC     AL
        CMP     AL,ES:[BP.dpb_cluster_mask]
        JBE     SAVPOS

        MOV     BX,[CLUSNUM]
        Invoke  IsEOF
        JAE     NONEXT

        invoke  UNPACK
        JC      NONEXT
clusgot:
        MOV     [CLUSNUM],DI
        INC     [LASTPOS]
        MOV     AL,0
SAVPOS:
        MOV     [SECCLUSPOS],AL
CLRET:
        CLC
        return
NONEXT:
        STC
        return
EndProc NEXTSEC

Break   <OPTIMIZE -- DO A USER DISK REQUEST WELL>
;----------------------------------------------------------------------------
;
; Procedure Name : OPTIMIZE
;
; Inputs:
;       BX = Physical cluster
;       CX = No. of records
;       DL = sector within cluster
;       ES:BP = Base of drives parameters
;       [NEXTADD] = transfer address
; Outputs:
;       AX = No. of records remaining
;       BX = Transfer address
;       CX = No. or records to be transferred
;       DX = Physical sector address            (LOW)
;       [HIGH_SECTOR] = Physical sector address (HIGH)
;       DI = Next cluster
;       [CLUSNUM] = Last cluster accessed
;       [NEXTADD] updated
;       Carry set if error (currently user FAILed to I 24)
; ES:BP unchanged. Note that segment of transfer not set.
;
;---------------------------------------------------------------------------

procedure OPTIMIZE,NEAR
	DOSAssume   <DS>,"Optimize"

	Assert	ISDPB,<ES,BP>,"Optimize"
        PUSH    DX
        PUSH    BX
        MOV     AL,ES:[BP.dpb_cluster_mask]
        INC     AL              ; Number of sectors per cluster
        MOV     AH,AL
        SUB     AL,DL           ; AL = Number of sectors left in first cluster
        MOV     DX,CX
        MOV     CX,0
OPTCLUS:
; AL has number of sectors available in current cluster
; AH has number of sectors available in next cluster
; BX has current physical cluster
; CX has number of sequential sectors found so far
; DX has number of sectors left to transfer
; ES:BP Points to DPB
; ES:SI has FAT pointer

do_norm3:
        invoke  UNPACK
        JC      OP_ERR
clusgot2:
	ADD	CL,AL
        ADC     CH,0
        CMP     CX,DX
        JAE     BLKDON
        MOV     AL,AH
        INC     BX
        CMP     DI,BX
        JZ      OPTCLUS
        DEC     BX
FINCLUS:
        MOV     [CLUSNUM],BX    ; Last cluster accessed
        SUB     DX,CX           ; Number of sectors still needed
        PUSH    DX
        MOV     AX,CX
        MUL     ES:[BP.dpb_sector_size]  ; Number of sectors times sector size
        MOV     SI,[NEXTADD]
        ADD     AX,SI           ; Adjust by size of transfer
        MOV     [NEXTADD],AX
        POP     AX              ; Number of sectors still needed
        POP     DX              ; Starting cluster
        SUB     BX,DX           ; Number of new clusters accessed
        ADD     [LASTPOS],BX
        POP     BX              ; BL = sector postion within cluster
        invoke  FIGREC
        MOV     BX,SI
	CLC
        return

OP_ERR:
        ADD     SP,4
	STC
        return

BLKDON:
        SUB     CX,DX           ; Number of sectors in cluster we don't want
        SUB     AH,CL           ; Number of sectors in cluster we accepted
        DEC     AH              ; Adjust to mean position within cluster
        MOV     [SECCLUSPOS],AH
        MOV     CX,DX           ; Anyway, make the total equal to the request
        JMP     SHORT FINCLUS
EndProc OPTIMIZE

Break   <FIGREC -- Figure sector in allocation unit>
;---------------------------------------------------------------------------
;
; Procedure Name : FIGREC
;
; Inputs:
;       DX = Physical cluster number
;       BL = Sector postion within cluster
;       ES:BP = Base of drive parameters
; Outputs:
;       DX = physical sector number           (LOW)
;       [HIGH_SECTOR] Physical sector address (HIGH)
; No other registers affected.
;
;---------------------------------------------------------------------------

procedure	FIGREC,NEAR

        Assert      ISDPB,<ES,BP>,"FigRec"
        PUSH    CX
        MOV     CL,ES:[BP.dpb_cluster_shift]
        DEC     DX
        DEC     DX

;hkn; SS override HIGH_SECTOR
        MOV     [HIGH_SECTOR],0              ;F.C. >32mb
        OR      CL,CL                        ;F.C. >32mb
        JZ      noshift                      ;F.C. >32mb
        XOR     CH,CH                        ;F.C. >32mb
rotleft:                                     ;F.C. >32mb
        CLC                                  ;F.C. >32mb
        RCL     DX,1                         ;F.C. >32mb
        RCL     [HIGH_SECTOR],1              ;F.C. >32mb
        LOOP    rotleft                      ;F.C. >32mb
noshift:

;       SHL     DX,CL
        OR      DL,BL
        ADD     DX,ES:[BP.dpb_first_sector]
        ADC     [HIGH_SECTOR],0              ;F.C. >32mb
        POP     CX
        return
EndProc FIGREC

Break   <ALLOCATE -- Assign disk space>
;---------------------------------------------------------------------------
;
; Procedure Name : ALLOCATE - Allocate Disk Space
;
;   ALLOCATE is called to allocate disk clusters.  The new clusters are
;   FAT-chained onto the end of the existing file.
;
;   The DPB contains the cluster # of the last free cluster allocated
;   (dpb_next_free).  We start at this cluster and scan towards higher
;   numbered clusters, looking for the necessary free blocks.
;
;   Once again, fancy terminology gets in the way of corrct coding.  When
;   using next_free, start scanning AT THAT POINT and not the one following it.
;   This fixes the boundary condition bug when only free = next_free = 2.
;
;       If we get to the end of the disk without satisfaction:
;
;           if (dpb_next_free == 2) then we've scanned the whole disk.
;               return (insufficient_disk_space)
;           ELSE
;               dpb_next_free = 2; start scan over from the beginning.
;
;   Note that there is no multitasking interlock.  There is no race when
;   examining the entrys in an in-core FAT block since there will be no
;   context switch.  When UNPACK context switches while waiting for a FAT read
;   we are done with any in-core FAT blocks, so again there is no race.  The
;   only special concern is that V2 and V3 MSDOS left the last allocated
;   cluster as "00"; marking it EOF only when the entire alloc request was
;   satisfied.  We can't allow another activation to think this cluster is
;   free, so we give it a special temporary mark to show that it is, indeed,
;   allocated.
;
;   Note that when we run out of space this algorithem will scan from
;   dpb_next_free to the end, then scan from cluster 2 through the end,
;   redundantly scanning the later part of the disk.  This only happens when
;   we run out of space, so sue me.
;
;----+----+----+----+----+----+----+----+----+----+----+----+----+----+----;
;            C  A  V  E  A  T     P  A  T  T  E  R  S  O  N                ;
;                                                                          ;
;   The use of FATBYT and RESTFATBYT is somewhat mysterious.  Here is the
;   explanation:
;
;   In the NUL file case (sf_firclus currently 0) ALLOCATE is called with
;   entry BX = 0.  What needs to be done in this case is to stuff the cluster
;   number of the first cluster allocated in sf_firclus when the ALLOCATE is
;   complete.  THIS VALUE IS SAVED TEMPORARILY IN CLUSTER 0, HENCE THE CURRENT
;   VALUE IN CLUSTER 0 MUST BE SAVED AND RESTORED.  This is a side effect of
;   the fact that PACK and UNPACK don't treat requests for clusters 0 and 1 as
;   errors.  This "stuff" is done by the call to PACK which is right before
;   the
;           LOOP    findfre         ; alloc more if needed
;   instruction when the first cluster is allocated to the nul file.  The
;   value is recalled from cluster 0 and stored at sf_firclus at ads4:
;
;   This method is obviously useless (because it is non-reentrant) for
;   multitasking, and will have to be changed.  Storing the required value on
;   the stack is recommended.  Setting sf_firclus at the PACK of cluster 0
;   (instead of actually doing the PACK) is BAD because it doesn't handle
;   problems with INT 24 well.
;
;            C  A  V  E  A  T     P  A  T  T  E  R  S  O  N                ;
;----+----+----+----+----+----+----+----+----+----+----+----+----+----+----;
;                                                                          ;
;       ENTRY   BX = Last cluster of file (0 if null file)
;               CX = No. of clusters to allocate
;               ES:BP = Base of drive parameters
;               [THISSFT] = Points to SFT
;
;       EXIT    'C' set if insufficient space
;                 [FAILERR] can be tested to see the reason for failure
;                 CX = max. no. of clusters that could be added to file
;               'C' clear if space allocated
;                 BX = First cluster allocated
;                 FAT is fully updated
;                 sf_FIRCLUS field of SFT set if file was null
;
;       USES    ALL but SI, BP

callmagic  proc near
        push    ds                              ;push segment of routine 
        push    OffsetMagicPatch                ;push offset for routine
        retf                                    ;simulate jmp far
                                                ;far return address is on
                                                ;stack, so far return from
                                                ;call will return this routine
callmagic  endp

PROCEDURE ALLOCATE,NEAR

	DOSAssume   <DS>,"Allocate"

	Assert	ISDPB,<ES,BP>,"Allocate"
;BEGIN MAGICDRV MODIFICATIONS
;
;7/5/92 scottq
;
;This is the disk compression patch location which allows
;the disk compression software to fail allocations if the
;FAT would allows allocation, but the free space for compressed
;data would not.
;        
;;;        call    far ptr MAGICPATCH
;;; We cannot do a far call since we cannot have fix-ups[romdos,hidos],
;;; but we do know the segment and offset of the routine
;;; so simulate a far call to dosdata:magicpatch
;;; note dosassume above, so DS -> dosdata
        clc                                     ;clear carry so we fall through
                                                ;if no patch is present
	push	cs				;push segment for far return
        call    callmagic                       ;this is a near call
        jnc     Regular_Allocate_Path
        jmp     Disk_Full_Return
Regular_Allocate_Path:
;END MAGICDRV MODIFICATIONS

        PUSH    BX                      ; save (bx)
        XOR     BX,BX
        invoke  UNPACK
        MOV     [FATBYT],DI             ; save correct cluster 0 value
        POP     BX
        retc                            ; abort if error   [INTERR?]

        PUSH    CX
        PUSH    BX

        MOV     DX,BX
        Assert      ISDPB,<ES,BP>,"Allocate/Unpack"
        mov     bx,es:[bp.dpb_next_free]
        cmp     bx,2
        ja      findfre

;   couldn't find enough free space beyond dpb_next_free, or dpb_next_free is
;   <2 or >dpb_max_clus.  Reset it and restart the scan

ads1:
        Assert      ISDPB,<ES,BP>,"Alloc/ads1"
        mov     es:[bp.dpb_next_free],2
        mov     bx,1                    ; Counter next instruction so first
                                        ;       cluster examined is 2

;   Scanning both forwards and backwards for a free cluster
;
;       (BX) = forwards scan pointer
;       (CX) = clusters remaining to be allocated
;       (DX) = current last cluster in file
;       (TOS) = last cluster of file

FINDFRE:
        INC     BX
        Assert      ISDPB,<ES,BP>,"Alloc/findfre"
        CMP     BX,ES:[BP.dpb_max_cluster]

;hkn; bad code! replace with ja.
	ja      ads7            ; at end of disk

;        JBE     aupk
;        jmp     ads7            ; at end of disk
;aupk:

        invoke  UNPACK          ; check out this cluster
        jc      ads4            ; FAT error             [INTERR?]
        jnz     findfre         ; not free, keep on truckin

;   Have found a free cluster.  Chain it to the file
;
;       (BX) = found free cluster #
;       (DX) = current last cluster in file

        mov     es:[bp.dpb_next_free],bx        ; next time start search here
        xchg    ax,dx           ; save (dx) in ax
        mov     dx,1            ; mark this free guy as "1"
        invoke  PACK            ; set special "temporary" mark
        jc      ads4            ; FAT error             [INTERR?]
        CMP     ES:[BP.dpb_free_cnt],-1 ; Free count valid?
        JZ      NO_ALLOC                ; No
        DEC     ES:[BP.dpb_free_cnt]    ; Reduce free count by 1
NO_ALLOC:
        xchg    ax,dx           ; (dx) = current last cluster in file
        XCHG    BX,DX
        MOV     AX,DX
        invoke  PACK            ; link free cluster onto file
                                ;  CAVEAT.. On Nul file, first pass stuffs
                                ;    cluster 0 with FIRCLUS value.
        jc      ads4            ; FAT error             [INTERR?]
        xchg    BX,AX           ; (BX) = last one we looked at
        mov     dx,bx           ; (dx) = current end of file
        LOOP    findfre         ; alloc more if needed

;   We've successfully extended the file.  Clean up and exit
;
;       (BX) = last cluster in file

        MOV     DX,0FFFFH
        invoke  PACK            ; mark last cluster EOF

;   Note that FAT errors jump here to clean the stack and exit.  this saves us
;   2 whole bytes.  Hope its worth it...
;
;       'C' set iff error
;       calling (BX) and (CX) pushed on stack

ads4:   POP     BX
        POP     CX              ; Don't need this stuff since we're successful
        retc
        invoke  UNPACK          ; Get first cluster allocated for return
                                ; CAVEAT... In nul file case, UNPACKs cluster 0.
        retc
        invoke  RESTFATBYT      ; Restore correct cluster 0 value
        retc
        XCHG    BX,DI           ; (DI) = last cluster in file upon our entry
        OR      DI,DI           ; clear 'C'
        retnz                   ; we were extending an existing file

;   We were doing the first allocation for a new file.  Update the SFT cluster
;   info
dofastk:
        PUSH    DX
        MOV     DL,ES:[BP.dpb_drive]              ; get drive #

        PUSH    ES
        LES     DI,[THISSFT]
        Assert  ISSFT,<ES,DI>,"Allocate/ads4"
        MOV     ES:[DI.sf_firclus],BX
        MOV     ES:[DI.sf_lstclus],BX
do_norm5:
        POP     ES
        POP     DX
        return


;** we're at the end of the disk, and not satisfied.  See if we've scanned ALL
;   of the disk...

ads7:   cmp     es:[bp.dpb_next_free],2
	DLJNZ	ads1		; start scan from front of disk

;   Sorry, we've gone over the whole disk, with insufficient luck.  Lets give
;   the space back to the free list and tell the caller how much he could have
;   had.  We have to make sure we remove the "special mark" we put on the last
;   cluster we were able to allocate, so it doesn't become orphaned.
;
;       (CX) = clusters remaining to be allocated
;       (TOS) = last cluster of file (before call to ALLOCATE)
;       (TOS+1) = # of clusters wanted to allocate


        POP     BX              ; (BX) = last cluster of file
        MOV     DX,0FFFFH
        invoke  RELBLKS         ; give back any clusters just alloced
        POP     AX              ; No. of clusters requested
                                ; Don't "retc". We are setting Carry anyway,
                                ;   Alloc failed, so proceed with return CX
                                ;   setup.
        SUB     AX,CX           ; AX=No. of clusters allocated
        invoke  RESTFATBYT      ; Don't "retc". We are setting Carry anyway,
                                ;   Alloc failed.
Disk_Full_Return:               ;label added for magic patch 8-6-92 scottq
;       fmt     <>,<>,<"$p: disk full in allocate\n">
        MOV     [DISK_FULL],1   ;MS. indicating disk full
        STC
        return

EndProc ALLOCATE

;-----------------------------------------------------------------------
;
; Procedure Name : RESTFATBYT
;
; SEE ALLOCATE CAVEAT
;       Carry set if error (currently user FAILed to I 24)
;-----------------------------------------------------------------------

procedure RESTFATBYT,NEAR
	DOSAssume   <DS>,"RestFATByt"

        PUSH    BX
        PUSH    DX
        PUSH    DI
        XOR     BX,BX
        MOV     DX,[FATBYT]
        invoke  PACK
        POP     DI
        POP     DX
        POP     BX
        return

EndProc RESTFATBYT


Break   <RELEASE -- DEASSIGN DISK SPACE>
;---------------------------------------------------------------------------
;
; Procedure Name : RELEASE
;
; Inputs:
;       BX = Cluster in file
;       ES:BP = Base of drive parameters
; Function:
;       Frees cluster chain starting with [BX]
;       Carry set if error (currently user FAILed to I 24)
; AX,BX,DX,DI all destroyed. Other registers unchanged.
;
;-----------------------------------------------------------------------------

procedure RELEASE,NEAR
	DOSAssume   <DS>,"Release"

        XOR     DX,DX
entry   RELBLKS
	DOSAssume   <DS>,"RelBlks"
        Assert      ISDPB,<ES,BP>,"RelBlks"

;   Enter here with DX=0FFFFH to put an end-of-file mark in the first cluster
;   and free the rest in the chain.

        invoke  UNPACK
	jc	RELEASE_flush
	jz	RELEASE_flush
        MOV     AX,DI
        PUSH    DX
        invoke  PACK
        POP     DX
	jc	RELEASE_flush
        OR      DX,DX
        JNZ     NO_DEALLOC              ; Was putting EOF mark
        CMP     ES:[BP.dpb_free_cnt],-1 ; Free count valid?
        JZ      NO_DEALLOC              ; No
        INC     ES:[BP.dpb_free_cnt]    ; Increase free count by 1
NO_DEALLOC:
        MOV     BX,AX
        dec     ax              ; check for "1"
	jz	RELEASE_flush	; is last cluster of incomplete chain
        Invoke  IsEOF
        JB      RELEASE         ; Carry clear if JMP not taken

RELEASE_flush:
	MOV	AL,ES:[BP.dpb_drive]
	push	si		; FLUSHBUF may trash these and we guarantee
	push	cx		;  them to be preserved.
	push	es
	push	bp
	invoke	FLUSHBUF		; commit buffers for this drive
	pop	bp
	pop	es
	pop	cx
	pop	si
RET12:
	return
EndProc RELEASE

Break   <GETEOF -- Find the end of a file>
;------------------------------------------------------------------------
;
; Procedure Name : GETEOF
;
; Inputs:
;       ES:BP Points to DPB
;       BX = Cluster in a file
;       DS = CS
; Outputs:
;       BX = Last cluster in the file
;       Carry set if error (currently user FAILed to I 24)
; DI destroyed. No other registers affected.
;
;--------------------------------------------------------------------------

procedure GETEOF,NEAR
	DOSAssume   <DS>,"GetEOF"

	Assert	ISDPB,<ES,BP>,"GetEof"
        invoke  UNPACK
        retc
        PUSH    BX
        MOV     BX,DI
        Invoke  IsEOF
        POP     BX
        JAE     RET12           ; Carry clear if jmp
        MOV     BX,DI
        JMP     GETEOF

EndProc GETEOF

DOSCODE ENDS
	END
