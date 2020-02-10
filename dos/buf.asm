	TITLE	BUF - MSDOS buffer management
	NAME	BUF
;
;	Microsoft Confidential
;	Copyright (C) Microsoft Corporation 1991
;	All Rights Reserved.
;

;**	BUF.ASM - Low level routines for buffer cache management
;
;	GETCURHEAD
;	ScanPlace
;	PLACEBUF
;	PLACEHEAD
;	PointComp
;	GETBUFFR
;	GETBUFFRB
;	FlushBuf
;	BufWrite
;	SET_RQ_SC_PARMS
;
;	Revision history:
;
;		AN000  version 4.00  Jan. 1988
;		A004   PTM 3765 -- Disk reset failed
;		M039 DB 10/17/90 - Disk write optimization
;		I001   5.0 PTR 722211 - Preserve CY when in buffer in HMA


	.xlist
	.xcref
	include version.inc
	INCLUDE dosseg.inc
	INCLUDE DOSSYM.INC
	include dpb.inc
	INCLUDE DEVSYM.INC
	.cref
	.list

Installed = TRUE

	i_need	PreRead,WORD
	i_need	LastBuffer,DWORD
	i_need	CurBuf,DWORD
	i_need	WPErr,BYTE
	i_need	ALLOWED,BYTE
	i_need	FAILERR,BYTE
	i_need	HIGH_SECTOR,WORD	     ; DOS 4.00 >32mb			;AN000;
	i_need	BufferQueue,DWORD
	i_need	DirtyBufferCount,WORD
	i_need	SC_CACHE_PTR,DWORD	     ; DOS 4.00 seconadary cache table	;AN000;
	i_need	SC_CACHE_COUNT,WORD	     ; DOS 4.00 secondary cache entries	;AN000;
	i_need	SC_SECTOR_SIZE,WORD	     ; DOS 4.00 sector size		;AN000;
	i_need	SC_DRIVE,BYTE		     ; DOS 4.00 drive			;AN000;
	i_need	DOS34_FLAG,WORD 	     ; DOS 4.00 common flag		;AN000;
	i_need	FIRST_BUFF_ADDR,WORD	     ; DOS 4.00 beginning of the chain	;AN000;

	i_need	BuffInHMA,byte
	i_need	LoMemBuff,dword

DOSCODE SEGMENT
	ASSUME	SS:DOSDATA,CS:DOSCODE

Break	<GETCURHEAD -- Get current buffer header>

;----------------------------------------------------------------------------
; Procedure Name : GetCurHead
; Inputs:
;	 No Inputs
; Function:
;	Returns the pointer to the first buffer in Queue
;	and updates FIRST_BUFF_ADDR
;       and invalidates LASTBUFFER (recency pointer)
; Outputs:
;	DS:DI = pointer to the first buffer in Queue
;	FIRST_BUFF_ADDR = offset ( DI ) of First buffer in Queue
;       LASTBUFFER = -1
; No other registers altered
;----------------------------------------------------------------------------


procedure   GETCURHEAD,NEAR

	lds	di, BufferQueue		; Pointer to the first buffer;smr;SS Override
	mov	word ptr [LASTBUFFER],-1; invalidate last buffer;smr;SS Override
	mov	[FIRST_BUFF_ADDR],di	;save first buffer addr;smr;SS Override
	ret

EndProc GETCURHEAD

Break	<SCANPLACE, PLACEBUF -- PUT A BUFFER BACK IN THE POOL>

;----------------------------------------------------------------------------
; Procedure Name : ScanPlace
; Inputs:
;	Same as PLACEBUF
; Function:
;	Save scan location and call PLACEBUF
; Outputs:
;	DS:DI Points to saved scan location
; All registers, except DS:DI, preserved.
;----------------------------------------------------------------------------
;M039: Rewritten to preserve registers.

procedure   ScanPlace,near

	push	[di].buf_next		;Save scan location
	call	PLACEBUF
	pop     di
	ret

EndProc ScanPlace

;----------------------------------------------------------------------------
; Procedure Name : PlaceBuf
; Input:
;	DS:DI points to buffer (DS->BUFFINFO array, DI=offset in array)
; Function:
;	Remove buffer from queue and re-insert it in proper place.
; NO registers altered
;----------------------------------------------------------------------------

procedure   PLACEBUF,NEAR
	push	AX			;Save only regs we modify	;AN000;
	push	BX							;AN000;
	push	SI							;AN000;
	mov	ax, [di].BUF_NEXT
	mov	bx, word ptr[BufferQueue]	; bx = offset of head of list;smr;SS Override

	cmp	ax,bx				;Buf = last?		;AN000;
	je	nret				;Yes, special case	;AN000;
	cmp	di,bx				;Buf = first?		;AN000;
	jne	not_first 			;Yes, special case	;AN000;
	mov	word ptr [BufferQueue],ax	;smr;SS Override
	jmp	short nret 			;Continue with repositioning;AN000;
not_first:
	mov	SI,[DI].BUF_PREV		;No, SI = prior Buf	;AN000;
	mov	[SI].BUF_NEXT,AX		; ax has di->buf_next	;AN000;
	xchg	si, ax
	mov	[SI].BUF_PREV,AX		;			;AN000;

	mov	SI,[BX].BUF_PREV		;SI-> last buffer	;AN000;
	mov	[SI].BUF_NEXT,DI		;Add Buf to end of list ;AN000;
	mov	[BX].BUF_PREV,DI					;AN000;
	mov	[DI].BUF_PREV,SI		;Update link in Buf too	;AN000;
	mov	[DI].BUF_NEXT,BX					;AN000;
nret:									;AN000;
	pop	SI							;AN000;
	pop	BX							;AN000;
	pop	AX							;AN000;
									;AN000;
	cmp	[di.buf_ID],-1			; Buffer FREE?		;AN000;
        jne     pbx                             ; M039: -no, jump.
	mov	WORD PTR [BufferQueue],di	; M039: -yes, make it LRU.
pbx:	ret								;AN000;

EndProc PLACEBUF

;M039 - Removed PLACEHEAD.
;----------------------------------------------------------------------------
; places buffer at head
;  NOTE:::::: ASSUMES THAT BUFFER IS CURRENTLY THE LAST
;	ONE IN THE LIST!!!!!!!
; BUGBUG ---- this routine can be removed because it has only
; BUGBUG ---- one instruction. This routine is called from
; BUGBUG ---- 3 places. ( Size = 3*3+6 = 15 bytes )
; BUGBUG ---- if coded in line = 3 * 5 = 15 bytes
; BUGBUG ---- But kept as it is for modularity
;----------------------------------------------------------------------------
;procedure   PLACEHEAD,NEAR
;	mov	word ptr [BufferQueue], di
;	ret
;EndProc PLACEHEAD
;M039

Break	<POINTCOMP -- 20 BIT POINTER COMPARE>
;----------------------------------------------------------------------------
;
; Procedure Name : PointComp
; Inputs:
;         DS:SI & ES:DI
; Function:
;          Checks for ((SI==DI) && (ES==DS))
;	   Assumes that pointers are normalized for the
;	   same segment
;
; Compare DS:SI to ES:DI (or DS:DI to ES:SI) for equality
; DO NOT USE FOR < or >
; No Registers altered
;
;----------------------------------------------------------------------------

procedure   PointComp,NEAR

	CMP	SI,DI
	jnz	ret_label	; return if nz
	PUSH	CX
	PUSH	DX
	MOV	CX,DS
	MOV	DX,ES
	CMP	CX,DX
	POP	DX
	POP	CX

ret_label:
	return

EndProc PointComp

	Break	<GETBUFFR, GETBUFFRB -- GET A SECTOR INTO A BUFFER>


;**	GetBuffr - Get a non-FAT Sector into a Buffer
;
;	GetBuffr does normal ( non-FAT ) sector buffering
;	It gets the specified local sector into one of the I/O buffers
;	and shuffles the queue
; 
;	ENTRY	(AL) = 0 means sector must be pre-read
;		       ELSE no pre-read
;		(DX) = Desired physical sector number	      (LOW)
;		HIGH_SECTOR = Desired physical sector number (HIGH)
;		(ES:BP) = Pointer to drive parameters
;		ALLOWED set in case of INT 24
;	EXIT	'C' set if error (user FAIL response to INT24)
;		'C' clear if OK
;		CURBUF Points to the Buffer for the sector
;		    the buffer type bits OF buf_flags = 0, caller must set it
;	USES	AX, BX, CX, SI, DI, Flags


;**	GetBuffrb - Get a FAT Sector into a Buffer
;
;	GetBuffr reads a sector from the FAT file system's FAT table.
;	It gets the specified sector into one of the I/O buffers
;	and shuffles the queue.  We need a special entry point so that
;	we can read the alternate FAT sector if the first read fails, also
;	so we can mark the buffer as a FAT sector.
; 
;	ENTRY	(AL) = 0 means sector must be pre-read
;		       ELSE no pre-read
;		(DX) = Desired physical sector number	      (LOW)
;		(SI) != 0
;		HIGH_SECTOR = Desired physical sector number (HIGH)
;		(ES:BP) = Pointer to drive parameters
;		ALLOWED set in case of INT 24
;	EXIT	'C' set if error (user FAIL response to INT24)
;		'C' clear if OK
;		CURBUF Points to the Buffer for the sector
;		    the buffer type bits OF buf_flags = 0, caller must set it
;	USES	AX, BX, CX, SI, DI, Flags


procedure   GETBUFFR,NEAR
	DOSAssume   <DS>,"GetBuffr"

	XOR	SI,SI

;	This entry point is called for FAT buffering with SI != 0

entry	GETBUFFRB

	Assert	ISDPB,<ES,BP>,"GetBuffr"
	MOV	PREREAD,AX			; save pre-read flag
	MOV	AL,ES:[BP].DPB_DRIVE
	LDS	DI,LASTBUFFER			; Get the recency pointer
ASSUME	DS:NOTHING

;hkn; SS override
	MOV	CX,HIGH_SECTOR			; F.C. >32mb		;AN000;

;	See if this is the buffer that was most recently returned.
;	A big performance win if it is.

	CMP	DI,-1				; Recency pointer valid?
	je	getb5				; No
	CMP	DX,WORD PTR [DI].BUF_SECTOR
	JNZ	getb5				; Wrong sector
	CMP	CX,WORD PTR [DI].BUF_SECTOR+2	; F.C. >32mb		;AN000;
	JNZ	getb5				; F.C. >32mb		;AN000;
	CMP	AL,[DI.buf_ID]
	LJZ	getb35				; Just asked for same buffer

;	It's not the buffer most recently returned.  See if it's in the
;	cache.
;
;	(cx:dx) = sector #
;	(al) = drive #
;	(si) = 0 iff non fat sector, != 0 if FAT sector read
;	??? list may be incomplete ???

getb5:	CALL	GETCURHEAD			; get Q Head
getb10:	CMP	DX,WORD PTR [DI].BUF_SECTOR
	jne	getb12				; wrong sector lo
	CMP	CX,WORD PTR [DI].BUF_SECTOR+2
	jne	getb12				; wrong sector hi
	CMP	AL,[DI.buf_ID]
	jne	@f
	jmp	getb25				; Found the requested sector
@@:
getb12:	mov	DI,[DI].BUF_NEXT
	cmp	DI,FIRST_BUFF_ADDR		; back at the front again?
	jne	getb10				; no, continue looking

;	The requested sector is not available in the buffers. DS:DI now points
;	to the first buffer in the Queue. Flush the first buffer & read in the
;	new sector into it.
;
;	BUGBUG - what goes on here?  Isn't the first guy the most recently
;	used guy?  Shuld be for fast lookup.  If he is, we shouldn't take
;	him, we should take LRU.  And the above lookup shouldn't be
;	down a chain, but should be hashed.
;
;	(DS:DI) = first buffer in the queue
;	(CX:DX) = sector # we want
;	(si) = 0 iff non fat sector, != 0 if FAT sector read

;hkn; SS override
	PUSH	cx
	SAVE	<si, dx, bp, es>
	CALL	BUFWRITE			; Write out the dirty buffer
	RESTORE <es, bp, dx, si>
	POP	HIGH_SECTOR
	jnc	@f
	jmp	getbx				; if got hard error
@@:
	CALL	SET_RQ_SC_PARMS 		; set parms for secondary cache

;	We're ready to read in the buffer, if need be.  If the caller
;	wanted to just *write* the buffer then we'll skip reading it in.

	XOR	AH,AH				; initial flags
;hkn; SS override
	CMP	BYTE PTR PREREAD,ah		; am to Read in the new sector?
	JNZ	getb20				; no, we're done
	LEA	BX,[DI].BufInSiz		; (ds:bx) = data address
	MOV	CX,1
	SAVE	<si, di, dx, es>
; Note:  As far as I can tell, all disk reads into buffers go through
;	 this point.  -mrw 10/88
	cmp	BuffInHMA, 0		; is buffers in HMA?
	jz	@f
	push	ds
	push	bx
	lds	bx, dword ptr LoMemBuff	; Then let's read it into scratch buff
@@:
;M039: Eliminated redundant HMA code.
	OR	SI,SI			; FAT sector ?
	JZ	getb15

	invoke	FATSECRD
	MOV	AH,buf_isFAT		; Set buf_flags
	JMP	SHORT getb17		; Buffer is marked free if read barfs

getb15:
	invoke	DREAD			; Buffer is marked free if read barfs
	MOV	AH,0			; Set buf_flags to no type, DO NOT XOR!
getb17:									  ;I001
	pushf								  ;I001
	cmp	BuffInHMA, 0		; did we read into scratch buff ? ;I001
	jz	not_in_hma		; no				  ;I001
	mov	cx, es:[bp].DPB_SECTOR_SIZE				  ;I001
	shr	cx, 1							  ;I001
	popf				; Retreive possible CY from DREAD ;I001
	mov	si, bx							  ;I001
	pop	di							  ;I001
	pop	es							  ;I001
	cld								  ;I001
	pushf				; Preserve possible CY from DREAD ;I001
	rep	movsw			; move the contents of scratch buf;I001
	push	es							  ;I001
	pop	ds							  ;I001
not_in_hma:								  ;I001
	popf								  ;I001
	RESTORE <es, dx, di, si>
	JC	getbx

;	The buffer has the data setup in it (if we were to read)
;	Setup the various buffer fields
;
;	(ds:di) = buffer address
;	(es:bp) = DPB address
;	(HIGH_SECTOR:DX) = sector #
;	(ah) = BUF_FLAGS value
;	(si) = 0 iff non fat sector, != 0 if FAT sector read

;hkn; SS override
getb20:	MOV	CX,HIGH_SECTOR
	MOV	WORD PTR [DI].BUF_SECTOR+2,CX
	MOV	WORD PTR [DI].BUF_SECTOR,DX
	MOV	WORD PTR [DI].buf_DPB,BP
	MOV	WORD PTR [DI].buf_DPB+2,ES
	MOV	AL,ES:[BP].DPB_DRIVE
	FOLLOWS	BUF_FLAGS, BUF_ID, 1
	MOV	WORD PTR [DI].BUF_ID,AX		; Set ID and Flags
getb25:	MOV	[DI].BUF_WRTCNT,1		; Default to not a FAT sector	;AC000;
	XOR	AX,AX
	OR	SI,SI				; FAT sector ?
	JZ	getb30
	MOV	AL,ES:[BP].DPB_FAT_COUNT	; update number of copies of
	MOV	[DI.buf_WRTCNT],AL		;  this sector present on disk
	MOV	AX,ES:[BP].DPB_FAT_SIZE		; offset of identical FAT
						;  sectors
;	BUGBUG - dos 6 can clean this up by not setting wrtcntinc unless wrtcnt
;		is set

getb30:	MOV	[DI.buf_wrtcntinc],AX
	CALL	PLACEBUF

;hkn; SS override for next 4
getb35: MOV	WORD PTR CURBUF+2,DS
	MOV	WORD PTR LASTBUFFER+2,DS
	MOV	WORD PTR CURBUF,DI
	MOV	WORD PTR LASTBUFFER,DI
	CLC

;	Return with 'C' set appropriately
;
;	(dx) = caller's original value

getbx:	Context DS
	return

EndProc GETBUFFR

Break	<FLUSHBUF -- WRITE OUT DIRTY BUFFERS>

;----------------------------------------------------------------------------
; Input:
;	DS = DOSGROUP
;	AL = Physical unit number local buffers only
;	   = -1 for all units and all remote buffers
; Function:
;	Write out all dirty buffers for unit, and flag them as clean
;	Carry set if error (user FAILed to I 24)
;	    Flush operation completed.
; DS Preserved, all others destroyed (ES too)
;----------------------------------------------------------------------------

procedure   FlushBuf,NEAR
	DOSAssume   <DS>,"FlushBuf"

	call	GetCurHead
	ASSUME	DS:NOTHING
	TESTB	[DOS34_FLAG], FROM_DISK_RESET	; from disk reset ? ;hkn;
	jnz	scan_buf_queue
	cmp	[DirtyBufferCount], 0				;hkn;
	je	end_scan
scan_buf_queue:
	call	CheckFlush
	mov	ah, [di].buf_ID
	cmp	byte ptr [wperr], ah				;hkn;
	je	free_the_buf
	TESTB	[DOS34_FLAG], FROM_DISK_RESET	; from disk reset ? ;hkn;
	jz	dont_free_the_buf
free_the_buf:
	mov	word ptr [di].buf_ID, 00ffh
dont_free_the_buf:
	mov	di, [di].buf_next
	cmp	di, [FIRST_BUFF_ADDR]				;hkn;
	jne	scan_buf_queue
end_scan:
	Context	DS
	cmp	[FAILERR], 0
	jne	bad_flush
	ret
bad_flush:
	stc
	ret

EndProc	FlushBuf

;----------------------------------------------------------------------------
;
; Procedure Name : CHECKFLUSH
;
; Inputs : AL - Drive number, -1 means do not check for drive
;	   DS:DI - pointer to buffer
;
; Function : Write out a buffer if it is dirty
;
; Carry set if problem (currently user FAILed to I 24)
;
;----------------------------------------------------------------------------

	procedure CHECKFLUSH,NEAR

	Assert	ISBUF,<DS,DI>,"CheckFlush"
	mov	ah, -1
	CMP	[DI.buf_ID],AH
	retz				; Skip free buffer, carry clear
	CMP	AH,AL			; 
	JZ	DOBUFFER		; do this buffer
	CMP	AL,[DI.buf_ID]
	CLC
	retnz				; Buffer not for this unit or SFT
DOBUFFER:
	TESTB	[DI.buf_flags],buf_dirty
	retz				; Buffer not dirty, carry clear by TEST
	PUSH	AX
	PUSH	WORD PTR [DI.buf_ID]
	CALL	BUFWRITE
	POP	AX
	JC	LEAVE_BUF		; Leave buffer marked free (lost).
	AND	AH,NOT buf_dirty	; Buffer is clean, clears carry
	MOV	WORD PTR [DI.buf_ID],AX
LEAVE_BUF:
	POP	AX			; Search info
	return
EndProc CHECKFLUSH

	Break	<BUFWRITE -- WRITE OUT A BUFFER IF DIRTY>


;**	BufWrite - Write out Dirty Buffer
;
;	BufWrite writes a buffer to the disk, iff it's dirty.
;
;	ENTRY	DS:DI Points to the buffer
;
;	EXIT	Buffer marked free
;		Carry set if error (currently user FAILed to I 24)
;
;	USES	All buf DS:DI
;		HIGH_SECTOR


procedure   BufWrite,NEAR

	Assert	ISBUF,<DS,DI>,"BufWrite"
	MOV	AX,00FFH
	XCHG	AX,WORD PTR [DI.buf_ID] ; Free, in case write barfs
	CMP	AL,0FFH
	retz				; Buffer is free, carry clear.
	test	AH,buf_dirty
	retz				; Buffer is clean, carry clear.
	invoke	DEC_DIRTY_COUNT 	; LB. decrement dirty count

;hkn; SS override
	CMP	AL,BYTE PTR [WPERR]
	retz				; If in WP error zap buffer

;hkn; SS override
	MOV	[SC_DRIVE],AL		;LB. set it for invalidation	;AN000;
	LES	BP,[DI.buf_DPB]
	LEA	BX,[DI.BufInSiz]	; Point at buffer
	MOV	DX,WORD PTR [DI].BUF_SECTOR	;F.C. >32mb		;AN000;
	MOV	CX,WORD PTR [DI].BUF_SECTOR+2	;F.C. >32mb		;AN000;

;hkn; SS override
	MOV	[HIGH_SECTOR],CX		;F.C. >32mb		;AN000;
	MOV	CL,[DI.buf_wrtcnt]		;>32mb				;AC000;
;	MOV	AL,CH			; [DI.buf_wrtcntinc]
	XOR	CH,CH

;hkn; SS override for ALLOWED
	MOV	[ALLOWED],allowed_RETRY + allowed_FAIL
	test	AH, buf_isDATA

	JZ	NO_IGNORE
	OR	[ALLOWED],allowed_IGNORE
NO_IGNORE:

	MOV	AX,[DI.buf_wrtcntinc]		;>32mb				;AC000;

	PUSH	DI		; Save buffer pointer
	XOR	DI,DI		; Indicate failure

	push	ds
	push	bx
WRTAGAIN:
	SAVE	<DI,CX,AX>
	MOV	CX,1
	SAVE	<BX,DX,DS>
; Note:  As far as I can tell, all disk reads into buffers go through this point.  -mrw 10/88

	cmp	BuffInHMA, 0
	jz	@f
	push	cx
	push	es
	mov	si, bx
	mov	cx, es:[bp].DPB_SECTOR_SIZE
	shr	cx, 1
	les	di, dword ptr LoMemBuff
	mov	bx, di
	cld
	rep	movsw
	push	es
	pop	ds
	pop	es
	pop	cx
@@:
	invoke	DWRITE		; Write out the dirty buffer
	RESTORE <DS,DX,BX>
	RESTORE <AX,CX,DI>
	JC	NOSET
	INC	DI		; If at least ONE write succeedes, the operation
NOSET:				;	succeedes.
	ADD	DX,AX
	LOOP	WRTAGAIN
	pop	bx
	pop	ds
	OR	DI,DI		; Clears carry
	JNZ	BWROK		; At least one write worked
	STC			; DI never got INCed, all writes failed.
BWROK:	POP	DI
	return

EndProc BufWrite

Break	<SET_RQ_SC_PARMS-set requesting drive for SC>

;**	Set_RQ_SC_Parms - Set Secondary Cache Parameters
;
;	Set_RQ_SC_Parms sets the sector size and drive number value
;	for the secondary cache. This updates SC_SECTOR_SIZE &
;	SC_DRIVE even if SC is disabled to save the testing
;	code and time
;
;	ENTRY	ES:BP = drive parameter block
;
;	EXIT	[SC_SECTOR_SIZE]= drive sector size
;		[SC_DRIVE]= drive #
;
;	USES	Flags

procedure   SET_RQ_SC_PARMS,NEAR

;hkn; SS override for all variables used in this procedure.

	SAVE	<ax>

	MOV	ax,ES:[BP].DPB_SECTOR_SIZE	; save sector size
	MOV	SC_SECTOR_SIZE,ax

	MOV	al,ES:[BP].DPB_DRIVE		; save drive #
	MOV	SC_DRIVE,al

	RESTORE <ax>

srspx:	return

EndProc SET_RQ_SC_PARMS 			;LB. return

Break	<INC_DIRTY_COUNT-increment dirty count>

;----------------------------------------------------------------------------
; Input:
;	none
; Function:
;	increment dirty buffers count
; Output:
;	dirty buffers count is incremented
;
; All registers preserved
;----------------------------------------------------------------------------

procedure   INC_DIRTY_COUNT,NEAR

; BUGBUG  ---- remove this routine
; BUGBUG ---- only one instruction is needed   (speed win, space loose)
	inc	[DirtyBufferCount]			;hkn;
	ret
EndProc INC_DIRTY_COUNT

Break	<DEC_DIRTY_COUNT-decrement dirty count>

;----------------------------------------------------------------------------
; Input:
;	none
; Function:
;	decrement dirty buffers count
; Output:
;	dirty buffers count is decremented
;
; All registers preserved
;----------------------------------------------------------------------------

procedure   DEC_DIRTY_COUNT,NEAR
	cmp	[DirtyBufferCount], 0			;hkn;
	jz	ddcx			; BUGBUG - shouldn't it be an
	dec	[DirtyBufferCount]	; error condition to underflow here?	;hkn;
ddcx:	ret

EndProc DEC_DIRTY_COUNT


DOSCODE	ENDS
	END


