	TITLE	SSSCAN	-	Main Static Scanner Module
;***
;ssscan - Main static scanner module
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;   This is the main static scanner module.  It contains all external
;   interfaces to the module.  These interfaces are:
;   ssscan	      - scans the current text table to executor mode.
;   SsDescan(toState) - descans current text table to the state specified
;			this state may be SS_PARSE or SS_RUDE.
;
;
;****************************************************************************

	.xlist
	include 	version.inc
SSSCAN_ASM	= ON
	IncludeOnce	context
	IncludeOnce	executor
	IncludeOnce	opmin
	IncludeOnce	opstmt
	IncludeOnce	optables
	IncludeOnce	pcode
	IncludeOnce	qbimsgs
	IncludeOnce	rtinterp
	IncludeOnce	ssint
	IncludeOnce	txtmgr
	IncludeOnce	variable
	.list

assumes ds, DATA
assumes es, NOTHING
assumes SS, DATA

extrn	B$IFindEvHandler:far
extrn	exBolLab:far


	    extrn   DimImplicit:far	


sBegin	DATA

EmitErrOtx	dw	0

sEnd	DATA


sBegin	SCAN
assumes cs,SCAN

;*************************************************************************
;UpdateTxdBdl - Update the TXD or BDL for text table
;
; Purpose:
;
;   Update the txdCur (or the direct mode bdl) for this text table.
;
; Entry:
;
;   al = scanState
;   di = cbLogicalNew	    oTx of opEOT + 2
;
;   The structure txdCur identifies the current text table.
;   If grs.fDirect is TRUE, the direct mode text table (grs.bdlDirect)
;      is used instead.
;
; Preserves:
;
;   All
;
;*************************************************************************
UpdateTxdBdl:
	cmp	grs.GRS_fDirect,FALSE
	jne	DirectMode		;branch if in direct mode
	mov	txdCur.TXD_bdlText_cbLogical,di
	mov	txdCur.TXD_scanState,al
	ret

DirectMode:
	mov	[grs.GRS_bdlDirect_cbLogical],di
	ret

;***
; TxLnkInit
; Entry:
;	bx points to table to use for SsLinkCtl
; Exit:
;	ax = 0 (callers assume this)
; preserves: bx,dx (callers assume this)
;
;********************************************************
TxLnkInit PROC NEAR
	push	es
	push	di
	mov	SsLinkCtl,bx		;Save for scan dispatch routines
	push	ds
	pop	es
	mov	di,bx
	mov	cx,(size TXLNK)/2	;Count of bytes in structure
	xor	ax,ax			;Get a zero
	rep	stosw			;Initialize link controls to zero
	pop	di
	pop	es
	ret
TxLnkInit ENDP

;***
;ushort SsScan - Scan pcode from SS_PARSE to SS_EXECUTE
;
;Purpose:
;
;   In the case of an error, the pcode is left entirely
;   in the state in which the pcode was found.	An error
;   code is always returned.
;
;   This procedure causes memory allocations in the
;   interpreter and far heaps.
;
;   Certain scan routines push identified frames on the stack.
;   These frames must be uniquely identifiable, as other scan
;   routines must check to see if they exist.  At scan initialization,
;   an end-of-frame identifier is pushed on the stack, ensuring
;   that the bottom of the stack is not misinterpreted as some
;   particular scan stack entry.
;
;   NOTE: If grs.GRS_fDirect is TRUE on entry, txdCur should be
;	    ignored, and we should assume that the text table (whose
;	    segment is obtained via TxtSegCur) is in SS_RUDE, and
;	    must be scanned to SS_EXECUTE. Note also that the information
;	    in txdCur should be unchanged on return in this case.
;
;Algorithm:
;
;   Determine whether current text table is in SS_EXECUTE already
;      return if so
;   Grow the text table
;      If cbScanned = 0 then guess that cbScanned = 1.3 * cbText
;      Attempt to grow the text table by cbScanned
;	    error if NO growth possible
;      Move text up to end
;	    relink all links
;   Set up for main scan loop
;      install return address
;      set up es, si, di
;      push an end-of-stack frame identifier
;   Loop to completion
;   pop end-of-stack frame identifier
;   Return
;
;Entry:
;
;   grs.GRS_fDirect	 Current context information
;   rs	    mrsCur	 Current context information
;   ushort  oPrsCur	 Offset of current procedure.
;			    UNDEFINED = module
;   prs     prsCur	 Procedure context if
;			    oPrsCur != UNDEFINED
;
;Exit:
;
;   ax	    = 0 if no error occurred
;	      or
;	      error code
;   SsScan		 Standard BASIC error code
;   grs.context 	 Pcode address of the error
;			 or
;			 unmodified
;Exceptions:
;
;   None
;
;********************************************************
cProc	SsScan,<FAR,PUBLIC>,<si,di>

	localV	txLinks,<SIZE TXLNK>

ScanXj: jmp	ScanX

cBegin	SsScan

	DbMessTimer	SCAN,<Enter SsScan - >

;Assume not direct mode
	mov	cx,txdCur.TXD_bdlText_cbLogical ;Bytes of text
	cmp	grs.GRS_fDirect,FALSE		;Direct mode?
	jz	GetTXDInfoX			;Program mode
	xor	cx,cx
GetTXDInfoX:
;Grow the text, ensuring a gap between emit and source
	shr	cx,1
	shr	cx,1
	shr	cx,1			;Gap should equal 1/8 source size.
	GETSEGTXTCUR			;es = the text segment
	xor	ax,ax
	mov	SsCbTxExpand,ax 	;Initialize bytes of text expansion
	mov	[ScannerFlags],ax
	SetStartOtx	si		;oTxSrc
	mov	di,si			;oTxEmit
	DbChkTxdCur			;perform sanity check on txdCur
	call	SsMakeGap		;Ensure enough gap to scan (cx=gap)
	mov	ax,ER_OM		;Indicate out of memory
	jc	ScanXj			;Out of memory - can't scan
	lea	bx,txLinks		;Address of link controls
	call	TxLnkInit		;init txLinks structure to 0
					;ax = 0
					;preserves: bx,ax,dx, uses: cx,es
	mov	dx,txdCur.TXD_otxLabLink;Label link head pointer
	mov	[bx].TXLNK_LabDefNext,dx;offset of next label definition
					;LabDefLast left at zero

	cmp	grs.GRS_fDirect,FALSE	;Direct mode?
	jnz	InitLinks		;If direct mode, don't worry about CONT
	or	[txdCur].TXD_otxLabLink,1 ;Set LSB to indicate unbound

	    mov     bx,dataOFFSET prsCur.PRS_cbFrameTemp    ;Assume PRS
	    test    byte ptr [grs.GRS_oRsCur+1],80H	    ;MRS?
	    jnz     @F					    ;No
	    and     [mrsCur].MRS_flags,not FM_OptionBase1   ;Set option base 0
	    mov     bx,dataOFFSET mrsCur.MRS_cbFrameTemp
	    mov     [mrsCur.MRS_data_otxFirst],UNDEFINED    ;Init head of DATA list
@@:
	    xchg    ax,[bx]		;Zero cbFrameTemp if not direct mode
	push	ax			;Save old cbFrameTemp
	mov	bx,[grs.GRS_otxCONT]	;Get CONT otx
	inc	bx
	jz	InitLinks		;Can't continue, don't swap pcode
	mov	cx,[grs.GRS_oRsCONT]	;Get CONT oRS
	call	GetOtxRS		;Make sure oRS in cx isn't for DefFn
	cmp	cx,[grs.GRS_oRsCur]	;Is it the one we're scanning?
	jnz	InitLinks
	mov	ax,opNoList0
	xchg	ax,PTRTX[bx-1+si-StartOtx] ; Replace CONT pcode with special
	mov	[SsErrOpcode],ax	;Save original pcode

InitLinks:
	xor	ax,ax			;AX = 0

	mov	f_Static,TRUE		;Set $STATIC in effect flag

	;Initialize oTypCur and oValCur fields in COMMON to zero

	    mov     bx,[grs.GRS_bdtComBlk.BD_pb]
	mov	cx,ax
ZeroCom:
	cmp	cx,[grs.GRS_bdtComBlk.BD_cbLogical] ;Within size of block?
	jae	@F
	mov	[bx].COM_oTypCur,ax	;Zero oTypCur
	mov	[bx].COM_oValCur,ax
	add	bx,size COM
	add	cx,size COM
	jmp	ZeroCom

@@:

	mov	ssStackSave,sp		;Preserve the sp from start of scan loop
.errnz	STYP_StackEnd			;Stack base indicator used to determine
	push	ax			; end of control structures on stack

ScanToExeStart:
	xor	ax,ax
	mov	[SsCbFrameTemp],ax	;Count of temps in next statement
	mov	[SsErr],ax		;Error code
	mov	[SsExec],ax		;No executable code yet
	dec	ax
	mov	[SsErrOTx],ax		;Set error location to FFFF
	mov	[grs.GRS_oTxCur],ax	
	mov	[EmitErrOtx],ax
	mov	[SsDelayCnt],ax
	mov	[SsOTxPatchBos],ax	

	;Top of scan loop when pcode has HeapMove flag set

	public	SetScanRet
SetScanRet:
	mov	[SsOtxHeapMove],di
	mov	ScanRet,SCANOFFSET ScanToExeLoop  ;Set return address for
						;  dispatched opcode scanners

	;Main scan loop for SS_PARSE to SS_EXECUTE

	public	ScanToExeLoop
ScanToExeLoop:
	LODSWTX
	and	ax,OPCODE_MASK


	mov	bx,ax
	mov	al,mpOpAtr[bx]
	or	[SsExecFlag],al
	test	al,OPA_fHeapMove	;Cause heap movement?
	jnz	SetHeapMove
GetExe:
	shl	bx,1
	mov	ax,mpOpExe[bx]		;Get nominal executor
	mov	dx,ax			; Some routines want it in DX
DbPub   DispSS
DispSS:
	jmp	mpOpScanDisp[bx]	;Dispatch to scan routine for opcode

SetHeapMove:
	mov	[ScanRet],SCANOFFSET SetScanRet
	jmp	GetExe

Public	ScanExit
ScanExit:
	pop	ax			;Remove stack base indicator
	call	SsFrameType		;Make sure nothing's on the stack
	mov	sp,[SsStackSave]
	mov	al,SS_EXECUTE		;Scan state of text table
	cCall	UpdateTxdBdl		;Update the TXD or BDL for this table
;See if temp space grew.  If there are frames for this procedure on the stack,
;then temps can't grow.  An exception is if the only frame is the one on the
;top of the stack, where the gosub return addresses can be moved down to make
;more room.  Although direct mode allocates temp space off the current proc,
;it doesn't matter if it grows then because the procedure itself doesn't
;need (or use) the space.
	cmp	grs.GRS_fDirect,FALSE	;Direct mode?
	jnz	CouldCont		;Could always continue if direct mode
;Check for CantCont because temps grew
	pop	ax			;Original cbFrameTemp
	test	[SsFlags],SSF_CantCont	;Already detect CantCont situation?
	jnz	SetCantCont
	test	byte ptr [grs.GRS_oRsCur+1],80H	;MRS?
	    mov     bx,dataOFFSET mrsCur.MRS_cbFrameTemp
	    jz	    TempsGrow		;Brif MRS
	    mov     bx,dataOFFSET prsCur.PRS_cbFrameTemp
TempsGrow:
	    cmp     ax,[bx]		;Did it grow?
	jae	CouldCont		;Didn't grow--still can continue
;Grew FrameTemps
	mov	bx,dataOffset b$CurFrame
	cCall	ActiveORs_Frame,<bx>	; See if frame on stack
	or	ax,ax			
	jz	CouldCont		; Didn't find one
SetCantCont:
	call	CantCont
CouldCont:
;Compute max size of blank COMMON type and value tables
	mov	bx,[grs.GRS_bdtComBlk.BD_pb];pBlankCommon
	mov	ax,[bx].COM_oTypCur	;Size of type table
	cmp	ax,[oTypComMax]		;Grow?
	jbe	MaxComSize
	mov	[oTypComMax],ax		;Set new max
	mov	ax,[bx].COM_oValCur
	mov	[oValComMax],ax		;Set new max for value table
MaxComSize:
	DbChkTxdCur			;perform sanity check on txdCur
	DbMessTimer	SCAN,<Leave SsScan - >
	mov	ax,[SsErr]		;Return error code in ax
	or	ax,ax
	jz	ScanX
	cmp	[grs.GRS_fDirect],FALSE ;don't descan direct mode buffer
	jne	ScanX			;branch if in direct mode
	push	ax			;Descan sets it own error--save ours
	call	far ptr SsDescanErr	;Back to parse state if error
	pop	ax
ScanX:
cEnd	SsScan

;CheckSLoop - exe loop nonRELEASE checking code


subttl	SsDescan
page
;***
;SsDescan
;
;Purpose:
;
;   Descan is dispatched as:
;	    [[mpOpScanDisp+(([executor-2])*2)]-2]
;
;   That is, the descan address is in memory as the word before
;   each scan routine.	This is memory conservative, as there are
;   relatively few scan routines compared to opcodes or executors.
;
;   Individual descan routines must determine descan requirements
;   based on ssTarget and the current pcode state.  This is efficient
;   in that there are few descan routines that are state sensitive.
;
;   When descanning from executor state all pcodes that can be inserted
;   by SsScan only are removed.  In other words scan routines do not have
;   to check to see if coercion tokens (for example) have already been
;   inserted.  This is efficient in that it is usually as hard to check
;   to see if the work is required as it is to simply do the work.
;
;   Descan routines are dispatched with:
;   ax = opcode
;   si = descan source
;   di = descan destination
;
;*******************************************************************************
public	SsDescan
cProc	SsDescanErr,<FAR>,<es,si,di>
	localW	oTxtLast		;Offset of last pcode word
	localV	txLinks,<SIZE TXLNK>	;Link list control
SsDescan:
	mov	[SsErrOTx],-1		;Set error location to FFFF
cBegin	SsDescanErr
	DbMessTimer	SCAN,<Enter SsDeScan - >
	DbChkTxdCur			;perform sanity check on txdCur
	DbAssertRelB grs.GRS_fDirect,e,0,SCAN,<descan called for direct mode buffer>
	DbAssertRelB txdCur.TXD_scanState,e,SS_EXECUTE,SCAN,<descan called when not in EXECUTE state>


;Load text segment to es:
	GETSEGTXTCUR			;es = seg adr of cur text table
	SetStartOtx	di		;Start at the beginning of text
					; and debind labels to oTxt
	call	SsLabelRefDebind	;First descan label references
	mov	ax,UNDEFINED
	mov	txdCur.TXD_otxLabLink,ax;Update txd head pointer
	lea	bx,txLinks		;Address of link control struc
	call	TxLnkInit		;init txLinks structure to 0
					;ax = 0
					;preserves: bx,ax,dx, uses: cx,es
	mov	[SsErr],ax
	mov	ax,txdCur.TXD_bdlText_cbLogical
	dec	ax
	dec	ax
	mov	oTxtLast,ax		;Save offset of last pcode word
	SetStartOtx	si		;Descan from the start
	mov	di,si			;Destination = source
	mov	[SsCbTxExpand],si
	mov	[ScanRet],SCANOFFSET ContDescanLoop ;Descan routines
						;return through ScanRet
SortOTx:
	mov	dx,[EmitErrOtx]		;Start sort with error location
	mov	[SsReturnBp],dataOFFSET EmitErrOtx
	call	FCanCont		; ZF set if user can't continue
	jz	SetNextOtxJ		;Don't search others if can't cont.
	push	si
	push	di
	mov	si,[pGosubLast] 	;Head of gosub list
	mov	di,[b$CurFrame]	;Start of bp return addr chain
	mov	cx,[grs.GRS_oRsCONT]	;cx = current pc's oRs
	mov	bx,dataOFFSET grs.GRS_oTxCONT
FixORs:
	call	GetOtxRS		;Make sure oRS in cx isn't for DefFn
CompOTx:
	test	byte ptr [bx],1 	;Special one we should ignore?
	jnz	NextOTx 		;Brif return to direct mode
	call	CheckUpdate
NextOTx:
	;Scan GOSUB return address list for returns in oPRS = cx

	or	si,si
	jz	CheckFrame		;No more gosub returns
	cmp	si,di			;Still within current module/procedure
	ja	CheckFrame
	lea	bx,[si].FR_otxRet
	mov	si,[si]
	jmp	CompOTx

CheckFrame:
	cmp	di,[b$MainFrame]	;End of list?
	jz	SetNext
	lea	bx,[di].FR_otxRet
	mov	cx,[di].FR_oRsRet	;oRS of return address
	mov	di,[di].FR_basBpLink	
	jmp	FixORs

SetNextOtxJ:
	jmp	short SetNextOtx

SetOtxCont:
	mov	ax,di
	inc	ax			;Set LSB of otx to remember it's set
	mov	[grs.GRS_otxCont],ax	;Set new CONT otx
	jmp	SortOtx

SetErrorLoc:
	mov	ax,di
	dec	ax
	dec	ax			;Point into previous pcode
	mov	[grs.GRS_oTxCur],ax	;Location of error
	mov	[EmitErrOtx],UNDEFINED
	jmp	SortOtx

OTxMatch:
	mov	cx,[SsReturnBp]
	cmp	cx,dataOFFSET grs.GRS_oTxCONT	;Is it current PC?
	jz	SetOtxCont
	cmp	cx,dataOFFSET EmitErrOtx	;Is it the location of an error?
	jz	SetErrorLoc

	;At this point, it has been determined that CX contains the offset into
	;DGROUP of a word containing an Otx into the current pcode table.  To
	;update this word to account for descaning and subsequent scanning,
	;an opcode is inserted with an operand of the DGROUP offset.  The
	;scan routine for this opcode will update the static location without
	;emitting the executor or operand.  To prevent this oTx from being
	;found on the next pass through SortOTx, it is set to UNDEFINED (0ffff).
	;If CX is odd then CX contains the location minus one where the update
	;should occur.	This location is the address of an error handler or
	;event handler and the opcode must be inserted after the BOL to
	;prevent an edit of lines before the handler from messing up the
	;update.

	mov	ax,opNoList1		;Return address opcode
	TestM	[SsNextOTx],1		; Is this a handler address?
	jz	@F			; Brif not
	mov	ah,HIGH (opNoList1+OPCODE_MASK+1)
@@:					
	mov	bx,di			;Insert right here
	call	Insert1Op
	mov	bx,cx
	mov	[bx],UNDEFINED		;Blast original oTx
	jmp	SortOTx

SetNext:
	pop	di
	pop	si
;
; See if there is a smaller otx in the Invoke chain.
;

	;See if any event handlers need update

	    push    dx			;Save referenced oTx
	    push    [grs.GRS_oRsCur]
	    call    B$IFindEvHandler	;Get smallest event handler oTx
	    mov     bx,dx		;Offset of smallest
	    pop     dx
	    call    CheckUpdateSkipBOL

	    ;See if references in MRS need update.

	    test    byte ptr [grs.GRS_oRsCur+1],80H  ;At module level?
	    jnz     SetNextOtx

	    ;Update module level error handler

	    mov     bx,dataOFFSET mrsCur.MRS_otxHandler
	    call    CheckUpdateSkipBOLAX

	    ;Update current Data position

	    mov     bx,dataOFFSET mrsCur.MRS_data_otxCur
	    call    CheckUpdateRs

SetNextOtx:
	add	dx,[SsCbTxExpand]	;Adjust for current source position
	jnc	ValidOtx
	mov	dx,UNDEFINED
ValidOtx:
	mov	[SsNextOTx],dx		;oText we're looking for
ContDescanLoop:
	cmp	si,[SsNextOTx]		;Looking for this oText?
	jae	OTxMatch		;Brif current otx >= next update loc

	LODSWTX 			;Load executor
	mov	bx,ax
	GetCodeIntoDs	SCAN		
	mov	ax,[bx-2]		;Load opcode for executor
	push	ss			
	pop	ds			;Move back to data segment
	mov	bx,ax
	and	bh,HIGH OPCODE_MASK
	DbAssertRel bx,be,OP_MAX,SCAN,<Descan: opcode out of range>
	shl	bx,1			;Move to word offset
	mov	bx,mpOpScanDisp[bx]	;Load scan routine address
	jmp	cs:SsProcParse[bx]	;And dispatch w/ ax = opcode


;DescanTerm is installed in ScanRet by descan to SS_PARSE dispatch routines
;that terminate descan.  For example, see SsD_Eot.

public	DescanTerm
DescanTerm:
	mov	al,SS_PARSE
	cCall	UpdateTxdBdl		;Update the TXD or BDL for text table
	call	FCanCont		; See if we can continue
	jz	DescanX			;Can't cont
	and	byte ptr [grs.GRS_otxCont],not 1 ;Reset LSB of CONT otx
	cmp	[SsErr],0		;Any errors?
	jz	DescanX
	call	CantCont		;If errors, can't continue

;DescanX is installed in ScanRet by descan to SS_RUDE dispatch routines
;that terminate descan.  For example, see SsV_Eot
Public	DescanX
DescanX:
	DbChkTxdCur			;perform sanity check on txdCur
	DbMessTimer	SCAN,<Leave SsDeScan - >
cEnd	SsDescan

;*** CheckUpdate,CheckUpdateSkipBOL
;Purpose:
;
;   See if the oTx at [bx] is smaller than the one in dx and is
;   in the same text table.
;
;Input:
;
;   ax = oTx (CheckUpdateSkipBOL only)
;   ds:bx = pointer to an oTx
;   cx = oRS of oTx at [bx]
;   dx = current smallest oTx in current text table
;
;Outputs:
;
;   dx updated with new smallest
;
;Preserves:
;
;   bx,cx

CheckUpdateSkipBOLAX:			
	mov	ax,word ptr [bx]	

CheckUpdateSkipBOL:
	inc	ax
	jz	UpdateX
	dec	ax
	push	bx
	GetCodeIntoDs	SCAN		
	mov	bx,ax			;oTx to bx
	add	bx,ss:[SsCbTxExpand]
	mov	bx,PTRTX[bx]		;Get executor
	mov	bx,[bx-2]		;Get opcode
	push	ss			
	pop	ds
	mov	bl,mpOpAtr[bx]		;Load attribute byte
	and	bx,OPA_CntMask		;Get the operand count from attribute
	add	ax,bx			;Compute oTx after BOL
	dec	ax			; LSB indicates BOL Update
	pop	bx
	jmp	short CheckUpdateAx

CheckUpdate:
	cmp	cx,[grs.GRS_oRsCur]	;In current text table?
	jnz	UpdateX
CheckUpdateRs:
	mov	ax,[bx]
CheckUpdateAx:
	cmp	dx,ax
	jbe	UpdateX
	xchg	dx,ax			;New smallest oTx
	mov	[SsReturnBp],bx		;Location being updated
UpdateX:
	ret

;*** GetOtxRs
;Inputs:
;	cx = any oRS
;Purpose:
;	Map oRS of DefFn back to it oMRS
;Outputs:
;	cx = oRS that owns text table of input oRS
;Preserves:
;	bx,dx,es

GetOtxRs:
	push	es			
	or	cx,cx
	jns	OtxRs			;If MRS, have text table
;See if oPRS is of a DefFn
	xchg	ax,bx			;Preserve bx
	xchg	cx,ax			;cx = old bx, ax = oRS
	and	ax,not 8000H		;Reset MSB
	call	PPrsOPrsSCAN		; bx = pPRS
	or	ax,8000H		;Make ax an oRS again
	cmp	BPTRRS[bx].PRS_procType,PT_DEFFN ; Is proc a DefFn?
	jnz	UsePRS			;If not, PRS in ax is OK
	mov	ax,PTRRS[bx].PRS_oMRS	; Get oMRS for DefFn
UsePRS:
	xchg	cx,ax			;cx = oRS
	xchg	bx,ax			;Restore bx
OtxRs:
	pop	es			
	ret


;***
;SsFrameType - determine type of scanner frame
;Purpose:
;	Report error if there was a scanner frame on the stack.
;Inputs:
;	ax = scan stack entry
;	si = scan source oTx
;	di = scan emit oTx
;	[sp+2] = oTx of source of scanner frame
;Ouputs:
;	if ax = 0, nothing
;	else report appropriate error
;***************************************************************************
	public	SsFrameType

SsFrameType:
	or	ax,ax
	jz	IgnoreErr
	cmp	[SsErr],0		;Already have an error?
	jnz	IgnoreErr		;If so, leave it
	mov	cx,ER_FN		;FOR without Next
	testx	ax,STYP_For		;In FOR block?
	jnz	CxErr
	mov	cx,ER_WH		;WHILE without WEND
	testx	ax,STYP_While
	jnz	CxErr
	mov	cx,MSG_Do		;DO without LOOP
	testx	ax,STYP_Do
	jnz	CxErr
	mov	cx,MSG_Select		;SELECT without END SELECT
	testx	ax,STYP_Case
	jnz	CxErr

	    mov     cx,MSG_DefNoEnd	;DEF without END DEF
	    testx   ax,STYP_DefFn
	    jnz     CxErr
;
;Insert additional control structure tests here
;Scan stack entry must be oTx of last operand
;
	testx	ax,STYP_If+STYP_Else	;In IF block?
	mov	ax,ER_IER		;Internal error if not
	jz	SsError
	mov	cx,MSG_IWE		;Block IF without END IF
CxErr:
	pop	dx			;Return address
	pop	bx			;oTx of error
	push	bx
	push	ax			;Restore frame
	push	dx			;Put return address back
	xchg	ax,cx			;Error code to ax
;	jmp	SsErrorBx		;Fall into SsErrorBx
	
;***
;SsError,SsErrorBx - scanner error handler
;
;Purpose:
;
;   Record scanner error, setting variables as follows:
;
;   [SsErr]		    = error code
;   [grs.GRS_oTxCur]	    = oTx in unscanned pcode of error
;   [SsErrOTx]		    = oTx in scanned pcode when error was found
;
;   This routine returns normally and scanning continues so that
;   all the various link chains will be properly updated.  If a
;   second error is encountered, it is ignored.
;
;Input:
;
;   ax = error code
;   si = Source oTx of pcode that caused the error. (SsError)
;   bx = Emit oTx of error (SsErrorBx)
;   di = Current emit oTx
;
;Modifies:
;
;   none.
;
;Preserves:
;
;   bx,cx,dx
;
;***************************************************************************
public	SsError,SsErrorBx

;NOTE: fallen into by SsFrameType above!
SsErrorBx:
;This variation of SsError reports bx as the emit location of the error
;instead of si as the source location
	cmp	[SsErr],0		;Already have an error?
	jnz	IgnoreErr		;If so, leave it
	mov	[SsErr],ax		;Record error code
	mov	[EmitErrOTx],bx		;Location of error
	mov	[SsErrOTx],di		;Remember current emit oTx
	ret


SsError:
	cmp	[SsErr],0		;Already have an error?
	jnz	IgnoreErr		;If so, leave it
	mov	[SsErr],ax		;Record error code
	mov	ax,si
	sub	ax,[SsCbTxExpand]	;Compute unscanned pcode address
	dec	ax
	dec	ax			;oTx - 2
	mov	[grs.GRS_oTxCur],ax	;Report location of error
	mov	[SsErrOTx],di		;Remember scanned error address too
IgnoreErr:
	ret


subttl	ScanAndExec and ExecuteFromScan
page
;*** ScanAndExec
;
;Purpose:
;
;   Called by rude scanner to scan to execute state, then
;   execute a line of code.  Used for assigning constants.
;
;	Modified in revision [12] to take inputs on stack, use cMacros,
;	become a far entry point.
;Inputs:
;	parm1 = oTx of pcode to execute
;	parm2 = cb of pcode
;Outputs:
;
;   ax = error code, 0 if no error; flags set
;	    if ax != 0, high-bit set indicates that pcode was not changed
;	    (i.e., still contains an oNam, not an oVar).
;   es = current text segment
;
;*** ExecuteFromScan
;
;Purpose:
;
;   Fires up execution from the scanner.  Used to DIM $STATIC arrays
;   in COMMON.	DIM executor direct jumps to ScanExExit to terminate.
;
;	Modified in revision [12] to take dummy parms on stack, use cMacros,
;	become a far entry point.
;Inputs:
;
;   [SsScanExStart] has starting oTx
;
;***************************************************************************
ScanExGrow	=	20
	public	ScanAndExec,SsScanExExit
	public	ExecuteFromScan,ScanExExit

cProc	ExecuteFromScan,<NEAR>,<si,di>	
	parmW	dummy1			; parms provided to match frame
	parmW	dummy2			; conditions of ScanAndExec
cBegin					
	DbAssertRel [SsErr],e,0,SCAN,<ExecuteFromScan: SsErr != 0>
	push	[b$curframe]
	push	[txdCur.TXD_bdlText_cbLogical]
DJMP	jmp	short StartExec		
cEnd					; nogen

cProc	ScanAndExec,<NEAR>,<si,di>	
	parmW	oTxPcode		
	parmW	cbPcode 		
cBegin					
	mov	[ScannerFlags],SSF_ScanAndExec*100H	;Scanning CONST statement
	push	[b$curframe]
	mov	di,[txdCur.TXD_bdlText_cbLogical]
	push	di			;Save current text size
	mov	si,[oTxPcode]		
	mov	cx,[cbPcode]		
	push	cx
	add	cx,ScanExGrow+2 	;Allow some growth and END executor
	push	cx
	call	TxtFreeFar		; Extend text table
	or	ax,ax			
	jz	ScanExOME		;Insufficient memory

	GETSEGTXTCUR			; es = the text segment
	pop	cx
	mov	[SsScanExSrc],si	;Save true oTx of source
	push	di			;Emit address
	add	di,ScanExGrow		;Source address
	push	di
	shr	cx,1			;cx = count of words
	cli				;Double prefix! No interrupts!
rep	movs	PTRTX[si],PTRTX[di]	;Copy pcode to end of text table
	sti
	mov	ax,opEOT
	STOSWTX
	mov	[txdCur.TXD_bdlText_cbLogical],di ;Extend text table
	pop	si
	pop	di
	mov	[SsScanExStart],di
	mov	[SsCbTxExpand],ScanExGrow


	jmp	ScanToExeStart

ScanExOME:
	pop	cx
	pop	dx			;Clean junk off stack
	mov	ax,08000H OR ER_OM	;high bit says pcode is unchanged
	jmp	short ScanExErr

SsScanExExit:
	mov	PTRTX[di],codeOFFSET exScanExExit
	mov	ax,[SsErr]
	or	ax,ax			;Any scanner errors?
	jnz	ScanExErr
StartExec:
;Dispatch execution
	call	far ptr StartExecCP
ScanExErr:
	GETSEGTXTCUR			;es = the text segment
	and	[SsFlags],not SSF_ScanAndExec
	pop	[txdCur.TXD_bdlText_cbLogical]
	pop	[b$curframe]
	or	ax,ax			; Any scanner errors?
cEnd					

sEnd	SCAN

sBegin	CP

assumes cs, CP
assumes ds, DATA
assumes es, NOTHING
assumes SS, DATA

cProc	StartExecCP,<FAR>
cBegin

	call	RtPushHandler		;Save current RT error handler
	mov	ax,cpOFFSET RtScanExTrap
	call	RtSetTrap		;Assign new RT error handler
	call	DisStaticStructs	;Deactivate mrsCur
	mov	[b$curframe],bp 	;Required by math executors
	    TestM   [SsScanExStart],1	; Is this implicit Dim?
	    jnz     @F			; Brif yes
	mov	si,[SsScanExStart]

	jmp	far ptr Start

@@:					
	jmp	DimImplicit		

ScanExExit:
	xor	ax,ax
RtScanExTrap:
	push	ss
	pop	ds			;restore ds == DGROUP from execution
	xchg	ax,si			;Save error code
	call	RtPopHandler
	call	EnStaticStructs 	;Re-activate mrsCur
	xchg	ax,si			;Restore error code


cEnd

sEnd	CP

sBegin	CODE

assumes cs, CODE
assumes ds, DATA
assumes es, NOTHING
assumes SS, DATA

	extrn	Start:far

exScanExExit:	jmp	far ptr ScanExExit

sEnd	CODE

end
