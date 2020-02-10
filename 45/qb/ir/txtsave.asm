	TITLE	txtsave.asm - ASCII Save Functions

;==========================================================================
;
;Module:  txtsave.asm - ASCII Save Functions
;System:  Quick BASIC Interpreter
;
;=========================================================================*/

	include		version.inc
	TXTSAVE_ASM = ON
	includeOnce	architec
	includeOnce	context
	includeOnce	heap
	includeOnce	lister
	includeOnce	names
	includeOnce	opcontrl
	includeOnce	opid
	includeOnce	opmin
	includeOnce	opstmt
	includeOnce	parser
	includeOnce	pcode
	includeOnce	qbimsgs
	includeOnce	rtinterp
	includeOnce	rtps
	includeOnce	rttemp
	includeOnce	scanner
	includeOnce	txtmgr
	includeOnce	txtint
	includeOnce	util
	includeOnce	ui
	includeOnce	variable
	includeOnce	edit		

	assumes	DS,DATA
	assumes	SS,DATA
	assumes	ES,NOTHING

	ASC_CRLF	EQU 0A0Dh	;ASCII Carriage return/Line Feed
	ASC_TAB 	EQU 9		;ASCII Tab

sBegin	DATA

	EXTRN	tabStops:WORD	;defined in edit manager
	EXTRN	b$PTRFIL:WORD	;defined by runtime - current channel ptr

CrLf		DW	ASC_CRLF	;for file line termination
oMrsSaveDecl	DW	0		;used by SaveDeclares
sEnd	DATA

sBegin	CODE

;Table of opcodes used to search for DECLARE or CALL statements
;
tOpDecl LABEL WORD
	opTabStart	DECL
	opTabEntry	DECL,opStDeclare
	opTabEntry	DECL,opStCall
	opTabEntry	DECL,opStCalls
	opTabEntry	DECL,opStCallLess
	opTabEntry	DECL,opEot

sEnd	CODE

	EXTRN	B$BUFO:FAR
	EXTRN	B$KILL:FAR

sBegin	CP
assumes	cs,CP

;*************************************************************
; ushort SaveTxdCur(ax:otxStart)
; Purpose:
;	ASCII save the contents of the current text table
; Entry:
;	ax = text offset to start saving text
; Exit:
;	ax = size of last line output (=2 if trailing blank line)
;	ps.bdpSrc is used
; Exceptions:
;	Can cause runtime error (Out of memory, I/O errors)
;
;*************************************************************
SaveTxdCur PROC NEAR
	DbChk	Otx,ax			
	push	si
	push	di
	sub	di,di			;Init cbLastLine = 0
	mov	[otxListNext],ax	;ListLine() updates [otxListNext]
	test	[mrsCur.MRS_flags2],FM2_NoPcode ; document file?
	je	GetOtxEndProg		; file is measured in Otxs, not lines

	DbAssertRel [otxListNext],e,0,CP,<SaveTxdCur:Not starting at the begining of file>
	push	[mrsCur.MRS_pDocumentBuf] 
	call	S_LinesInBuf		; get # lines in document buffer
	jmp	short SetMaximumSave	

GetOtxEndProg:
	call	OtxEndProg		;ax = otx to Watch pcode
SetMaximumSave:
	xchg	si,ax			;si = otx to Watch pcode
StLoop:
	mov	ax,[otxListNext]	;ax=offset for next line to list
	cmp	ax,si
DJMP	jae	SlDone			;brif done with this text table

	test	[mrsCur.MRS_flags2],FM2_NoPcode ; document file?
	je	ListPcodeLine		; brif not, let lister get line

	push	[mrsCur.MRS_pDocumentBuf] ; document table to list from
	push	ax			; line to list
	push	ps.PS_bdpSrc.BDP_cbLogical ; length of buffer
	push	ps.PS_bdpSrc.BDP_pb	;pass ptr to dest buf
	call	S_cbGetLineBuf		; AX = cBytes in line
	inc	[otxListNext]		; bump pointer to next line
	mov	[cLeadingSpaces],0	; start with no leading spaces
	mov	bx,[ps.PS_bdpSrc.BDP_pb]; BX = ptr to 0 terminated string

CheckNextChar:				
	cmp	byte ptr [bx],' '	; Is it a space
	jne	GotLine 		; brif not, say that we got line

	inc	[cLeadingSpaces]	; indicate another space
	inc	bx			; point to next character
	jmp	CheckNextChar		; check it for a space

ListPCodeLine:				

	push	ax			;pass offset to ListLine
	PUSHI	ax,<DATAOFFSET ps.PS_bdpSrc>	;pass dst buf ptr to listline
	call	ListLine		;ax=char count
	inc	ax			;test for UNDEFINED
	jne	NotOmErr		;brif out-of-memory
	jmp	OmErrCP
NotOmErr:
	dec	ax			;restore ax = byte count
GotLine:
	cmp	[fLsIncluded],0
	jne	StLoop			;brif line was part of $INCLUDE file

	test	mrsCur.MRS_flags2,FM2_EntabSource ;do we need to entab leading
					;blanks?
	jz	NoEntab 		;brif not
	mov	cl,[cLeadingSpaces]	;cl = count of leading spaces
	or	cl,cl			;any leading spaces?
	jnz	EntabLeadingSpaces	;brif so, replace with tabs
NoEntab:
	mov	bx,[ps.PS_bdpSrc.BDP_pb]
EntabCont:
	; There is currently no need to call UpdChanCur here, because
	; there is no chance of having nested open files during ascii save.
DbAssertRel b$PTRFIL,ne,0,CP,<SaveTxdCur:Invalid channel>

	; Call OutLine as we can not guarentee that the buffer
	; pointed to by BX contains at least two more bytes.
	; This is slower, but will not trash the heaps.
	mov	di,ax			; DI = new "cbLastLine"
	inc	di			; account for CRLF
	inc	di			
	call	OutLine 		; Print line and CRLF
DJMP	jmp	SHORT StLoop

SlDone:
	xchg	ax,di			;ax = cb last line emitted
	pop	di
	pop	si
	ret
SaveTxdCur ENDP

; We have a line with leading spaces which needs to be entabbed.
; We will convert spaces to tabs in the buffer, and return the
; new buffer char count, and a ptr to the start of the buffer.
;
; Entry:
;	ax = count of chars in line buffer
;	cl = count of leading spaces
; Exit:
;	ax = adjusted count of chars in line buffer
;	bx = ptr to first char in buffer
; Uses:
;	bx,cx,dx

EntabLeadingSpaces:
	push	ax			;preserve buffer char count
	xchg	ax,cx
	sub	ah,ah			;ax = cLeadingSpaces
	mov	dx,ax			;remember cLeadingSpaces
	mov	cx,[tabStops]		;get user defined tabstop settings

; User interface guarantees tabStops will not be set to 0
DbAssertRel cx,nz,0,CP,<tabStops=0 detected in Ascii save>

	div	cl			;al=tab count, ah=space count
	mov	bx,[ps.PS_bdpSrc.BDP_pb] ;bx=ptr to line buffer

	add	bx,dx			;bx=ptr to first non-leading space
	sub	dl,al
	sub	dl,ah			;dx=excess space in buffer
	sub	bl,ah			;backup over remaining spaces
	sbb	bh,0
	xchg	ax,cx
	sub	ch,ch			;cx=tab count
	jcxz	NoTabs			;brif none to replace
	mov	al,ASC_TAB
TabLoop:
	dec	bx			;back up a char
	mov	[bx],al 		;replace space with tab
	loop	TabLoop
NoTabs:
	pop	ax			;recover buffer char count
	sub	ax,dx			;adust for removed spaces
	jmp	EntabCont

;*************************************************************
; OutLine, OutCrLf
; Purpose:
;	OutLine - Output line and CR-LF to current file
;	OutCrLf - Output CR-LF to current file
; Entry:
;	bx points to 1st byte to output
;	ax = byte count
;
;*************************************************************
OutLine PROC NEAR
	; There is currently no need to call UpdChanCur here, because
	; there is no chance of having nested open files during ascii save.
DbAssertRel b$PTRFIL,ne,0,CP,<OutLine:Invalid channel>

	push	ds			;pass segment of buffer
	push	bx			;pass offset of buffer
	push	ax			;pass length of buffer
	call	B$BUFO			;output line via runtime
	;fall into OutCrLf
OutLine ENDP

OutCrLf PROC
	; There is currently no need to call UpdChanCur here, because
	; there is no chance of having nested open files during ascii save.
DbAssertRel b$PTRFIL,ne,0,CP,<OutCrLf:Invalid channel>

	push	ds
	PUSHI	ax,<dataOFFSET CrLf>
	PUSHI	ax,2
	call	B$BUFO			;output CR/LF via runtime
	ret
OutCrLf ENDP

;*************************************************************
; RelShBuf
; Purpose:
;	Release temporary text table used by SaveProcHdr.
;	Called when we're done saving, or when an error occurs.
;
;*************************************************************
RelShBuf PROC NEAR
	mov	[txdCur.TXD_bdlText_cbLogical],0
					;so TxtDiscard won't examine deleted txt
	call	TxtDiscard		;discard temporary text table
	call	TxtActivate		;make module's text table cur again
	mov	[ps.PS_bdpDst.BDP_cbLogical],0 ;release space held by temp bd
	ret
RelShBuf ENDP

;*************************************************************
; ushort SaveProcHdr(ax:otxProcDef)
; Purpose:
;	ASCII save the current procedure's header.
;
; Entry:
;	ax = otxProcDef = offset into procedure's text table to opBol for line
;	   containing SUB/FUNCTION statement.  0 if this table has no
;	   SUB/FUNCTION statement yet.
;
; Exit:
;	ps.bdpSrc is used
;	grs.fDirect = FALSE
;	ax = 0 if no error, else Standard BASIC error code (i.e. ER_xxx)
;
; Exceptions:
;	Can cause runtime error (Out of memory, I/O errors)
;
;*************************************************************
SaveProcHdr PROC NEAR
	push	si			;save caller's si,di
	push	di

	mov	di,ax			;di = otxProcDef
	push	[grs.GRS_oPrsCur]	;pass current oPrs to PrsActivate below

	;fill tEtTemp[] with DEFTYP's from start of proc table to SUB line
	mov	ax,di			;ax = otxProcDef
	mov	bx,dataOFFSET tEtTemp	;bx -> type table
	call	OtxDefType

	;move everything up to proc def from procedure's to temp text table
	PUSHI	ax,<dataOFFSET ps.PS_bdpDst>
	push	di			;pass otxProcDef
	call	BdRealloc
	or	ax,ax
	je	JE1_ShOmErr		;brif out-of-memory error

	PUSHI	ax,<dataOFFSET txdCur.TXD_bdlText>
	SetStartOtx ax			
	push	ax
	push	[ps.PS_bdpDst.BDP_pb]
	push	di			;pass otxProcDef
	call	BdlCopyFrom

	;Now we create a temporary text table for saving the synthetically
	;generated procedure header.  We must go through the following steps
	; to do this:
	;  PrsDeactivate()  ---  causes module's text table to be made active
	;  TxtDeactivate()  ---  causes no text table to be made active
	;  TxtCurInit()     ---  make temp text table active
	;  put synthetically generated pcode into txdCur
	;  ASCII save this pcode buffer to the file
	;  TxtDiscard()     ---  discard temporary text table
	;  TxtActivate()    ---  make module's text table current again
	;  PrsActivate(oPrsSave)
	

	;[flagsTM.FTM_SaveProcHdr] is non-zero while in critical state
	; within function SaveProcHdr. Tells SaveFile's error cleanup
	; to take special action.
	
	or	[flagsTM],FTM_SaveProcHdr ;if err, remember to clean up
	call	PrsDeactivate		;make module's text table active
	call	TxtDeactivate		;causes no text table to be made active
	call	TxtCurInit		;make temp text table active
	je	ShOmErr			;brif out-of-memory error

	;emit synthetic DEFxxx statements as transition from end of last
	;text table to procedure definition line
	PUSHI	ax,<dataOFFSET ps.PS_tEtCur>
	PUSHI	ax,<dataOFFSET tEtTemp>
	SetStartOtx ax			;insert at start of text
	call	InsertEtDiff
JE1_ShOmErr:
	je	ShOmErr			;brif out-of-memory error
	call	OtxEndProg		;ax = otx to Watch pcode
	xchg	si,ax			; = offset beyond synthetic DEFxxx stmts

	;Append everything up to SUB line to temp table
	push	si			;pass otx to Watch pcode
	push	di			;pass otxProcDef
	call	TxtMoveUp
	je	ShOmErr			;brif out-of-memory error
	PUSHI	ax,<dataOFFSET txdCur.TXD_bdlText>
	push	si			;pass otx to Watch pcode
	push	[ps.PS_bdpDst.BDP_pb]
	push	di			;pass otxProcDef
	call	BdlCopyTo
	call	SqueezeDefs		;takes parm in si

	;if setting of $STATIC/$DYNAMIC differs between procedure's header
	;and where procedure will be listed in source file,
	;insert pcode to change the state for the procedure,
	;Note: fLsDynArrays's value will be changed by ListLine() when it
	;      lists the line emitted by InsertDynDiff (if any)
	
	SetStartOtx ax			;insert at start of text
	mov	dh,[fLsDynArrays]	;dh = old $STATIC/$DYNAMIC state
	mov	dl,[fProcDyn]		;dl = new $STATIC/$DYNAMIC state
	call	InsertDynDiff
	je	ShOmErr			;brif out of memory error
	SetStartOtx ax			;start saving at start of text
	call	SaveTxdCur		;save procedure's header to file
	call	RelShBuf		;release temp text tbl
	and	[flagsTM],NOT FTM_SaveProcHdr ;reset critical section flag

	;oPrs parm was pushed on entry to this function
	call	PrsActivateCP
	sub	ax,ax			;return no-error result
;al = error code
ShExit:
	mov	[ps.PS_bdpDst.BDP_cbLogical],0 ;release space held by temp bd
	or	al,al			;set condition codes for caller
	pop	di			;restore caller's si,di
	pop	si
	ret

ShOmErr:
	pop	ax			;discard oPrs
	mov	al,ER_OM		;return al = out-of-memory error
	jmp	SHORT ShExit
SaveProcHdr ENDP

;Cause runtime error "Out of memory"
OmErrCP:
	mov	al,ER_OM
	call	RtError

;*************************************************************
; ONamOtherOMrs
; Purpose:
;	Given an oNam in current mrs, convert it to an oNam
;	in another mrs (which has a different name table).
; Entry:
;	grs.oMrsCur = source oMrs
;	ax = source oNam
;	dx = target oMrs
; Exit:
;	ax = target oNam (0 if out of memory error)
;	flags set based upon return value.
;
;*************************************************************
cProc	ONamOtherOMrs,<NEAR>		
	localV	bufNam,CB_MAX_NAMENTRY	
cBegin
	cmp	[grs.GRS_oMrsCur],dx
	je	OnOExit			;brif source mrs = target mrs

	xchg	ax,bx			;bx = oNam (save until CopyONamPb)
	push	di
	push	[grs.GRS_oRsCur]	;save caller's oRs -for RsActivate below

	mov	di,dx			;di = target oMrs
	lea	ax,bufNam
	push	ax			;save ptr to string
					; string ptr in ax
					; oNam to CopyONamPb in bx
	cCall	CopyONamPb,<ax,bx>	; ax = byte count
	push	ax			;save byte count
	cCall	MrsActivateCP,<di>	;activate target mrs
	pop	cx			;cx = byte count
	pop	ax			;ax = ptr to bufNam
	call	ONamOfPbCb		;ax = target oNam (ax=Pb, cx=Cb)
	xchg	di,ax			;di = target oNam

	call	RsActivateCP		;re-activate caller's oRs
					; parm was pushed on entry
	xchg	ax,di			;ax = target oNam
	pop	di			;restore caller's es,di
OnOExit:
	or	ax,ax			;set condition codes
cEnd

;*************************************************************
; SaveDeclares
; Purpose:
;	Generate synthetic DECLARE stmts for forward referenced
;	SUBs and FUNCTIONs in this module as follows:
;	Pass1:
;	 For every prs in system,
;	   reset FTX_TmpDecl
;	   if prs type is FUNCTION and prs is in mrs being saved,
;	      set FTX_TmpRef bit, else reset it
;	Pass2:
;	 For every text table in this module
;	   Search text table for a reference to a SUB or FUNCTION
;	    if opStDeclare ref found
;	       set FTX_TmpDecl bit
;	    else if CALL, CALLS, implied CALL
;	       set FTX_TmpRef bit
;	Pass3:
;	 For every prs in system,
;	   if FP_DEFINED and FTX_TmpRef bit are set, and FTX_TmpDecl bit is not,
;	   copy pcode for definition to module, changing opcode to opStDeclare,
;	   and changing the oNam for each formal parm and explicitly
;	   listing the TYPE.
;
; Exit:
;	grs.fDirect = FALSE
;	ax = 0 for out of memory error.
;	flags set on value in ax
;*************************************************************

;----------------------------------------------------------------
;	 For every prs with a text table in system,
;	   reset FTX_TmpDecl
;	   if prs type is FUNCTION and prs is in mrs being saved,
;	      set FTX_TmpRef bit, else reset it
;----------------------------------------------------------------
cProc	SdPass1,<NEAR>
cBegin
	and	[txdCur.TXD_flags],NOT (FTX_TmpDecl OR FTX_TmpRef)
					;start out by turning both bits off
	cmp	[prsCur.PRS_procType],PT_FUNCTION
	jne	Sd1ResetBits		;exit if SUB
	mov	ax,[oMrsSaveDecl]
	cmp	ax,[prsCur.PRS_oMrs]
	jne	Sd1ResetBits		;exit if Func defined in another module

	;for func in module, assume it is referenced.  For external func
	;refs, even qbi requires user have a DECLARE stmt for it.
	
	or	[txdCur.TXD_flags],FTX_TmpRef ;turn on FTX_TmpRef bit
Sd1ResetBits:
	mov	ax,sp			;return TRUE for ForEachCP
cEnd

;-----------------------------------------------------------------
;	 For every text table in module being saved:
;	   Search text table for a reference to a SUB or FUNCTION
;	    if opStDeclare ref found
;	       set FTX_TmpDecl bit
;	    else if CALL, CALLS, implied CALL
;	       set FTX_TmpRef bit
;-----------------------------------------------------------------
cProc	SdPass2,<NEAR>,<si>
cBegin
	SetStartOtx si			;otxCur = start of text
Sd2Loop:
	push	si
	PUSHI	ax,<CODEOFFSET tOpDecl>
	call	TxtFindNextOp		;ax = otx to next opStDeclare opcode
	cmp	dl,DECL_opEot
	je	Sd2Exit
	xchg	si,ax			;si = new otxCur
	GetSegTxtTblCur			;es = seg addr of text table
	mov	ax,es:4[si]		;ax = oPrs field
	call	PPrsOPrs		; es:bx points to prs structure
					;all other regs preserved
	test	BPTRRS[bx.PRS_flags],FP_DEFINED 
	je	Sd2Loop			;don't count references to native-code
					; procedures, only those defined with
					; a SUB/FUNCTION stmt
	mov	al,FTX_TmpRef
	.errnz	DECL_opStDeclare
	or	dl,dl			;dl = 0 for DECLARE, non-zero for CALL
	jne	Sd2SetBit		;brif CALL
	mov	al,FTX_TmpDecl
Sd2SetBit:
	or	BPTRRS[bx.PRS_txd.TXD_flags],al 
	jmp	SHORT Sd2Loop

Sd2Exit:
	mov	ax,sp			;return TRUE for ForEachCP
cEnd

;***
;GetWord
;Purpose:
;	This header block added as part of revision [5]
;Preserves:
;	All but ES, BX, and SI
;******************************************************************************
GetWord PROC NEAR
	GetSegTxtTblCur			;es = seg addr of text table
	lods	WORD PTR es:[si]	;ax = cntEos
	ret
GetWord ENDP

MoveWord PROC NEAR
	call	GetWord
	jmp	Emit16_AX		;emit cntEos operand
					; and return to caller
MoveWord ENDP

;------------------------------------------------------------------------------
;	 For every prs with a text table in system,
;	   if FP_DEFINED and FTX_TmpRef bit are set, and FTX_TmpDecl bit is not,
;	   copy pcode for definition to module, changing opcode to opStDeclare,
;	   and changing the oNam for each formal parm and explicitly
;	   listing the TYPE.
;
;------------------------------------------------------------------------------
cProc	SdPass3,<NEAR>,<si,di>
	localW	oNamParm
cBegin
	test	[prsCur.PRS_flags],FP_DEFINED
	je	J1_Sd3Exit		; don't count references to
					; undefined procedures

	test	[txdCur.TXD_flags],FTX_TmpRef
	je	J1_Sd3Exit		;don't generate DECLARE for text tbl
					; with no references in this module
	test	[txdCur.TXD_flags],FTX_TmpDecl
	je	EmitDecl		;don't generate DECLARE for prs which
J1_Sd3Exit:
	jmp	Sd3Exit			; already has a declare in this prs

EmitDecl:
	mov	ax,[prsCur.PRS_otxDef]	; ax = otx to opStSub/Function
	mov	si,ax			;ax = si = text offset
	call	OtxDefTypeCur		;fill ps.tEtCur with default types
					; at definition of procedure
	mov	ax,opBol
	call	Emit16_AX

	mov	ax,opStDeclare
	call	Emit16_AX

	lodsw				;si=si+2 (points to cntEos parm)
	.errnz DCL_cntEos
	call	MoveWord		;move cntEos from es:[si] to ps.bdpDst

	.errnz DCL_oPrs	- 2
	call	MoveWord		;move oPrs from es:[si] to ps.bdpDst

	.errnz DCL_atr - 4
	call	GetWord			;ax = procAtr from es:[si]
	push	ax			;save proc atr
	.errnz	DCLA_procType - 0300h
	and	ah,DCLA_procType / 100h	;ah = procType
	cmp	ah,PT_FUNCTION
	jne	NoProcType		;brif this is not a FUNCTION
  	.errnz DCLA_Explicit - 0080h
	or	al,al
	js	NoProcType		;brif it was explicitly typed
	push	[prsCur.PRS_ogNam]	
	call	ONamOfOgNam		; ax = oNam of this prs
	DbAssertRel  ax,nz,0,CP,<txtsave.asm: ONamOfOgNam returned ax = 0>
	cCall	OTypOfONamDefault,<ax>	; ax = default oTyp (ax)
  	or	al,DCLA_Explicit	;remember this was Explicitly typed
	pop	dx
	mov	ah,dh			;ax = new procAtr
	push	ax
;top of stack = procAtr
NoProcType:
	call	Emit16			;emit proc atr operand

	.errnz DCL_cParms - 6
	call	GetWord			;ax = cParms operand from es:[si]
	mov	di,ax			;di = cParms
	call	Emit16_AX		;emit cParms operand
	inc	di
Sd3ParmLoop:
	dec	di			;decrement parm count
	jz	Sd3Exit			;brif done with parms
	.errnz	DCLP_id - 0	
	call	GetWord			;ax = parm's oNam or oVar
	cCall	oNamoVarRudeOrParse,<ax>;if we text not in rude map oVar
					; to oNam
	mov	[oNamParm],ax
	mov	dx,[oMrsSaveDecl]
	call	ONamOtherOMrs		;ax = equivalent oNam in module dx
					; (es is preserved)
	je	Sd3OmExit		;brif OM error (AX=0) to stop ForEach
	call	Emit16_AX		; oVar in SS_PARSE or SS_EXECUTE

 	.errnz DCLP_atr - 2		;Formal parm attributes (PATR_xxx)
	call	GetWord			;ax = formal parm atr
	push	ax			;save parmAtr
	.errnz	PATR_asClause AND 0FFh
	test	ah,PATR_asClause / 100h
	jne	Sd3AsClause		;brif 'id AS xxx'
	.errnz	PATR_explicit AND 0FFh
	or	ah,PATR_explicit / 100h	;in DECLARE, force it to be explicit
Sd3AsClause:
	call	Emit16_AX

	; if not SS_RUDE, it is oTyp of user type.
	
	.errnz DCLP_oTyp - 4		;Type of the formal parm
	call	GetWord			;ax = oNam for <user type> if > ET_MAX

	pop	bx			;bx = parmAtr
	.errnz	PATR_asClause AND 0FFh
	.errnz	PATR_explicit AND 0FFh
	test	bh,(PATR_explicit OR PATR_asClause) / 100h
	jne	NotImpl			;brif not implicitly typed
	push	[oNamParm]		
	call	OTypOfONamDefault	;ax = default oTyp for parm (ax)
NotImpl:
	cmp	ax,ET_MAX
	jbe	NotUserTyp		;brif it is a primitive type

	;Since declares are inserted before any type declarations, we cannot
	;insert any references to a type name in the declare.  SOOO, we
	;just always use as ANY for synthetic declares with user defined
	;types.
	sub	ax,ax			;ax = AS ANY
NotUserTyp:
	call	Emit16_AX
	jmp	SHORT Sd3ParmLoop

Sd3Exit:
	mov	ax,sp			;return TRUE for ForEachCP
Sd3OmExit:
cEnd

;-------------------------------------------------------------
; SaveDeclares - main code
;-------------------------------------------------------------
PUBLIC	SaveDeclares			;for debugging only
cProc	SaveDeclares,<NEAR>,<si>
cBegin
DbAssertRelB [txdCur.TXD_scanState],e,SS_RUDE,CP,<SaveDeclares:TxdCur not in SS_RUDE>
	call	PrsDeactivate		;make module's txt tbl active
	mov	ax,[grs.GRS_oMrsCur]
	mov	[oMrsSaveDecl],ax

	test	[mrsCur.MRS_flags2],FM2_Include ;is this an include mrs?
	jne	SdGoodExit		;don't insert decls into include
					;mrs's.  Re-Including could break
					;a previously running program.

	;For each prs in system which has a text table:
	mov	al,FE_PcodeMrs+FE_PcodePrs+FE_SaveRs
	mov	bx,CPOFFSET SdPass1	;bx = adr of function to call
	call	ForEachCP

	;For each text table in module being saved:
	mov	al,FE_CallMrs+FE_PcodePrs+FE_SaveRs
	mov	bx,CPOFFSET SdPass2	;bx = adr of function to call
	call	ForEachCP

	sub	ax,ax			
	mov	[ps.PS_bdpDst.BDP_cbLogical],ax
	call	SetDstPbCur

	;For each prs in system which has a text table:
	mov	al,FE_PcodeMrs+FE_PcodePrs+FE_SaveRs
	mov	bx,CPOFFSET SdPass3	;bx = adr of function to call
	call	ForEachCP
	je	SdExit			;brif out-of-memory

	SetStartOtx si			;insert DECLAREs at start of module
	call	TxtInsert
	je	SdExit			;brif out-of-memory

	SetStartOtx si			;otxInsert = start of text
	mov	bx,[ps.PS_bdpDst.BDP_cbLogical] ;pass cbInserted in bx
	or	bx,bx			;was any pcode inserted?
	je	NoDeclaresInserted	;brif not
	or	[mrsCur.MRS_flags2],FM2_Modified ;set modified bit so compiler
					;will compile same source as QBI for
					;MakeExe.
	push	bx			;save cbInsert
	call	DrawDebugScrFar		;update list windows for inserted text
	pop	bx			;restore bx=cbInsert
NoDeclaresInserted:
	call	TxtInsUpdate
SdGoodExit:				
	mov	ax,sp			;return non-zero (not out-of-memory)
SdExit:
	or	ax,ax			;set condition codes
cEnd


;*************************************************************
; SaveAllDeclares
; Purpose:
;	Generate synthetic DECLARE stmts for forward referenced
;	SUBs and FUNCTIONs for every module in the system.
;	Called by UI before MakeExe to ensure that Compiler
;	will compile same source as interpreter.  This solves
;	the situation for a QB2/3 program is loaded and works
;	correctly for QBI, but will not compile in BC.	If we
;	have inserted synthetic declares, or altered the pcode
;	in some way, we need to make sure that the dirty bit
;	gets set for the module.
; Entry:
;	none.
; Exit:
;	grs.fDirect = FALSE
;	ax = 0 for no error, else QBI standard error code.
;*************************************************************
cProc	SaveAllDeclares,<PUBLIC,FAR>
cBegin
	;For each mrs in system which has a pcode text table:
	mov	al,FE_PcodeMrs+FE_CallMrs+FE_SaveRs
	mov	bx,CPOFFSET SaveDeclares ;bx = adr of function to call
	call	ForEachCP
	mov	ax,ER_OM		;default Out of memory error
	je	SaveAllDeclaresExit	;brif out-of-memory
	sub	ax,ax
SaveAllDeclaresExit:
cEnd

;*************************************************************
; ushort AsciiSave()
; Purpose:
;	ASCII save the current module (with all its procedures)
;
; Exit:
;	grs.fDirect = FALSE
;	ps.bdpSrc is used
;	ax = 0 if no error, else Standard BASIC error code (i.e. ER_xxx)
;
; Exceptions:
;	Can cause runtime error (Out of memory, I/O errors)
;
;*************************************************************
cProc	AsciiSave,<NEAR>,<si>
cBegin
	call	AlphaBuildORs		; build sorted list of all oRs's
	or	ax,ax			;set flags based on returned value
	mov	ax,ER_OM		;prepare to return Out-of-memory error
	je	AsDone			;brif error
	call	PrsDeactivate		;make module's txt table active
	sub	ax,ax
	mov	[fLsDynArrays],al	;default state is $STATIC
	DbAssertRel ax,e,0,CP,<AsciiSave: ax!=0> ;SaveTxdCur needs ax=0

;ax = otx of 1st line in current text table to be written to file
AsLoop:
	call	SaveTxdCur		;save module/procedure text table
	test	[mrsCur.MRS_flags2],FM2_NoPcode ; document file?
	jne	NotModuleText		; brif so, never add blank line
	cmp	ax,2			;was last line a blank one?
	jbe	NotModuleText		;brif so
	call	OutCrLf 		;output a blank line so comment blocks
					;are associated with correct text tbls
NotModuleText:
	call	OtxDefTypeEot		;fill ps.tEtCur with default types
					; at end of module/procedure
	call	NextAlphaPrs		;activate next procedure in module
	or	ax,ax			;set flags
	je	AsDone			;brif no more procedures in module
	SetStartOtx ax			
	test	[prsCur.PRS_flags],FP_DEFINED
	je	ProcNotDefined		;brif no SUB/FUNCTION stmt
	push	[prsCur.PRS_otxDef]	;push offset to opStSub/opStFunction
	call	OtxBolOfOtx		;ax = text offset for 1st line of SUB
ProcNotDefined:
	mov	si,ax			;si = ax = otxProcDef
	call	SaveProcHdr		;save proc hdr(ax) (may contain some
					; synthetically generated statements
	jne	AsDone			;brif error
	xchg	ax,si			;ax = otxProcDef
	jmp	SHORT AsLoop

;al = 0 if no error, else standard QBI error code
AsDone:
cEnd	;AsciiSave

;****************************************************************************
;SaveModName - save the name of the current module to the file
;
;Purpose:
;	Used by Save to save the name of each module in a .MAK file.
;Entry:
;	The .MAK file is open to current channel
;	si points to static buffer holding name of the MAK file's directory.
;	di points to static buffer which can be used to hold module's name
;Exceptions:
;	Assumes caller called RtSetTrap to trap runtime errors.
;
;****************************************************************************
SaveModName PROC NEAR
	mov	ax,di			; pDest (parm to CopyOgNamPbNear)
	mov	bx,[mrsCur.MRS_ogNam]	; ogNam (parm to CopyOgNamPbNear)
	call	CopyOgNamPbNear		; copies name to buffer, returns 
					;   ax = cbName
	mov	bx,di			
	add	bx,ax			; add cbName
	mov	BYTE PTR [bx],0		; zero terminate

	;MakeRelativeFileSpec(szFilename, szMakDirectory)
	cCall	MakeRelativeFileSpec,<di,si> ;convert szFilename to relative
					; path from szMakDirectory if possible
	cCall	CbSz,<di>		;ax = length of result path
					;ax = size of line to output
	mov	bx,di			;bx points to start of line to output
	call	OutLine			;output the line
	ret
SaveModName	ENDP

;****************************************************************************
; FNotMainModule
; Purpose:
;	Called via ForEachCP to see if there is any pcode module
;	that is not the main module (i.e. to see if this is a
;	multiple-module program.
; Exit:
;	Return 0 in ax if current module is not main-module
;	else return non-zero in ax
;
;****************************************************************************
FNotMainModule PROC NEAR
	mov	ax,[grs.GRS_oMrsCur]
	cmp	ax,[grs.GRS_oMrsMain]
	mov	ax,sp			;prepare to return non-zero
	je	FNotMainExit
	sub	ax,ax			;return 0 (not main module)
FNotMainExit:
	ret
FNotMainModule ENDP

;*************************************************************
; SaveMakFile
; Purpose:
;	Called by SaveFile to see if we're saving the main module
;	of a multi-module program.  If so, this creates <filename>.MAK
;	file and writes the names of all modules in the program.
; Entry:
;	mrsCur.ogNam is current module's filename
; Exit:
;	ax = error code (0 if none), condition codes set
; Exceptions:
;	assumes caller has called SetRtTrap to trap runtime errors
;
;*************************************************************
cProc	SaveMakFile,<NEAR>,<si,di>
	localV	szDir,FILNAML		
	localV	filenameNew,FILNAML	; size expected by runtime routines
					; used for filename normalization
	localV	sdFilenameNew,<SIZE SD>
cBegin
	mov	ax,[grs.GRS_oMrsMain]
	cmp	ax,[grs.GRS_oMrsCur]
	jne	SmfGood			;brif this isn't main module

	mov	bx,si			;bx = psdFilename
	lea	si,[sdFilenameNew]	;si = &sdFilenameNew
	lea	di,[filenameNew]	
	mov	[si.SD_pb],di		; set up string descr.
	call	MakFilename		;fill di with <moduleName>.MAK
	jne	SmfExit 		;brif Bad File Name

	mov	al,FE_PcodeMrs+FE_CallMrs+FE_SaveRs
	mov	bx,CPOFFSET FNotMainModule ;bx = adr of function to call
	call	ForEachCP		;ax=0 if multi-module program
	je	MultiModules		;brif multi-module program is loaded
	push	di			;pass ptr to szFilenameNew
	call	DelFile			;delete filename.MAK
	jmp	SHORT SmfGood		;exit if not multi-module program

;Open filename in sdFilename (si) (.MAK file) and write all module names to it
MultiModules:
	;If we could assume DOS 3.0 or greater, (we can't yet) we could set
	;dx to (ACCESS_WRITE OR LOCK_BOTH) SHL 8 OR MD_SQO
	
	mov	dx,MD_SQO
	call	OpenChan		;al = error code (0 if no error)
	jne	SmfExit			;brif errors

	;fill si with sz for directory of .MAK file
	lea	si,szDir		;si points to working static buffer
	push	di			;pass pbSrc (filenameNew)
	push	si			;pass pbDst (szDir)
	mov	bx,[sdFilenameNew.SD_cb]
	push	bx			;pass byte count
	mov	BYTE PTR [bx+si],0	;0-terminate destination
	call	CopyBlk			;copy module name to static buffer
	push	si			;pass szDir
	call	FileSpec		;ax points beyond pathname
	xchg	bx,ax			;bx points beyond pathname
	mov	BYTE PTR [bx-1],0	;0-terminate szDir

	;Save the name of the Main Module first, so it will be loaded first
	;si points to szDir
	;di points to filenameNew (will be used for temp buffer)
	
	call	SaveModName		;write main module's relative path
	call	MrsDeactivate		;start writing other module names
SmLoop:
	call	NextMrsFile_All 	;make next mrs active
	inc	ax			;test for UNDEFINED (end of mrs list)
	je	SmDone			;brif done with all mrs's
	dec	ax			;restore ax = module's name
	cmp	ax,[grs.GRS_oMrsMain]
	je	SmLoop			;brif this is MAIN mod (already output)
	test	[mrsCur.MRS_flags2],FM2_NoPcode OR FM2_Include
	jne	SmLoop			;skip document and include mrs's
	call	SaveModName
	jmp	SHORT SmLoop

SmDone:
	push	[grs.GRS_oMrsMain]	;we know the main module was active
	call	MrsActivateCP		; on entry - reactivate it on exit

	call	CloseChan		;close [chanCur]
SmfGood:
	sub	ax,ax
SmfExit:
	or	ax,ax			;set condition codes for caller
cEnd

;*************************************************************
; ushort SaveFile()
; Purpose:
;	Open the specified file and save program to it.
;
; Entry:
;	mrsCur.ogNam = filename to be saved.
;	   (the filename need not be 0-byte terminated)
;	mrsCur.flags2 FM2_AsciiLoaded is TRUE for ASCII Save
;	FOR EB: parm1 = mode for opening file
;
; Exit:
;	ps.bdpSrc is used
;	grs.fDirect = FALSE
;	ax = 0 if no error, else Standard BASIC error code (i.e. ER_xxx)
;
;*************************************************************
cProc	SaveFile,<PUBLIC,FAR,NODATA>,<si>
	localV	FileName,FILNAML	
	localV	sdFileName,<SIZE SD>	
cBegin
	mov	ax,-MSG_Saving		;display Saving msg in intense video
	call	StatusMsgCP		; to tell user we're loading
	call	AlphaORsFree		;release table of sorted oRs's
					; (user interface may have chosen
					;  a new name for this mrs)
	push	[grs.GRS_oRsCur]	;save mrs/prs - restored on exit

	call	RtPushHandler		;save caller's runtime error handler
					; could be called by LoadFile->NewStmt
					; (NOTE: alters stack pointer)
	SetfDirect al,FALSE		;turn off direct mode
	mov	ax,CPOFFSET SfDone	;if any runtime errors occur,
	call	RtSetTrap		;branch to SfDone with sp,di =
					;current values

					; doesn't have to be recompiled
	call	ModuleRudeEdit		
	call	SaveDeclares		;generate synthetic DECLARE stmts
					; for forward-referenced
	mov	ax,ER_OM		;default to OM error
	je	SfDone			;brif error

	lea	si,[sdFileName]		;cant use buffers here used for
	lea	ax,[FileName]  		;load because we may need to save
	mov	[si.SD_pb],ax		;current module during fileopen
	mov	bx,[mrsCur.MRS_ogNam]	
	call	CopyOgNamPbNear		; ax = number of chars copied
	mov	[si.SD_cb],ax		
	call	SaveMakFile		;create <filename>.MAK if main
					; program of multi-module program.
	jne	SfDone			;brif errors

	;If we could assume DOS 3.0 or greater, (we can't yet) we could set
	;dx to (ACCESS_WRITE OR LOCK_BOTH) SHL 8 OR MD_SQO
	
	mov	dx,MD_SQO
	call	OpenChan		;[chanCur] = channel #
	jne	SfDone			;brif error


DoAsciiSave:
	call	AsciiSave		;al = errCode

;We're done trying to write the file, now try to close it.
;Closing the file can cause I/O errors when close flushes the buffer.
;al = 0 if no error, else standard QBI error code
SfDone:
	sub	ah,ah			;ax = error code
SfDone2:				
	xchg	si,ax			;si = return value
	test	[flagsTM],FTM_SaveProcHdr
	je	NoShCleanup		;brif SaveProcHdr was not in critical
					; section.
	call	RelShBuf		;release temp text tbl used by
					; SaveProcHdr
NoShCleanup:
	call	RtFreeTrap		;free previous trap address
	mov	ax,CPOFFSET SfGotErr	;if any runtime errors occur,
	call	RtSetTrap		;branch to SfGotErr with sp,bp,si,di
					;set to current values
	call	CloseChan		;close file before kill
					; (sets ChanCur = 0)
	cCall	RtFreeTrap		; release error handler
	test	si,7FFFh		; test low 15 bits for error code
	je	SfNoErr			;brif no error before close

	xor	ax, ax			; no error during close

;If we got an error during save, delete partially created file
SfGotErr:

	test	si, 7fffh		; do we already have an error
	jnz	SfTestDelFile		; brif so, use it
	or	si, ax			; else add in new error


;	Only delete the file if we actually created and started to save
;	a binary file.	We don't want to delete an existing file if we
;	got an error on or before the open, and we also don't want to
;	delete a partially written ascii file.

SfTestDelFile:
	or	si,si			; got to BinarySave?
	jns	SfExit			; no, then don't kill the file

	push	di			
	mov	ax,CPOFFSET SfKillErr	; trap & ignore any runtime errors
	cCall	RtSetTrap		; in KILL 
	sub	sp,((FILNAML+SIZE SD+2)+1) AND 0FFFEh ;[3] create a fake sd
						      ;    on the stack
	mov	di,sp			
	add	di,6			; pnt to strt of where string will be
	mov	[di-2],di		; setup pb part of fake sd
	mov	ax,di			; parm to CopyOgNamPbNear
	mov	bx,[mrsCur.MRS_ogNam]	; parm to CopyOgNamPbNear
	call	CopyOgNamPbNear		; copy name onto stack, ax = cbName
	sub	di,4			; di = pFakeSd
	mov	[di],ax			; set up cb part of fake sd
	cCall	B$KILL,<di>		; call rt to delete file
	add	sp,((FILNAML+SIZE SD+2)+1) AND 0FFFEh ;[3] restore stack ptr

SfKillErr:				; branched to if error during KILL
	pop	di			

	jmp	SHORT SfExit

SfNoErr:
	and	[mrsCur.MRS_flags2],NOT (FM2_Modified or FM2_ReInclude) 
	and	[mrsCur.MRS_flags3],NOT FM3_NotFound ;If the user told
					;us to save the file, we have
					;found it.
	test	[mrsCur.MRS_flags2],FM2_Include
	je	SfExit			;brif this is not an $INCLUDE file
	or	[flagsTm],FTM_reInclude	;re-parse all $INCLUDE lines in
					;all modules before next RUN
SfExit:
	and	[flagsTM],NOT FTM_SaveProcHdr ;reset critical section flag
	call	RtPopHandler		;restore caller's runtime error handler
					; (saved on stack by RtPushHandler)
	call	RsActivateCP		;restore caller's mrs/prs
	call	StatusMsg0CP		;tell user interface we're done saving
	xchg	ax,si			;restore ax = error code
	and	ah,7Fh			; clear BinarySave flag bit
cEnd


sEnd	CP

end
