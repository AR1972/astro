	TITLE	txtthr.asm - Text Table Linked List Maintenance Functions

;==========================================================================
;
;Module:  txtthr.asm - Text Table Linked List Maintenance Functions
;System:  Quick BASIC Interpreter
;
;=========================================================================*/

	include		version.inc
	TXTTHR_ASM = ON
	IncludeOnce	architec
	IncludeOnce	context
	IncludeOnce	opcodes
	IncludeOnce	opaftqb4
	IncludeOnce	qbimsgs
	includeOnce	txtint
	includeOnce	txtmgr
	includeOnce	types
	includeOnce	variable

	assumes	DS,DATA
	assumes	SS,DATA
	assumes	ES,NOTHING

sBegin	CODE

;These opcodes are of special interest whenever they are inserted:
;
PUBLIC	tOpUpdl
tOpUpdl	LABEL WORD
	opTabStart	UPDL
	opTabEntry	UPDL,opLab
	opTabEntry	UPDL,opLabSp
	opTabEntry	UPDL,opBolLab
	opTabEntry	UPDL,opBolLabSp
		UPDL_labMax EQU UPDL_opBolLabSp
	; Put in opBol to keep terminate most searches in update links
	; before encountering opEot.  This significantly speeds up generic
	; edits which don't have any threads past the point of the edit.
	opTabEntry	UPDL,opBol
		UPDL_SkipMax EQU UPDL_opBol
	opTabEntry	UPDL,opReParse
	opTabEntry	UPDL,opStDefType
	opTabEntry	UPDL,opStType
	opTabEntry	UPDL,opStEndType
	opTabEntry	UPDL,opStDefFn
	opTabEntry	UPDL,opStEndDef
	opTabEntry	UPDL,opEndSingleDef
	opTabEntry	UPDL,opEot

;Opcodes which are valid within TYPE/END TYPE blocks
;
tOpTypes LABEL WORD
	opTabStart	TYPES
	opTabEntry	TYPES,opElemRef
	opTabEntry	TYPES,opAsType
	opTabEntry	TYPES,opAsTypeExp
	opTabEntry	TYPES,opAsTypeFixed	
	opTabEntry	TYPES,opStEndType
		TYPES_DispatchMax EQU TYPES_opStEndType
	; the following opcodes are allowed but no processing is required
	; by ScanBlock
	opTabEntry	TYPES,opBol
	opTabEntry	TYPES,opBos
	opTabEntry	TYPES,opBolSp
	opTabEntry	TYPES,opBolInclude
	opTabEntry	TYPES,opBolIncludeSp
	opTabEntry	TYPES,opStRem
	opTabEntry	TYPES,opQuoteRem
	opTabEntry	TYPES,op_Static
	opTabEntry	TYPES,op_Dynamic
	opTabEntry	TYPES,op_Include
	opTabEntry	TYPES,opEot

sEnd	CODE

sBegin	CP
assumes	cs,CP

;Threaded pcode descriptor
THR_DSC STRUC
THR_otxPrev	DW 0	;points to previous thread
THR_fInDS	DW 0	;non-zero if THR_otxPrev is a DS offset
THR_DSC ENDS

;*************************************************************************
; TxtDeThread(ax:pHead, di:otxDelFirst, cx:cbDel)
;
; Purpose:
;	Called when a block of text is deleted.
;	Traverse a linked list through the current text table,
;	removing any nodes which are being deleted, and updating
;	next-node-offsets for those nodes beyond the deleted range.
;	This function is called BEFORE the text has been deleted.
;
; Entry:
;	ax =  pointer to ushort which heads linked list
;	       for example, &txdCur.otxLabLink
;	di =  text offset to 1st byte being deleted
;	cx =  number of bytes being deleted
;
; Preserves:
;	cx (callers depend on this)
;
;*************************************************************************
cProc	TxtDeThread,<PUBLIC,NEAR>,<si>
	localW	segTxtTab		;text table's segment
cBegin
	xchg	si,ax			;si points to head of list
	DbChk	TxdCur			;perform sanity check on txdCur
	GetSegTxtTblCur			;es = seg adr of cur txt tbl
					;preserves cx
	mov	[segTxtTab],es		;save text table's segment
	mov	dx,ds			;dx -> prev link node's segment (DGROUP)
	mov	bx,[si] 		;bx -> 1st node in linked list (in ES)
	mov	ax,di
	add	ax,cx			;ax -> last byte deleted
	DbChk	Otx,ax
DeThrLoop:
	cmp	bx,UNDEFINED
	je	DeThrExit		;branch if done with linked list
	DbChk	Otx,bx
	cmp	bx,di
	jb	DeThrUnaffected 	;brif node is unaffected by delete
					; (i.e. node < 1st byte deleted)
	cmp	bx,ax
	jb	DeThrDeleted		;brif node is in deleted range
					; (i.e. node < last byte deleted)
					;else node is beyond deleted range
	mov	es,dx			;es = segment of predecessor node
					;may be DGROUP:pHead or es:<node>
	mov	es:[si],bx		;update predecessor node
	sub	es:[si],cx
	mov	es,[segTxtTab]		;es = text table's segment
DeThrUnaffected:
	mov	si,bx			;save offset of prev node ptr in si
	mov	dx,es			;save seg of prev node ptr in dx
DeThrDeleted:
	mov	bx,es:[bx]		;bx points to next node in list
	jmp	SHORT DeThrLoop
DeThrExit:
	mov	es,dx			;es = segment of predecessor node
					;may be DGROUP:pHead or es:<node>
	mov	es:[si],bx		;update predecessor node
cEnd

;**********************************************************************
; TxtDelThread
; Purpose:
;	Called BEFORE pcode is deleted from current text table.
;	Traverses all linked lists through the text table,
;	updating the linked lists for the delete.
; Entry:
;	parm1 = offset to start of delete
;	parm2 = cbDel
;
;**********************************************************************
; TxtDeThread(&txdCur.otxLabLink, otxDelFirst, cbDel)
; TxtDeThread(&txdCur.otxReParseLink, otxDelFirst, cbDel)
; TxtDeThread(&txdCur.otxDefTypeLink, otxDelFirst, cbDel)
; TxtDeThread(&txdCur.otxTypeLink, otxDelFirst, cbDel)
;
; NOTE: TxtDeThread takes ax,cx,di as parms and preserves cx
;
cProc	TxtDelThread,<PUBLIC,NEAR>,<si,di>
	parmW	otxStart
	parmW	cbDel
cBegin
	mov	di,[otxStart]
	mov	cx,[cbDel]
	mov	si,CPOFFSET TxtDeThread	;si = adr of function to call
	mov	ax,dataOFFSET txdCur.TXD_otxTypeLink
	call	si			;fixup otxTypeLink list
	mov	ax,dataOFFSET txdCur.TXD_otxDefTypeLink
	call	si			;fixup otxDefTypeLink list
	mov	ax,dataOFFSET txdCur.TXD_otxReParseLink
	call	si			;fixup otxReParseLink list
	mov	ax,dataOFFSET txdCur.TXD_otxLabLink
	call	si			;fixup otxLabLink list

	test	[txdCur.TXD_flags],FTX_mrs
	je	DelNotMod		;brif not module text table

	; The linked list of opcodes headed by mrsCur.MRS_data_otxFirst
	; only needs to be maintained while in SS_EXECUTE state, so
	; we don't need to fix it up here
	
	mov	ax,dataOFFSET mrsCur.MRS_otxDefFnLink
	call	si	
DelNotMod:
cEnd

;*************************************************************************
; TxtInsThread(pHead, pThrDsc, otxInsFirst, cbIns)
;
; Purpose:
;	Called when a block of text is inserted.  Traverse a linked
;	list through the current text table, updating next-node-offsets
;	for those nodes beyond the inserted range.
;	This function is called AFTER the text has been inserted.
;
; Entry:
;	parm1: pointer to ushort which heads linked list
;	       for example, &txdCur.otxLabLink
;	parm2: pointer to thread descriptor which on exit contains fields:
;	       THR_otxPrev: offset to head or node which precedes the node
;			being inserted in the list
;	       THR_fInDS:   non-zero if THR_otxPrev is an offset into DS
;			zero if THR_otxPrev is an offset into text table
;	parm3: text offset to 1st byte inserted
;	parm4: number of bytes being deleted
;
; Exit:
;	*pThrDsc is updated so it refers to the last link which
;	precedes otxInsFirst.
;
;*************************************************************************
cProc	TxtInsThread,<PUBLIC,NEAR>,<si,di>
	parmW	pHead
	parmW	pThrDsc
	parmW	otxInsFirst
	parmW	cbIns
cBegin
	DbChk	TxdCur			;perform sanity check on txdCur
	mov	si,[pHead]		;si -> head of linked list (in DGROUP)
	mov	di,[pThrDsc]
	mov	[di.THR_otxPrev],si	;default THR_otxPrev to head of list
	mov	[di.THR_fInDS],1 	;set flag indicating THR_otxPrev is
					; offset in DS
	GetSegTxtTblCur			;es = seg adr of cur txt tbl
	mov	ax,es			;ax = seg adr of cur txt tbl
					;NOTE: ax=es for remainder of function
	mov	dx,ds			;dx -> prev link node's segment (DGROUP)
	mov	bx,[si] 		;bx -> 1st node in linked list (in ES)
	mov	cx,[cbIns]		;cx = #bytes inserted
InsThrLoop:
	cmp	bx,[otxInsFirst]	;compare with adr of 1st byte inserted
	jb	InsThrUnaffected	;brif node is unaffected by insert
					; (i.e. node < 1st byte inserted)
	cmp	bx,UNDEFINED
	je	InsThrExit		;branch if done with linked list
	mov	es,dx			;es = segment of predecessor node
					;may be DGROUP:pHead or es:<node>
	add	bx,cx			;bx points to where node is after insert
	mov	es:[si],bx		;update predecessor node
	mov	es,ax			;es = text table's segment
	jmp	SHORT NextLink

InsThrUnaffected:
	mov	[di.THR_otxPrev],bx	;THR_otxPrev points to last link before
					; inserted block of text
	mov	[di.THR_fInDS],0 	;reset flag to 0, indicating THR_otxPrev
					; is relative to the text segment
NextLink:
	mov	si,bx			;save offset of prev node ptr in si
	mov	dx,es			;save seg of prev node ptr in dx
	mov	bx,es:[si]		;bx points to next node in list
	jmp	SHORT InsThrLoop

InsThrExit:
cEnd

;*************************************************************************
; TxtAddThread(pThrDsc, otxNewLink)
;
; Purpose:
;	Called AFTER a block of text has been inserted which contains
;	an opcode having an operand that should be added to a linked
;	list through the text table.  This function inserts this opcode
;	in the list.
;
; Entry:
;	parm1: pointer to thread descriptor which contains following fields:
;	       THR_otxPrev: offset to head or node which precedes the node
;			being inserted in the list
;	       THR_fInDS: non-zero if THR_otxPrev is an offset into DS
;			zero if THR_otxPrev is an offset into text table
;	parm2: pointer to new link field to add to linked list
;
;*************************************************************************
cProc	TxtAddThread,<PUBLIC,NEAR>,<si,di>
	parmW	pThrDsc
	parmW	otxNewLink
cBegin
	DbChk	TxdCur			;perform sanity check on txdCur
	GetSegTxtTblCur			;es = seg adr of cur txt tbl
	mov	ax,es			;ax = seg adr of cur txt tbl
	mov	si,[pThrDsc]		;si -> thread descriptor
	mov	bx,[si.THR_otxPrev]	;bx -> prev node / head of list
	cmp	[si.THR_fInDS],0
	je	prevInTxt		;brif bx is offset within text table
	mov	[si.THR_fInDS],0 	;reset THR_fInDS to 0
	push	ds
	pop	es			;es -> DGROUP
prevInTxt:
	mov	dx,es:[bx]		;dx -> next node
	mov	di,[otxNewLink] 	;di -> new node
	DbChk	Otx,di
	mov	es:[bx],di		;prev node -> new node
	mov	es,ax			;es -> text table
	mov	es:[di],dx		;new node -> next node
	mov	[si.THR_otxPrev],di	;save adr of new node in thread dsc
cEnd

;**************************************************************
; void NEAR UpdateLinks(otxInsStart, otxInsEnd)
; void NEAR NoWalkUpdateLinks(otxInsStart, otxInsEnd)
;
; Purpose:
;	Called after some text has been inserted into the current text table.
;	During ASCII Load, it is not called for every line, but every
;	time we switch text tables (speed optimization for LOAD).
;	For each inserted opcode which has a link-field operand, it
;	is inserted into its appropriate linked list.  The NoWalkUpdateLinks
;	entrypoint allows the binary translator to adjust links for pcode
;	growth without looking at the inserted pcode.  This is necessary as
;	the pcode is not valid until after all translation has been performed.
;
; Entry:
;	grs.oMrsCur, grs.oPrsCur have their usual meaning
;	txdCur.bdlText describes the current text table.
;	ps.bdpSrc contains source line to be inserted
;	parm1: ushort otxInsStart - text table offset to opBol
;	   opcode for 1st line inserted.
;	parm2: ushort otxInsEnd - text table offset to opBol
;	   opcode beyond last line inserted.
;	   (parm2 - parm1) should equal the number of bytes inserted.
;	   If this (parm2 - parm1), this function is effectively a nop.
;
; Exit:
;	no error or exception is possible.
;
;**************************************************************

;DoInsThread
;Entry:
;	ax = adr of head of linked list (in DS)
;	bx = adr of local thread descriptor used to remember previous link
;	si = adr of 1st byte inserted
;	di = # bytes inserted
;
DoInsThread PROC NEAR
	push	ax			;pass DS offset to head of linked list
	push	bx			;pass offset to thread descriptor
	push	si			;pass otxInsStart
	push	di			;pass cbIns
	call	TxtInsThread		;update linked list for inserted pcode
	ret
DoInsThread ENDP

cProc	NoWalkUpdateLinks,<PUBLIC,NEAR> 
cBegin
	mov	al, 00h
	SKIP2_PSW
cEnd	<nogen>

cProc	UpdateLinks,<PUBLIC,NEAR>	
cBegin
	mov	al, 01h
cEnd	<nogen>

cProc	UpdateCommon,<PUBLIC,NEAR>,<si,di>  
	parmW	otxInsStart
	parmW	otxInsEnd
	localV	thrLab,<size THR_DSC>
	localV	thrReParse,<size THR_DSC>
	localV	thrType,<size THR_DSC>
	localV	thrDefType,<size THR_DSC>
	localV	thrDefFn,<size THR_DSC>
	localB	fWalkNew		; !0 => walk inserted pcode
cBegin
	mov	fWalkNew, al		; save flag for later

	DbChk	Otx,[otxInsStart]
	DbChk	Otx,[otxInsEnd]
	mov	si,[otxInsStart]	;si = offset to 1st byte to delete

	DbAssertRel si,be,otxInsEnd,CP,<UpdateLinks err 1>
	mov	di,[otxInsEnd]		;di = offset to last byte to delete
	sub	di,si			;di = cbIns

;Traverse all linked lists through the text table,
;updating the linked lists for the insert
;
	mov	ax,dataOFFSET txdCur.TXD_otxLabLink
	lea	bx,[thrLab]
	call	DoInsThread

	mov	ax,dataOFFSET txdCur.TXD_otxReParseLink
	lea	bx,[thrReParse]
	call	DoInsThread

	mov	ax,dataOFFSET txdCur.TXD_otxTypeLink
	lea	bx,[thrType]
	call	DoInsThread

	mov	ax,dataOFFSET txdCur.TXD_otxDefTypeLink
	lea	bx,[thrDefType]
	call	DoInsThread

	test	[txdCur.TXD_flags],FTX_mrs
	je	UpdNotMod		;brif we're not looking at module text

	;fix linked lists which can only occur in a module text table
	mov	ax,dataOFFSET mrsCur.MRS_otxDefFnLink
	lea	bx,[thrDefFn]
	call	DoInsThread

	; The linked list of opcodes headed by mrsCur.MRS_data_otxFirst
	; only needs to be maintained while in SS_EXECUTE state, so
	; we don't need to fix it up here

UpdNotMod:
	mov	al, fWalkNew		; check if we need to walk new pcode
	or	al, al			
	jz	UpdDone 		; brif no need to walk inserted pcode

	push	si			;pass otxInsStart
	PUSHI	ax,<CODEOFFSET tOpUpdl>
	call	TxtFindOp		;ax = offset to 1st interesting opcode
UpdLoop:
	mov	si,ax			;si = otxCur (offset to cur opcode)
	cmp	ax,[otxInsEnd]
	jae	UpdDone			;brif beyond end of inserted pcode
	mov	al,[txtFindIndex]
	cmp	al,UPDL_labMax
	ja	UpdNotLab		;brif not a label opcode

	lea	ax,[thrLab]
	push	ax
	lea	ax,[si+2]		;ax = offset to link field
	push	ax
	call	TxtAddThread
	jmp	SHORT UpdNext

UpdNotLab:
	;OpBol is in the UPDL_SkipMax range to cause us to terminate
	; the TxtFindOp Calls before opEot if no threads are after
	; oTxInsEnd.  This significantly speeds up generic edits.
	
	cmp	al,UPDL_SkipMax 	;should we skip this opcode?
	jbe	UpdNext 		;brif so, get next opcode
	sub	al,UPDL_opReParse
	sub	ah,ah			;ax = dispatch index
	shl	ax,1			;ax = dispatch offset
	xchg	ax,bx			;bx = dispatch offset
	jmp	WORD PTR cs:UpdDisp[bx]
UpdDisp:
	DW	UpdOpReParse
	DW	UpdOpStDefType
	DW	UpdOpStType
	DW	UpdOpStEndType
	DW	UpdOpStDefFn
	DW	UpdOpStEndDef
	DW	UpdOpEndSingleDef
	DW	UpdDone			;for opEot (caller can pass
					;txdCur.cbLogical which would allow
					;opEot to be found by TxtFind...
UpdNext:
	push	si			;pass otxCur
	PUSHI	ax,<CODEOFFSET tOpUpdl>
	call	TxtFindNextOp
	jmp	SHORT UpdLoop

UpdDone:
	DbChk	TxdThr			;check all threads through cur txt table
cEnd	;UpdLinks

;for opReParse, the 1st operand is a byte count,
;               the 2nd operand is the link field
;
UpdOpReParse:
	lea	ax,[thrReParse]
	jmp	SHORT UpdAdd4

UpdOpStDefType:
	lea	ax,[thrDefType]
	push	ax
	lea	ax,[si+2]		;ax = offset to link field
	push	ax
	jmp	SHORT UpdAdd

UpdOpStType:
UpdOpStEndType:
	lea	ax,[thrType]
	push	ax
	lea	ax,[si+2]		;ax = offset to link field
	push	ax
	call	TxtAddThread
	jmp	SHORT UpdNext

;for these opcodes, the 1st operand is a byte count,
;                   the 2nd operand is the link field
;
UpdOpStDefFn:
UpdOpStEndDef:
UpdOpEndSingleDef:
	lea	ax,[thrDefFn]
UpdAdd4:
	push	ax
	lea	ax,[si+4]		;ax = offset to link field
	push	ax
UpdAdd:
	call	TxtAddThread
	jmp	UpdNext

;**************************************************************
; ushort ScanTypeBlock
; Purpose:
;	This function goes through the a TYPE block in the current text table
; NOTE:
;	Parse->Execute Scanner prevents   id AS type   statement outside
;	TYPE/END TYPE block.
; Entry:
;	si = text offset beyond opStType opcode
; Exit:
;	If no error occurred
;	   returns 0
;	   si points beyond opStEndType's operands
;	Else returns standard qbi error message code in ax (see qbimsgs.h):
;	   MSG_UndType if "x AS foo" was seen, but either foo is not defined
;	     or it is defined beyond the reference (forward refs are illegal
;	     because BASCOM doesn't know how to handle them)
;	   ER_DD if either "TYPE x" has already been defined, or
;	     2 elements within 1 type record have the same name
;	   MSG_InvInTypeBlk if some statement which is illegal within
;	     a TYPE block was seen
;	   ER_OM if out-of-memory
;	   si points to 1st byte of opcode which caused error
;
;**************************************************************
cProc	ScanTypeBlock,<PUBLIC,FAR>,<di> 
	localW	oNamElem
	localW	oTypType
cBegin
	xor	ax,ax			;ax = 0
	mov	[oNamElem],ax		;initialize [oNamElem] to zero
	lea	ax,[si+2]		;ax points to oNam field
	call	GetWOtx			;ax = oNam field
	push	ax
	call	DefineTyp
	or	ax,ax
	js	J1_StExit		;brif got some error
	mov	[oTypType],ax
	dec	si
	dec	si			;si points to opStType's opcode

	;now try to define each element within the type block
	;Element definitions in the type block have the form:
	;  foo AS bar	   ==>	opElemRef(oNam<foo>) opAsType(oNam<bar>,column)
	;  foo AS INTEGER  ==>	opElemRef(oNam<foo>) opAsTypeExp(ET_I2,column)
	;  foo AS STRING * 6 ==> opElemRef(oNam<foo>) opAsTypeExp(0x8006,column)
	
StLoop:
	mov	di,si			;di points to opStType's opcode
	mov	ax,di			;pass otxPrev in ax
	call	TxtSkipOp		;ax = offset to next opcode
	xchg	si,ax			;si = text offset
	push	di			;pass otxPrev
	PUSHI	ax,<CODEOFFSET tOpTypes>
	call	TxtFindNextOp
	cmp	ax,si
	mov	ax,MSG_InvInTypeBlk
	jne	StExit			;brif invalid opcode was found in line
	cmp	dl,TYPES_DispatchMax	
	ja	StLoop			;brif no special processing required

	lea	ax,[si+2]		;ax points to opcode's 1st operand
	push	dx			;save [txtFindIndex]
	call	GetWOtx			;ax = 1st operand
	pop	dx			;dl = [txtFindIndex]

	xor	dh,dh
	shl	dx,1			;dx = dispatch offset
	mov	bx,dx			;bx = dispatch offset
	jmp	WORD PTR CP:AsTypeDispatch[bx] ;dispatch to handler	

AsTypeDispatch:				
	DW	CPOFFSET TyOpElemRef	
	DW	CPOFFSET TyOpAsType	
	DW	CPOFFSET TyOpAsTypeExp	
	DW	CPOFFSET TyOpAsTypeFixed
	DW	CPOFFSET TyOpStEndType	

TyOpStEndType:
	mov	ax,MSG_UndElem		;prepare for no elements in type
	mov	bx,[oTypType]		; bx = ptr to type entry
	add	bx,[mrsCur.MRS_bdVar.BD_pb] ;[4] get type table base
	cmp	[bx.TYP_oElementFirst],0 ;were any elements defined?
	je	StExit			;brif not, generate error

	lodsw				;skip opStEndType opcode
	lodsw				;skip opStEndType's operand
	sub	ax,ax			;no error occurred
J1_StExit:
	jmp	SHORT StExit

; id AS within a TYPE/END TYPE block - save oNam in oNamElem
; ax = oNam field
;
TyOpElemRef:
	mov	[oNamElem],ax
StLoop2:				;fix jump out of range
	jmp	SHORT StLoop

; saw an AS <user type> within a TYPE/END TYPE block
; call DefineElem to define the element
;
TyOpAsType:
	push	ax			;pass oNam of element's user type
	push	si			;pass text offset
	call	RefTyp			;ax = oTyp for element's oNam
CheckError:
	or	ax,ax
	js	StExit			; brif RefTyp returned error code
	; Fall into dispatch routine for TyOpAsTypeExp

; saw an AS <explicit type> within a TYPE/END TYPE block -
; call DefineElem to define the element
;
TyOpAsTypeExp:
	mov	bx,CPOFFSET DefineElem	;prepare to call DefineElem
CallDefineElemxxx:
	xor	cx,cx			;cx = 0
	xchg	cx,[oNamElem]		;cx = oNam; zero [oNamElem]
	jcxz	GotADim			;brif oNam is zero
	push	cx			;pass oNam given by opElemRef
	push	[oTypType]		;pass oTyp for TYPE block being built
	push	ax			;pass oTyp for element
	call	bx			;call DefineElem[Exp]
	or	ax,ax
	je	StLoop2 		;brif no error
StExit:
	and	ah,7FH			;mask off high bit set by Variable Mgr
cEnd

; saw an AS STRING*[oNam | cb] within a TYPE/END TYPE block - 
; call DefineElemFixed to define the element
;
TyOpAsTypeFixed:			
	xchg	di,ax			;save oTyp in di
	lea	ax,[si+4]		;ax -> opcode's second operand
	call	GetWOtx 		;ax = byte count or oNam of constant
	push	ax			;pass as 1st arg to DefineElemFixed
	xchg	ax,di			;restore ax = oTyp
	mov	bx,CPOFFSET DefineElemFixed ;bx = routine to define element
	jmp	SHORT CallDefineElemxxx	

;we've encountered an AsTypexxx opcode which was not preceeded by an
; opElemRef opcode. This can occur in a Dim statement and perhaps other places.
GotADim:				
	mov	ax,MSG_InvInTypeBlk	;message "invalid in type block"
	jmp	SHORT StExit		;exit

sEnd	CP

end
