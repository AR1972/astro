	TITLE	txtdata.asm - Ascii Load DATA Stmt movement Functions

;==========================================================================
;
;Module:  txtdata.asm
;System:  Quick BASIC Interpreter
;
;This entire module could disappear if previous versions of BASCOM
;either did not allow DATA statements within a SUB, or localized
;access to DATA and RESTORE statements to the context they are in.
;For compatibility, the functions in this module move DATA statements
;and labels for RESTORE statements from SUB/FUNCTIONs to main level code.
;
;
;===========================================================================

	include		version.inc
	TXTDATA_ASM = ON
	includeOnce	architec
	includeOnce	context
	includeOnce	heap
	includeOnce	msgshort
	includeOnce	names
	includeOnce	opmin
	includeOnce	parser
	includeOnce	scanner
	includeOnce	txtmgr
	includeOnce	txtint

	assumes	DS,DATA
	assumes	SS,DATA
	assumes	ES,NOTHING

sBegin	DATA

;-----------------------------------------------------------------------------
;		Description of Table bdData
;
; This table contains 1 entry for every statement in the module significant
; to migration of DATA statements from SUB/FUNCTION to main program.
; The first byte of each entry is a type, which determines what info
; is contained in the rest of the entry as follows:
;
; DT_Data - entry describes a DATA opcode which needs to be moved from an
;           SUB/FUNCTION's text table to the module's text table.
;	    It is followed by 2 byte oPrs and a 2 byte text offset to
;	    the opStData opcode within the prs.  This entry is created
;	    by TDataEntry when text mgr sees opStData during ASCII Load.
;
; DT_Restore - entry describes a RESTORE opcode which may or may not refer to 
;           a label within a SUB/FUNCTION's text table.
;	    It is followed by 2 byte oPrs and a 2 byte text offset to
;	    the opStRestore1 opcode.  This entry is created
;	    by TDataEntry when text mgr sees opStRestore1 during ASCII Load.
;
; DT_EndProc - entry is created when we see the end of a SUB/FUNCTION during
;	    ASCII Load.  It is followed by 2 byte oPrs and a 2 byte text offset
;	    into the module's text table, where the SUB/FUNCTION was encountered
;	    during the Load.
;
; DT_Label - entry describes a new label to be generated in the main program.
;	    This is needed because the label in the prs which was refered to
;	    by a RESTORE, may also be refered to by GOTO or GOSUB.
;	    This entry is created by TDataEnd after the Load has completed.
;
; DT_End  - marks the end of the table.
;
;-----------------------------------------------------------------------------
bdData	bd	<0,0,0>
fIgnoreTDataEntry	DB TRUE 	

CB_TDATA EQU 5				;5 bytes per entry

PUBLIC	fDataMoved
;set to TRUE every time a DATA statement is moved from a SUB to module level
fDataMoved	DB	0		

sEnd	DATA

sBegin	CP
assumes	cs,CP

;*****************************************************************************
; TDataStart
; Purpose:
;	Called at the start of ASCII Load to initialize things for movement
;	of DATA statements from prs to module.
; Exit:
;	returns al=ER_OM if out-of-memory
;	        al=0 if no error
;	        (with condition codes set accordingly)
;
;*****************************************************************************
PUBLIC	TDataStart
TDataStart PROC NEAR

; Don't allow nested calls to TDataStart
	DbAssertRelB [fIgnoreTDataEntry],ne,0,CP,<TDataStart: nested call>

	sub	ax,ax
	mov	[fIgnoreTDataEntry],al
	mov	[bdData.BD_cbLogical],ax

	PUSHI	ax,<dataOFFSET bdData>
	PUSHI	ax,CB_TDATA		;initial size of node buffer
	PUSHI	ax,<IT_NO_OWNERS>	;heap type
	call	BdAlloc
	or	ax,ax			;set condition codes for caller
	mov	al,ER_OM
	je	TDStartExit		;brif out-of-memory error
GotBd:
	mov	bx,[bdData.BD_pb]
	mov	BYTE PTR [bx],DT_End	;make table look empty
					;condition codes still based on ax
	sub	ax,ax			;no error
TDStartExit:
	or	ax,ax			;set condition codes for caller
	ret
TDataStart ENDP

;*****************************************************************************
; TDataEntry
; Purpose:
;	Called during ASCII Load every time a DATA statement
;	or a RESTORE statement is inserted into any text table.
;	It adds info about the statement to a buffer so TDataEnd
;	can move the DATA statements from prs to module at end of load.
;	Always keeps room for a new entry at end of buffer
; Entry:
;	al = DT_Data for DATA stmt, 
;	   = DT_Restore for RESTORE stmt, 
;	   = DT_EndProc for END SUB/FUNCTION stmt
;	bx = otx
;	grs.oPrsCur identifies the text table within the module
; Exit:
;	returns ax=0 (with condition codes set accordingly) if out-of-memory
;
;*****************************************************************************
PUBLIC	TDataEntry
TDataEntry PROC NEAR
DbAssertRelB [txdCur.TXD_scanState],ne,SS_EXECUTE,CP,<TDataEntry: bad scanstate>
DbAssertRelB al,ne,0,CP,<TDataEntry: called with al=0>
	;function assumes we can return al=non-zero if we don't alter ax
	FLoadActive
	je	TdExit			;brif not LOADing a file
	cmp	[fIgnoreTDataEntry],FALSE
	jne	TdExit			;brif TDataEnd is active.  TDataEnd
					; calls TxtInsUpdate, which can call
					; TDataEntry
	cmp	al,DT_Data
	jne	NotData
	test	[txdCur.TXD_flags],FTX_mrs
	jne	TdExit			;brif DATA stmt within main program
NotData:
	push	di			;save caller's di
	mov	di,[bdData.BD_pb]
	DbAssertRel di,ne,NULL,CP,<TDataEntry: no buffer>
	add	di,[bdData.BD_cbLogical]
	sub	di,CB_TDATA
	push	ds
	pop	es			;es = DGROUP
					;NOTE: al must still = TDataEntry parm
	;**********************************
	;NOTE: al still equals entry value 
	;**********************************
	stosb				;store node type
	mov	ax,[grs.GRS_oPrsCur]
	stosw				;store current prs
	xchg	ax,bx			;ax = otx
	stosw

	;make room for next entry
	PUSHI	ax,<DATAOFFSET bdData>
	PUSHI	ax,CB_TDATA
	call	BdGrow
	mov	bx,[bdData.BD_pb]
	add	bx,[bdData.BD_cbLogical]
	mov	BYTE PTR [bx - CB_TDATA],DT_End	;make table look empty
	pop	di			;restore caller's di
TdExit:
	or	ax,ax			;set condition codes for caller
	ret
TDataEntry ENDP

;*****************************************************************************
; TDataEnd
; Purpose:
;	Called at the end of an ASCII Load
;	Step1:	For each RESTORE in bdData, find the label definition.
;		If the label definition is in a prs,
;		  Generate a new unique oNam
;		  Store the new oNam in the restore opcode's operand
;		  Insert DT_Label into bdData with
;		     otx field to the label definition's otx,
;		     oPrs field to the label definition's oPrs
;		else
;		  ignore the entry
;	Step2:  For each DT_Data move the DATA opcode from the prs to
;		the mrs.  For each DT_Label entry in bdData, generate
;		an opBolLab opcode in the mrs.
;
; Exit:
;	returns al=ER_OM if out-of-memory
;	        al=0 if no error
;	        (with condition codes set accordingly)
;
;*****************************************************************************
cProc	TDataEnd,<PUBLIC,NEAR,NODATA>,<si,di>
	localW	lnCur
	localW	oPrsRestore
	localW	oNamNew
cBegin
	cmp	[fIgnoreTDataEntry],0	
	je	@F			; Redundant tDataEnd
J1_TDEndExitOk: 			
	jmp	TDEndExitOk		; Redundant tDataEnd
@@:					

	mov	[lnCur],100d
	mov	[fIgnoreTDataEntry],NOT FALSE
	mov	si,[bdData.BD_pb]
	or	si,si			; is bdData even allocated?
	jz	J1_TDEndExitOK		; brif not, nothing to do
Step1Loop:
	lodsb				;al = node type
	.errnz DT_End
	or	al,al			;test for DT_End
	je	Step1Done		;brif done with step1's pass over list
	cmp	al,DT_Restore
	jne	Step1Next		;brif not a RESTORE node
	lodsw				;ax = oPrs field (may be UNDEFINED)
	mov	[oPrsRestore],ax
	push	ax
	call	PrsActivateCP		;activate opRestore's prs

;Generate a label which has not been used in this module
UniqueLabelLoop:
	inc	[lnCur]
	mov	ax,[lnCur]		;pass line# to ONamOfLn in ax
	call	ONamOfLn		;ax = oNam for line number lnCur
	je	JE1_TdEndOmErr		;brif out-of-memory error
	mov	[oNamNew],ax		;preserve proposed oNam
	test	dl,NM_fLineNumLabel	;dl = flags byte returned by ONamOfLn
	jne	UniqueLabelLoop		;brif name is already used as label

	lodsw				;ax = otx field of opStRestore
	inc	ax			;advance to oNam operand
	inc	ax
	mov	di,ax			;di = otx to opStRestore's oNam
	call	GetWOtx			;ax = label's oNam (from opStRestore)
	call	FindLabelDef		;bx = oPrs, ax = otx of label def
					;current prs is preserved
	inc	ax
	je	Step1Loop		;brif label not found
	inc	bx			;test prs for UNDEFINED
	je	Step1Loop		;brif label defn is in mrs (no need
					; to change RESTORE's operand, we're
					; not moving DATA stmts already in mrs)
	dec	ax			;restore ax = otx
	dec	bx			;restore bx = oPrs where label's defined
	mov	cx,[oNamNew]
	call	InsertLabInTbl		;insert DT_Label entry in bdData
					; si updated if new entry inserted
					; before current si
	inc	ax			;test for UNDEFINED
	je	Step1Loop		;brif label defn is in prs with no DATA
					; stmts (we could handle this case,
					; but it is not worth the code it would
					; take)
	dec	ax			;test for 0
	je	JE1_TdEndOmErr		;brif out-of-memory error

	push	[oPrsRestore]
	call	PrsActivateCP		;activate opRestore's prs

	push	di			;pass otx to PutWOtx
	push	[oNamNew]		;pass new oNam to PutWOtx
	call	PutWOtx			;store new oNam in opStRestore's operand
	SKIP2_PSW			;skip next 2 lodsw instructions
Step1Next:
	lodsw				;skip oPrs field
	lodsw				;skip otx field
	jmp	SHORT Step1Loop

J1_Step2Done:
	jmp	Step2Done

;si points beyond the end of the bdData table
Step1Done:
	call	PrsDeactivate		;make module's text table active
	add	si,CB_TDATA - 1		;si points CB_TDATA bytes beyond
					; DT_End entry
;walk through table bdData backwards
;si points 10 bytes beyond entry to be processed
;di = otx to insert DATA stmts in main module's text table
;
Step2Loop:
	sub	si,CB_TDATA+CB_TDATA	;si points to entry in table bdData
	cmp	si,[bdData.BD_pb]
	jc	J1_Step2Done		;brif done with table
	lodsb				;al = node type
	cbw				;ax = node type (always less than 128)
	xchg	cx,ax			;cx = node type
	lodsw				;ax = oPrs if DT_Data
	xchg	dx,ax			;dx = oPrs if DT_Data
	lodsw				;ax = otx if DT_Data, oNam if DT_Label
	sub	si,[bdData.BD_pb]	;convert pointer to offset in case
					; following code causes heap movement
	dec	cx
	.errnz DT_Data-1
	je	MoveDataStmt		;brif DATA stmt entry
	.errnz DT_EndProc-2
	loop	NotEndProc		;brif not EndProc entry
	xchg	di,ax			;di = otx where following DATA
					; stmts are to be inserted into
					; main program
J1_Step2Next:
	DJMP	jmp SHORT Step2Next

NotEndProc:
	loop	J1_Step2Next		;brif not Label entry
	.errnz DT_Label-3
	;ax = oNam for new label
	push	ax			;save oNam for new label

;  CALL	bdlAlloc(&bdlTxtScrap, 6)	[2]
	PUSHI	ax,<DATAOFFSET bdlTxtScrap>
	PUSHI	ax,6			;number of bytes in a opBolLab opcode
	PUSHBDL_TYPE  pgtypEBGeneric,ax ; pass sb type for EB version
	call	bdlAlloc
	or	ax,ax

	pop	ax			;restore ax = oNam of new label
JE1_TdEndOmErr:
	je	JE2_TdEndOmErr		;brif out-of-memory error
	GETSEG	es,bdlTxtScrap.BDL_seg	;[2]es points to segment of Scrap
	xor	bx,bx
	mov	WORD PTR es:[bx],opBolLab	;store opcode
	mov	es:[bx+4],ax		;store oNam of new label
	jmp	SHORT DoIns

;ax = otx for DATA opcode
MoveDataStmt:
	push	ax			;preserve otxData
	push	dx			;pass oPrs to PrsActivateCP
	call	PrsActivateCP
	pop	ax			;restore ax = otxData

;  CALL TxtCopyScrap(otxData, 2+StartOtx, cb, TRUE)	[2][5]
	push	ax			;pass otxData 
	PUSHI	bx,2+StartOtx		;length of opBol opcode
	inc	ax
	inc	ax			;ax = offset to opStData's cbEos operand
	call	GetWOtx			;ax = cbEos
	add	ax,5			;include opcode, cbEos in count
	and	al,0FEh			;round up to even byte count
	push	ax			;pass cb to TxtExtract
	push	ax			;push TRUE
	call	TxtCopyScrap		;move data from txdCur to bdlTxtScrap
JE2_TdEndOmErr:
	je	TdEndOmErr		;brif out-of-memory error

	GETSEG	es,bdlTxtScrap.BDL_seg	;[2] es points to segment of Scrap
	xor	bx,bx
	mov	WORD PTR es:[bx],opBol
DoIns:
	DbAssertRel di,ne,UNDEFINED,CP,<TDataEnd: missing DT_EndProc>
	call	PrsDeactivate		;make module's text table active
	xchg	si,di			;pass otxInsert in si
	call	TxtInsScrap		;insert pcode in main module @ si
	je	TDEndOmErr		;brif out-of-memory error
	mov	[fDataMoved],1		;so user interface can put up a
					; dialog box that says we've altered
					; his program.
	mov	bx,si			;bx = otxInsert
	add	bx,[bdlTxtScrap.BDL_cbLogical] ;bx=offset past inserted pcode
	call	TxtInsUpdate		;Update static vars affected by insert
	xchg	si,di			;restore si = offset into bdData
					;        di = otxInsert
	je	TDEndOmErr		;brif out-of-memory error
Step2Next:
	add	si,[bdData.BD_pb]	;convert offset to pointer
	jmp	Step2Loop

Step2Done:
	PUSHI	ax,<DATAOFFSET bdData>
	call	BdFree
TDEndExitOk:
	sub	ax,ax			;return 0 (no error)
TDEndExit:
	or	ax,ax			;set condition codes for caller
cEnd	;TDataEnd

TdEndOmErr:
	mov	al,ER_OM
	jmp	SHORT TdEndExit


;*****************************************************************************
; InsertLabInTbl
; Purpose:
;	insert DT_Label entry in bdData
; Entry:
;	ax = otx of label def
;	bx = oPrs
;	cx = oNamLabel (oNam for new label being generated)
; Exit:
;	if new entry is inserted before current si, si is updated
;	if out-of-memory, ax = 0 on exit
;	if no place found to insert entry (because no DATA or RESTORE stmts
;	   occurred in prs with label definition), ax = UNDEFINED
;	else ax = sp (not UNDEFINED or 0)
;
;*****************************************************************************
InsertLabInTbl PROC NEAR
	push	di			;save caller's di, ax

	sub	si,[bdData.BD_pb]	;convert si to offset (in case bdData
					; gets moved during function)
	push	si			;save offset

	push	cx			;save oNamNew
	mov	si,[bdData.BD_pb]	;start search at 1st entry in table
	xchg	di,ax			;di = otx of label definition
ILoop:
	lodsb				;al = node type
	cbw				;ax = node type
	xchg	cx,ax			;cx = node type
	lodsw				;ax = oPrs field
	xchg	dx,ax			;dx = oPrs field
	lodsw				;ax = otx field
	jcxz	IDone			;brif reached DT_End entry
	cmp	dx,bx
	jne	ILoop			;brif not prs of interest
	.errnz DT_End
	loop	INotData		;brif not DT_Data
	.errnz DT_Data - 1
	cmp	ax,di
	jbe	ILoop			;brif otx is too low
	jmp	SHORT IInsert

INotData:
	loop	ILoop			;brif not DT_EndProc
	.errnz DT_EndProc - 2
IInsert:
	;make room for CB_TDATA bytes in bdData buffer before si-CB_TDATA
	; BdShiftRight(bdData, si-CB_TDATA, CB_TDATA)
	
	mov	ax,CB_TDATA
	mov	bx,DATAOFFSET bdData
	sub	si,ax			;si -> start of entry to insert before
	push	bx			;pass &bdData
	sub	si,[bx.BD_pb]		;convert pointer to offset
	push	si			;pass insert offset
	push	ax			;pass cbInsert
	call	BdShiftRight		;make room for new entry
	pop	bx			;restore bx = oNamNew

	or	ax,ax
	je	IExit			;brif out-of-memory
	mov	di,si			;di = destination offset
	add	di,[bdData.BD_pb]	;convert offset to pointer
	push	ds
	pop	es			;es = DGROUP for stosb
	mov	al,DT_Label
	stosb				;store node type field
	xchg	ax,bx			;ax = oNamNew
	stosw				;store oNamNew field
	stosw
	sub	di,[bdData.BD_pb]	;convert pointer to offset
	pop	si			;si = caller's offset into bdNodes
	cmp	si,di
	jb	InsBefore		;brif caller's si not affected by insert
	add	si,CB_TDATA
InsBefore:
	push	si
	mov	ax,sp			;return TRUE (not out-of-memory)

;ax = 0 if out-of-memory error, UNDEFINED if prs not found, otherwise ax=sp
IExit:
	pop	si
	add	si,[bdData.BD_pb]	;convert offset to pointer
	pop	di			;restore caller's di
	ret
InsertLabInTbl ENDP

IDone:
	inc	bx			;is oPrsSearch = UNDEFINED?
	je	IInsert			;brif we're beyond prs of interest
	pop	bx			;pop oNamNew off stack
	mov	ax,UNDEFINED
	jmp	SHORT IExit

;*****************************************************************************
; FindLabelDef
; Purpose:
;	Search all procedures in current module for a label definition
; Entry:
;	ax = oNam for label
; Exit:
;	bx = oPrs
;	ax = otx of label def (UNDEFINED if not found)
;
;*****************************************************************************
FindLabelDef PROC NEAR
	push	di			;preserve caller's di
	xchg	ax,di			;di = oNam to search for
	call	PrsDeactivate		;start in main text table

;di = oNam of label being searched for
FlLoop1:
	GETSEG	es,txdCur.TXD_bdlText_seg   ;[1]
	mov	bx,[txdCur.TXD_otxLabLink]
FlLoop2:
	inc	bx			;test for UNDEFINED
	je	FlDone2			;brif end of linked list of labels
	dec	bx
	cmp	es:[bx+2],di
	je	FlFound			;brif found oNam
	mov	bx,es:[bx]		;bx points to next node in linked list
	jmp	SHORT FlLoop2

FlDone2:
	call	NextTextPrsInMrs
	inc	ax			;test for UNDEFINED
	jne	FlLoop1			;brif got a prs text table
	dec	ax			;return UNDEFINED (not found)
	jmp	SHORT FlExit

FlFound:
	dec	bx
	dec	bx			;bx = otx to op[Bol]Lab[Sp] opcode
	mov	ax,[grs.GRS_oPrsCur]	;ax = oPrs
	xchg	ax,bx			;ax = otx, bx = oPrs
FlExit:
	pop	di			;restore caller's di
	ret
FindLabelDef ENDP

sEnd	CP

end
