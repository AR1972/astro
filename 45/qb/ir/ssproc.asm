page	49,132
	TITLE	ssproc	- Scan support for procedures
;***
;ssproc	- Scan support for procedures
;
;	Copyright <C> 1986, Microsoft Corporation
;
;
;****************************************************************************

	.xlist
	include		version.inc
SSPROC_ASM = ON
	IncludeOnce	context
	IncludeOnce	exint		
	IncludeOnce	extort
	IncludeOnce	opid
	IncludeOnce	opmin
	IncludeOnce	opstmt
	IncludeOnce	optables
	IncludeOnce	names		
	IncludeOnce	pcode
	IncludeOnce	qbimsgs
	IncludeOnce	rtinterp
	IncludeOnce	ssint
	IncludeOnce	txtmgr
	IncludeOnce	variable
	.list


assumes DS, DATA
assumes es, NOTHING
assumes ss, DATA


	extrn	ABSOLUTE:far 		

sBegin	CODE

	extrn	exBranchRel:near	;This executor in exgoto.asm
	extrn	exBranch:near		;This executor in exgoto.asm
	extrn	exNoList1:near		; This executor in exproc.asm
	extrn	exParamCnt:near 	;This executor in exproc.asm
	extrn	exSave87:near		;This executor in exproc.asm
	extrn	exR8ToStack:near	; This executor in exproc.asm

	    extrn   exR4ToStack:near	; This executor in exproc.asm

extrn	exDelLocSD:near
extrn	exDeallocArray:near

extrn	exDelTmpSD:near

;This executor in exrefarg.asm
extrn	exPushSeg:far		;executor to coerce near reference to far ref.


sEnd	CODE


sBegin	DATA


	extrn	b$ULSymSeg:word 	;Zero iff no user library
	extrn	b$TRPTBL:word		;Start of event handler table
	extrn	b$TRPTBLEND:word	;End+1 of event handler table


pbAbsolute	db	'ABSOLUTE'	
CB_Absolute	EQU	$ - pbAbsolute	


SsLastExit	dw	0
SsProcPRS	dw	0
SsDeclSeg	dw	0	
FuncOtyp	db	0

sEnd	DATA


sBegin	SCAN
assumes cs, SCAN

NoParamsFlag=	80H	;No parentheses on declaration:  no type checking
ArrayFlag=	04H	;Current parameter is whole array
DefFnFlag=	40H	;Def Fn:  pass by value
CallSFlag=	20H	;CallS:  pass by far reference
ByValFlag=	ST_ByVal;BYVAL found on current parameter
SegFlag	=	ST_Seg	;SEG found on current parameter
ProcType=	03H	;From PRS_procType
CallFlag=	SegFlag	;Dual use flag - explicit call

TypeMatchFlag	=	FP_ENDPROC	;Set this bit if type mismatch
ResetBits	=	(ByValFlag + SegFlag + ArrayFlag)*100H + TypeMatchFlag
			;Reset for each parameter

.errnz	ByValFlag - HIGH PATR_byVal
.errnz	SegFlag - HIGH PATR_Seg
.errnz	ArrayFlag - HIGH PATR_array

;Verify the SsRefArg flags defined in SSINT.INC
.errnz	Lvalue - ProcType
.errnz	FarArg - (CallSFlag+SegFlag)
.errnz	FScb - ByValFlag

public	ByValMarker,SegMarker		;Put in rule byte

ByValMarker=	ByValFlag
SegMarker=	SegFlag


;executor map to store values in a temp.
tTmpType	label	word
	DWEXT	exStTmp2
	DWEXT	exStTmp4
	DWEXT	exStTmpR4
	DWEXT	exStTmpR8
	DWEXT	exStTmpSD


;*** Ss_LParen
;
; Make sure type of expression on stack is "expression", not
; variable or literal.

SsProc	LParen
	STOSWTX				;Emit no-op executor
	pop	ax			;Get expression type
	and	ax,((ST_ByVal OR ST_Seg) SHL 8) OR ST_Typ_Mask
	push	ax
	jmp	[ScanRet]


;*** Ss_ByVal_Seg
;
; Set flag that indicates Byval or Seg was seen
; Rule table byte has flag bit set

SsProc	ByVal_Seg
	STOSWTX				;Emit it
	shr	bx,1			;Back to byte index
	pop	ax			;Get oTyp and flags
	or	ah,mpOpRule[bx]		;Add ByVal or Seg flag
	push	ax			;Put it back
	jmp	[ScanRet]


;*** Ss_NoList0 - Current PC oText
;
; This opcode indicates location of current PC.  It was swapped in place
; of original opcode just before scanning.

SsProc	NoList0
	mov	[grs.GRS_otxCont],di	;Set new CONT otx
	dec	si
	dec	si			;Point back to opcode
	mov	ax,[SsErrOpcode]	;Get original opcode
	mov	PTRTX[si],ax		;Put it back so we can scan it now
	jmp	[ScanRet]


;*** Ss_NoList1 - Pcode reference to update
;
; This opcode identifies a spot in the pcode that is referenced by a static
; location in DS.  Its operand is the offset in DS where the oTx is stored.
; This location is updated to contain the current emit oTx if it is possible
; to continue.  Otherwise, this opcode is deleted.
;
; This opcode is used for return addresses on the stack, event table
; entries, and pcode references in the MRS.
;
; In the case of DefFn/Function return addresses, there will be a scan
; stack frame for the return value.  The oTx of this entry must be updated
; to point AFTER this return address opcode, so that any coercions, etc.,
; will be performed AFTER the DefFn/Function returns.
;
; The opNoList1 for error and event handlers is inserted after the opBol
; so that the text manager won't get confused.  A check is made for these
; handlers so they can be updated to the oTx of the saved opBos.  The opcode
; is deleted so that it won't be updated if a coercion takes place later on
; the line.

SsProc	NoList1
	    cmp     [grs.GRS_otxCONT],-1
	jz	EatNoList		;If can't continue, delete pcode
	STOSWTX
	LODSWTX				;Get operand, offset into stack
	STOSWTX
	xchg	bx,ax

	;See if we're dealing with an error or event handler

	test	byte ptr es:[si-3],HIGH (OPCODE_MASK+1)
	jnz	UpdateBOS		; Brif error/event handler

	;   The processing of a opNoList1 results in an exNoList1 being
	;emitted.  This is necessary in case text is inserted before the
	;current location.  opBos processing in ssbos.asm will scan the
	;statement for exNoList1s and update the return addresses with
	;the correct oTx.  The update is still performed here because
	;the Bos search and update will not occur unless there is an
	;insertion.

	mov	[bx],di			;Set oTx of return address to here
	or	[SsBosFlags],SSBOSF_PcUpdate	;Remember that update occured
	pop	ax
	or	ax,ax			;Scan stack entry present?
	jz	PushAx

	;Analyze stack entry to see if oTx needs updating

	pop	bx			;Get oTx
	lea	dx,[bx+4]		; Size of this opcode
	cmp	di,dx			;Did it point just in front of us?
	jnz	RestoreEntry
	mov	bx,di			;Use current emit oTx for entry
RestoreEntry:
	push	bx			;Restore oTx
PushAx:
	push	ax			;Restore oTyp
NoListRet:
	jmp	[ScanRet]

EatNoList:
	inc	si
	inc	si			;Skip over operand
	jmp	NoListRet

UpdateBOS:
	mov	ax,[SsOtxBos]
	mov	[bx],ax
	sub	di,4			;Eat opNoList1 for error/event handler
	jmp	NoListRet

subttl	StDeclare,StSub,StFunction,StDefFn - Scan procedure headers
page

;***
;Ss_StDeclare - Scan the DECLARE statement
;
;Make sure this is the same as the official declaration
;
;***********************************************************************

SsProc	StDeclare,rude
	STOSWTX
	test	[SsExecFlag],OPA_fExecute ;Already seen executable stmt?
	jz	@F
	mov	ax,MSG_COM		;DECLARE must precede executable stmts
	call	SsError
@@:
	mov	ax,PTRTX[si+2]		;Get oPRS
	push	di			;Save current position
	call	ReLinkScan		;Copy operands, adjust PRS if def.
	mov	es,cx			;Segment of PRS to es
	pop	ax
	push	si
	push	di			;Save source and emit oTx
	xchg	ax,di			;Get oTx of declare to di
	add	di,DCL_cParms		;Point to parameter count field
	xor	dh,dh			;Not a DefFn
	call	GetDecl			;Get declaration
	assumes ds,NOTHING

	;ds:si points to declaration of SUB/FUNCTION/DEF FN
	;ax = oRS of declaration

	xchg	ax,bx			;oRS to bx
	lodsw				;Get attributes
	mov	dx,ax			;Preserve length of alias
	xor	ax,es:[di-2]		;Compare attributes
	and	ax,DCLA_cdecl+DCLA_procType+DCLA_oTyp+DCLA_cbAlias
					;Make sure proc type, alias length,
					;CDECL, and fcn return type match
	jnz	DeclareDD
	lodsw				;Get count of parameters
	scasw				;Same as this declare?
	xchg	cx,ax			;Count to cx
	mov	ax,ER_AC
	jnz	DeclareX		;Argument count error?
	inc	cx
	jz	CompAlias		;No parameter list?
	dec	cx
	jz	CompAlias		;No arguments
CompareArgs:
	cmpsw				;Skip over oVar
	lodsw				;Get ParamAtr
.errnz	LOW PATR_byVal			
	mov	dl,ah			; Save ByVal flag in dl
	xor	ax,PTRTX[di]		;Compare ParamAtr
	TestX	ax,PATR_Array+PATR_Seg+PATR_ByVal+PATR_oTyp ;Only these count
	jnz	DeclareTM
	mov	ax,PTRTX[di+2]		; Get declared oType

	; Make sure ByVal is only on numeric types

	xchg	cx,ax			; Declared oType to cx, loop cnt to ax
	test	dl,HIGH PATR_ByVal	; ByVal?
	jz	CompType		; Not ByVal - go compare oTyps
	jcxz	DeclareTM		; Don't allow As Any with ByVal
	cmp	cx,ET_MaxNum		;[2] Record or SD type?
	ja	DeclareTM		; Don't allow non-numeric with ByVal
CompType:				
	push	dx
	push	ax			;Save loop count
	push	bx
	push	ds
	lodsw				;Get official oType
	xchg	ax,dx			;oType to dx

	;NOTE: Zero flag still set here if not ByVal!

	push	ss
	pop	ds
assumes	ds,DATA
	jcxz	AsAny			;Always allow ANY to pass--ZF must be set
	mov	ax,[grs.GRS_oRsCur]
	cCall	CompareTyps,<ax,bx,cx,dx>	
	REFRESH_ES				
	or	ax,ax				
AsAny:
	pop	ds
assumes	ds,NOTHING
	pop	bx
	pop	cx
NextArg:
	pop	dx
	jnz	DeclareTM
	add	di,4
	loop	CompareArgs
CompAlias:
;Make sure aliases match
.errnz	DCLA_cbAlias - 07C00H
	mov	cl,dh			;cbAlias to cx
	and	cl,HIGH DCLA_cbAlias
	shr	cl,1
	shr	cl,1
rep	cmpsb				;Compare alias strings (ZF set if none)

DeclareDD:
	mov	ax,ER_DD		;Duplicate def. if aliases don't match
DeclareX:
;Zero flag set if no error, else error code in ax
	pop	di
	pop	si
	push	ss
	pop	ds
assumes	ds,DATA
	jz	NoDeclErr
	call	SsError
NoDeclErr:
	jmp	[ScanRet]

DeclareTM:
;To accurately position error cursor, figure out exact position in pcode
;of error.  di = emit oTx of error.
	mov	dx,di			;Error location to dx
	pop	di
	pop	si
	sub	dx,di			;Distance back to error
	inc	dx			;Set LSB
	add	si,dx			;Position of error in source
	mov	ax,MSG_ParmTM		;Parameter type mismatch
	push	ss
	pop	ds			;Restore ds
	call	SsError
	sub	si,dx			;Restore source oTx
	jmp	short NoDeclErr

;***
;SsReLinkDecl - adjust PRS to point to proc declaration after scan/descan
;
;Purpose:
;	If this is the official definition of the procedure, i.e. it is
;	referred to by PRS_oRsDef and PRS_otxDef, then adjust PRS_otxDef
;	to refer to new (emit side) location.  Set flag bit if this
;	is done.
;
;	Also copies all operands.
;
;Inputs:
;	ax = oPRS
;	bx = opcode * 2
;	si & di = oTx of cbEOS (source & emit, respectively)
;	dh = 0 if scanning, dh = -1 if descanning
;Outputs:
;	cx:bx = pPRS
;	si & di = oTx of next pcode
;Preserves:
;	dl
;***********************************************************************
	public	SsReLinkDecl,SsReLinkNoCopy
ReLinkScan:
	xor	dh,dh
SsReLinkDecl:
	PUSH_ES 			
	push	di			;Save oTx of declar. (emit side)
	push	si
	push	ax			;Save oPRS
	call	CopyOperands
	pop	ax
NoCopy:
	call	PPrsOPrsSCAN		;[22] oPRS in ax --> pPRS in es:bx
	pop	ax			;oTx+2 of declaration (source side)
	pop	cx
	xor	dh,BPTRRS[bx].PRS_flags ; Get flags, adjust for scan vs. descan
	test	dh,FP_DEFSCANNED	;Already scanned/descanned definition?
	jnz	ReLinked		;No work if already in correct state
	dec	ax
	dec	ax
	sub	ax,[SsCbTxExpand]	;Compute original oTx
	cmp	ax,PTRRS[bx].PRS_otxDef ; Is that where defined?
	jnz	ReLinked
	mov	ax,PTRRS[bx].PRS_oRsDef 
	cmp	ax,grs.GRS_oRsCur	;Defined in this RS?
	jnz	ReLinked
	xor	BPTRRS[bx].PRS_flags,FP_DEFSCANNED ; Change scan state
	dec	cx
	dec	cx
	mov	PTRRS[bx].PRS_otxDef,cx ; Adjust oTx to emit location
ReLinked:
	mov	cx,es			;Save segment of PRS
	POP_ES				
	ret

SsReLinkNoCopy:
;Same as SsReLinkDecl except does not copy operands
;si & di unchanged
	PUSH_ES 			
	push	di
	push	si
	jmp	NoCopy

subttl	StData,StDefType,StType,StEndType,StDefFn,StEndDef
page
;***
;StData,StDefType,StType,StEndType,StDefFn,StEndDef
;
;	These scan routines manage linked lists across the
;	source/emit boundary.
;
;Algorithm:
;	TXLNK is a data structure with a tail pointer for each list.
;	When another item to be linked is encountered, we simply
;	find the previous one with TXLNK and point it to the new one.
;	TXLNK is updated to point to our new one, too.  TXLNK = 0
;	means there is no previous element.
;
;	These routines are generally used for both scan and descan.
;	The exception is StDefFn/StEndDef, which is descan only.  During
;	scan to execute state a stack entry is used to link Def to End.
;	End is not linked in execute state.
;
;	Reasons why these guys are linked in Parse state:
;
;	DEF FN/END DEF - Used to keep track of what is within the definition.
;
;	DATA - NOT linked by parser in Parse state.
;
;	DEFtyp - Used to figure out what type something is, given a pcode
;		location.
;
;	TYPE/END TYPE - Used to keep track of what is within the type definition
;
;
;	Reasons why these guys are linked in Execute state:
;
;	DEF FN/END DEF - Need to know what is within a definition, to 
;		prevent GOTO, etc. to/from DefFn during scan.
;
;	DATA - to find the rest of the data.
;
;	DEFtyp - Used to assign types to direct mode things, using DEFtyp
;		status of the current PC or the last statement.
;
;	TYPE/END TYPE - Used to jump over the type definition.  End is linked
;		to next Type, but not needed.


SsDProc	StDefFn
	STOSWTX
	mov	dh,-1			;Set Descan direction
LinkDefFn:
	mov	ax,PTRTX[si+4]		;Get oPRS
	call	SsReLinkNoCopy		;Adjust PRS_otxDef
	jmp	short LinkDef

SsDProc	EndSingleDef
SsDProc	StEndDef
	STOSWTX
LinkEndDef:
	mov	PTRTX[si],2		;Set filler to cbEOS
LinkDef:
	mov	bx,TXLNK_DefFn
	mov	dx,dataOFFSET mrsCur.MRS_otxDefFnLink
	cmp	PTRTX[si+2],-1		;End of list?
	jz	VarLenLink		;If so, don't change it
	inc	PTRTX[si+2]		;Set LSB of link to indicate end
	jmp	short VarLenLink

ssProc	StData,,Local
SsD_StData:
	mov	bx,TXLNK_Data		;Link field for DATA link list
	mov	dx,dataOFFSET mrsCur.MRS_data_otxFirst
	STOSWTX 			;Emit executor
	mov	PTRTX[si+2],UNDEFINED	;In case this is last, mark end
VarLenLink:
	LODSWTX 			;Load cbEos
	mov	cx,ax
	inc	cx
	shr	cx,1			;Words to EOS including link field
	jmp	short EmitAndAdjLinks	; Emit cbEos and link


SsProc	StDefType,rude,Local
SsD_StDefType:
	mov	cx,3			;Link field plus 1 Dword operand
	mov	bx,TXLNK_DefType
	mov	dx,dataOFFSET txdCur.TXD_otxDefTypeLink
	jmp	short EmitAndAdjLinks	;Emit executor and link


SsProc	StType,rude,Local
	or	[SsFlags],SSF_InType	;Remember we're in TYPE declarationt
	test	byte ptr [grs.GRS_oRsCur+1],80H	;In procedure?
	jz	SsD_StType		;Better not be
	push	ax			;Save executor
	mov	ax,MSG_InvProc		;Illegal in procedure
	call	SsError
	pop	ax
SsD_StType:
	mov	cx,2			;Link field plus 1 word operand
	jmp	short FixType

SsProc	StEndType,rude,Local
	and	[SsFlags],not SSF_InType;No longer within TYPE declaration
SsD_StEndType:
	mov	cx,1			;Link field w/no additional operands
FixType:
	mov	bx,TXLNK_Type
	mov	dx,dataOFFSET txdCur.TXD_otxTypeLink

EmitAndAdjLinks:

	;AX = Word to be emitted
	;BX = offset into LinkCtl
	;CX = remaining cw of operands to copy
	;DX = pointer to head of list

	STOSWTX
	add	bx,[ssLinkCtl]		;Get pointer to link control struc
	mov	ax,di
	xchg	ax,[bx] 		;Get last item, set new "last"
	or	ax,ax			;First item?
	jz	SetHead
	xchg	bx,ax			;Pointer to previous item in bx
	mov	PTRTX[bx],di		;Fix up pointer to current value
CopyOps:
public	CopyOps
	cli				;Double prefix! No interrupts!
rep	movs	PTRTX[si],PTRTX[di]	;Copy remaining operands
	sti
	jmp	[ScanRet]

SetHead:
	mov	bx,dx
	mov	[bx],di			;Set head pointer
	jmp	CopyOps

;***
;Ss_StSub,Ss_StFunction,SsStDefFn - Scan SUB, FUNCTION, and DEF FN statements
;
;Purpose:
;	Two functions are performed:
;
;	1.  Look up executor and copy operands unchanged
;
;	2.  Assign oBP to the parameters.
;
;	The stack looks like this at execution time:
;	<arg 1>
;	<arg 2>
;	. . .
;	<arg n>
;	<oRS of return address>
;	<oText of return address>
;	<old BP>
;
;	At this point, the MOV BP,SP is done.  <arg n> is at offset
;	FR_MinFrame from BP.  Arguments are passed by reference, with
;	the size of the pointer determined by memory model (SizeD).
;	Offsets are assigned in the variable table in a loop starting 
;	with <arg n>.  Since Def Fn arguments are passed by value, 
;	the references are always to temporaries already allocated
;	in the stack.
;	
;******************************************************************


SsProc	StDefFn,rude,local
	STOSWTX

	;Make entry on scan stack

	pop	ax
	push	ax
	or	ax,ax			;Scan frame already on stack?
	jz	DefFnFrame
	mov	ax,MSG_DefFnCtrl
	call	SsError
DefFnFrame:
	push	di
	PushI	ax,STYP_DefFn

	push	PTRTX[si+8]		;Save count of parameters
	mov	[SsBosStack],sp 	; Reset BOS SP mark for 1 Line Fn
	push	PTRTX[si+4]		;Put oPRS on stack
	mov	ax,SCANOFFSET ContDefFn
	xchg	ax,[ScanRet]
	push	ax
	xor	dh,dh
	jmp	LinkDefFn		;"call" scan routine with ret. addr.
					;  in [ScanRet]

ContDefFn:
	pop	[ScanRet]		;Restore original [ScanRet]
	mov	[SsOTxStart],di 	; Reset DOS oTx mark for 1 Line Fn
	call	PrsActivate		; oPRS pushed earlier
	REFRESH_ES			
	jmp	short FuncDef


SsProc	StFunction,rude
	STOSWTX				;Emit executor
	push	PTRTX[si+6]		;Save count of parameters
	mov	ax,PTRTX[si+2]		;Get oPRS
	call	ReLinkScan		;Re-link decl. and copy operands
FuncDef:
	cmp	prsCur.PRS_cbFrameVars,-FR_FirstVar	
					; Any frame space already allocated?
	jnz	SubFuncDef
	mov	al,prsCur.PRS_oType	;Get oTyp of return value
	and	al,M_PT_OTYPE		; mask out possible flag bits
	DbAssertRelB	al,b,ET_FS,SCAN,<Ss_StFunction: oTyp is invalid>
	cbw
	call	CbTypOTypSCAN		; Get size of this type
	add	prsCur.PRS_cbFrameVars,ax ; Allocate space for return value
	jmp	short SubFuncDef


SsProc	StSub,rude
	STOSWTX				;Emit executor
	push	PTRTX[si+6]		;Save count of parameters
	mov	ax,PTRTX[si+2]		;Get oPRS
	call	ReLinkScan		;Re-link decl. and copy operands
SubFuncDef:
	mov	[SsLastExit],0
	pop	cx			;Count of parameters
	mov	ax,cx
	jcxz	NoParams
	push	di			;Save oTx
	mov	ax,FR_MinFrame		;First oBP
AssignBP:
	sub	di,6			;Back up to next oVar
		mov	bx,[MrsCur.MRS_bdVar.BD_pb]
	    add     bx,PTRTX[di]
	.errnz	AFORMAL_oFrame
	mov	[bx].VAR_value,ax	;Assign oBP
	    inc     ax
	    inc     ax			;Offsets need two bytes
	.errnz	LOW FV_STATICSET	;Assure byte is ok
	or	byte ptr [bx].VAR_flags+1,HIGH FV_STATICSET ; Flag as dynamic array
	loop	AssignBP
	pop	di			;Recover oTx
	sub 	ax,FR_MinFrame		;cb of parameters
	shr	ax,1			;Word count of parameters
NoParams:
	mov	prsCur.PRS_cwParams,al
	jmp	[ScanRet]

SsProc	StExitProc
	STOSWTX
	inc	si
	inc	si			;Skip over operand
	mov	ax,di
	xchg	ax,[SsLastExit]
	STOSWTX				;Link this with last EXIT
	test	byte ptr [grs.GRS_oRsCur+1],80H	;In a procedure?
	jnz	ExitX
	mov	ax,MSG_InvMain		;Illegal outside procedure
	call	SsError
ExitX:
	jmp	[ScanRet]

SsProc	StEndProc
	call	LinkExit		;Point all EXIT statements to here

	;ife SizeD Insert pcode to release local strings and arrays
	;if  SizeD and FV_FORMS Insert pcode to release Forms and Menus

	push	ax			;Save executor
	call	far ptr RelLocalVars	
	GETSEGTXTCUR			
	pop	ax			;Restore END executor

	STOSWTX
	jmp	[ScanRet]


SsProc	EndSingleDef,rude,local
	STOSWTX
	mov	al,[prsCur.PRS_oType]	;Get result type
	and	ax,M_PT_OTYPE		; mask out possible flag bits
	call	EnsureArgType
;Update count of temps needed
	xor	ax,ax
	xchg	ax,[SsCbFrameTemp]	;Get count of temps needed
	mov	[prsCur.PRS_cbFrameTemp],ax	;Set temp count
	jmp	short EndDef

SsProc	StEndDef,rude,local
	call	LinkExit
	STOSWTX				;Emit executor
EndDef:
	pop	ax			;Get stack entry
	cmp	ax,STYP_DefFn		;Is it our DefFn?
	jnz	ExtraEndDef
	pop	ax			;Clean oTx off stack
EndDefX:
	call	PrsDeActivateFar	
	REFRESH_ES			
	jmp	LinkEndDef

ExtraEndDef:
	test	byte ptr [grs.GRS_oRsCur+1],80H	;In procedure?
	jnz	GetFrame		;If so, go analyze frame
	push	ax			;Restore frame
	mov	ax,MSG_EndNoDef
	call	SsError
	jmp	EndDefX

GetFrame:
	call	SsFrameType
	jmp	EndDefX


LinkExit:
;Set operand of all EXIT statements to point to current emit oTx
	mov	cx,[SsLastExit] 	; Head of list of EXITs
LinkLoop:
	jcxz	LinkX			; Brif end of list
	mov	bx,cx			;BX = Link to next
	mov	cx,di			;CX = Current oTx
	xchg	PTRTX[bx],cx		;Set oTx operand, get link to next
	jmp	LinkLoop

LinkX:
	ret


subttl	Ss_StCall
page
;***
;Ss_StCall - Scan CALL statement
;
;Purpose:
;	1.  Look up executor, copy operands unchanged.
;
;	2.  Using oPRS of target, get oRS of declaration,
;	    then actual far address of declaration.
;
;	3.  Check count of arguments, then compare types of actual
;	    arguments with declared parameters.
;
;	4.  Adapt form of each parameter as required: near reference,
;	    far reference, or value.  Interpreted SUBs and FUNCTIONs
;	    always pass by near reference, DEF FN's by value.
;
;
;******************************************************************


	public	SsCallFunc
SsCallFunc:
extrn	IdLdtoFuncMap:abs
;Enter here from IdLd and AIdLd with FVFUN set
;
;	ds:bx = pVar
;	cx = count of arguments (0 means from IdLd, non-zero means AIdLd)
;	dx = base of executor map for IdLd or AIdLd

	push	cx
	call	SsIndexType		;Index into executor map based on oTyp
	pop	cx
	add	dx,IdLdtoFuncMap
	mov	al,[bx].VAR_flags
	and	al,FV_TYP_MASK
	DbAssertFlags	nz,SCAN,<SsProc: function RetVal oTyp = 0>
	mov	[FuncOtyp],al
	mov	ax,[bx].VAR_Value	;fetch oPrs
	push	ax			;Save oPRS
	push	es			
	call	PPrsOPrsSCAN		;[22] ax = oPrs, es:bx = pPrs
	cmp	BPTRRS[bx.PRS_procType],PT_DEFFN 
	pop	es			
	mov	bx,dx
	mov	ax,cs:[bx]		;Get executor
	STOSWTX				;Emit
	MOVSWTX				;Copy one operand
	pop	ax			;Recover oPRS
	mov	dx,PT_FUNCTION*100H + PT_FUNCTION
	jnz	CallFunc		;brif not a DEF FN
	mov	dx,(DefFnFlag+PT_DEFFN)*100H+PT_DEFFN
;Make sure DefFn is not calling itself recursively
	cmp	ax,[grs.GRS_oPrsCur]	;Same as the one we're in?
	jnz	CallFunc		;If not, then not recursive
	inc	[SsDelayCnt]		;First delayed error on line?
	jnz	CallFunc		;If not, don't update its oTx
	mov	[SsDelayLoc],si		;Save source oTx of error
	mov	[SsDelayErr],ER_UF	;Undefined function
CallFunc:
	jcxz	ParamCheck		;If no arguments, only one operand
	MOVSWTX				;Copy 2nd operand of AIdLd
	jmp	short ParamCheck


ssProc	StCallS
	mov	dh,CallSFlag+PT_SUB
	jmp	short StCall


SsProc	StCall
	mov	dh,CallFlag+PT_SUB
	jmp	short StCall

ssProc	StCallLess
	mov	dh,PT_SUB
StCall:
	mov	dl,PT_SUB
	STOSWTX 			;Emit it

	LODSWTX				;Get operand count
	STOSWTX
	mov	[FuncOtyp],ah		;Set to zero - no RetVal
	xchg	cx,ax			;Save count in cx
	LODSWTX				;Get oPRS
	STOSWTX
ParamCheck:

	;Start of [39]

	;   During an Edit and Continue operation, the pcode may contain
	;opNoList1 opcodes to point to return addresses on the stack
	;that must be updated with the execute state pcode addresses of
	;the current location.	If the current "Call" opcode requires
	;executors to discard temporaries or copy array elements back
	;to far memory, the return address must point immediately after
	;the call and before the inserted executors.  Normal scanning
	;would not process the opNoList1s until the "Call" and it's
	;parameters are finished with.	This results in the stack
	;being updated with the wrong return address.  To solve this
	;problem, the scanner looks ahead to see if the following
	;opcode is opNoList1 and if so, processes it immediately.  Note,
	;there may be more than one occurance for recursive procedures.
	;After this is complete, the parameters are coerced and any
	;necessary insertions are performed.
	;   The processing of a opNoList1 results in an exNoList1 being
	;emitted.  This is necessary in case text is inserted before the
	;"Call" executor.  opBos processing in ssbos.asm will scan the
	;statement for exNoList1s and update the return addresses with
	;the correct oTx.  The update is still performed here because
	;the Bos search and update will not occur unless there is an
	;insertion.

	cmp     [grs.GRS_otxCONT],UNDEFINED	
	jz	IgnoreOpList1		; If can't continue, ignore it

	push	ax			;Save oPrs
@@:
	LODSWTX 			;Look ahead at next opcode
	cmp	ax,opNoList1		;Is this a PC update?
	jnz	@F

	mov	ax,codeOFFSET exNoList1
	STOSWTX 			;Emit executor
	LODSWTX				;Get operand, offset into stack
	STOSWTX
	xchg	bx,ax
	mov	[bx],di			;Set oTx of return address to here
	or	[SsBosFlags],SSBOSF_PcUpdate	;Remember that update occured
	jmp	@B			;Look for another PC update

@@:
	dec	si			;Backup before next opcode
	dec	si
	pop	ax			;Restore oPrs

IgnoreOpList1:				

	;End of [39]

	mov	[SsOtxHeapMove],di	;Procedures can cause heap movement
	mov	[SsParmCnt],cx
	mov	[SsProcPRS],ax		;Save oPRS
	call	PPrsOPrsSCAN		;[22] oPRS in ax --> pPRS in es:bx
					; if FV_SBSWAP, sets up sbRsScan
	cmp	dl,BPTRRS[bx].PRS_ProcType ; Use consistent with PRS?
	jz	PrsOK
	push	ax			
	mov	ax,ER_DD		;Duplicate definition
	call	SsError
	pop	ax			
PrsOK:
	mov	dl,BPTRRS[bx].PRS_flags 
	mov	cl,dl

;***** Start revision [36]
;***** End revision [36]

	push	bx			;Save pPRS
	push	es			; save seg of prs
	GETSEGTXTCUR			
	and	dl,not FP_CDECL		;Reset CDECL for now
	mov	PTRTX[di],si		;Save si in the emitted text
	xor	ax,ax			;Indicate no alias if no decl.
	mov	[SsCbParmCur],ax
	test	cl,FP_DEFINED+FP_DECLARED  ;Is there a declaration?
	mov	cx,-1			;Indicate no declared params
	pop	es			; seg of prs
	jz	NoDecl

	push	es			; save seg of prs
	call	GetDecl 		; returns with ds = seg of declare
	assumes DS,nothing		
	pop	es			; seg of prs
	pop	bx
	push	ax			;Save oRS of declaration

	;ds:si points to delcaration of SUB/FUNCTION/DEF FN
	;es:bx = pPRS of the procedure		
	;dx = flags

	lodsw				;Get oTypFn
	or	ah,ah			;CDECL bit set?
.errnz	DCLA_cdecl - 8000H
	jns	NoCDECLbit
	or	dl,FP_CDECL
NoCDECLbit:
	push	ax			;Save oTypFn
	lodsw				;Get count of parameters
	mov	[SsDeclSeg],ds		; preserve segment of declaration
	push	ss
	pop	ds			;Restore ds = ss
	assumes DS,DATA 		
	mov	cx,ax
	inc	ax			;UNDEFINED same as zero params
	jz	HavAlias
	dec	ax
	shl	ax,1			;ax = cnt*2
	add	ax,cx			;ax = cnt*3
	shl	ax,1			;ax = cnt*6
	add	si,ax			;si points to alias, if any
HavAlias:
	pop	ax			;oTypFn and attributes
NoDecl:
;oRS of decl. on stack (pPRS of procedure if no decl.)
	mov	BPTRRS[bx].PRS_flags,dl ; Update CDECL bit
	push	cx			;Declared count of params
	push	dx			;Flags

	push	bx			; parm to Ss_UL_Support: oPrs
	push	ax			; parm to Ss_UL_Support: cbAlias
	push	dx			; parm to Ss_UL_Support: flags
	call	far ptr Ss_UL_Support	; this chunk is in CP to support
					;	some calls that must be made
					;	from CP
	inc	dx			; error occurred?
	jnz	@F			;	brif not

	GETSEGTXTCUR			;Restore es
	xchg	si,PTRTX[di]		;Restore source pointer
	call	SsError 		; ax contains error code
	xchg	si,PTRTX[di]		;Resave source pointer
@@:
	dec	dx			
	mov	cx,dx			;Save segment of UL proc.
	push	ax			;Save offset
	mov	ax,[SsProcPRS]		;PRS may have moved--get pointer again
	call	PPrsOPrsSCAN		;[22] oPRS in ax --> pPRS in es:bx
	pop	ax
NoUL:
	push	es			; save seg of prs
	GETSEGTXTCUR			;Restore es
	xchg	si,PTRTX[di]		;Restore source pointer
	pop	es			; seg of prs
	pop	dx			;Get flags
	test	dl,FP_DEFINED		;Already defined in interpreter?
	    jnz     InterpProc		
	test	dl,FP_DECLARED		;Was procedure declared?
	    jnz     OKtoUse
	    test    dh,CallFlag + CallSFlag ;Explicit CALL of undeclared proc?
	    jnz     OKtoUse
	    mov     ax,ER_SN		    ;Syntax error if non-existant
	    jcxz    ULError
DeclError:
	mov	ax,ER_US		;Undefined subprogram
ULError:
	call	SsError
	jmp	short CheckParams


sEnd	SCAN
sBegin	CP
assumes cs,CP


DbPub	Ss_UL_Support			
cProc	Ss_UL_Support,<FAR>		
	parmW	oPrs			
	parmW	cbAlias 		
	parmW	flags			
cBegin					
	call	RtPushHandler		;Blasts cx
	mov	ax,cpOFFSET MakeSDFail	
	call	RtSetTrap		;Set trap, errSP at this level
	mov	cx,[cbAlias]		
	mov	cl,ch			
	and	cx,HIGH DCLA_cbAlias	;Mask to cbAlias
.errnz	DCLA_cbAlias - 7C00H
	shr	cl,1			
	shr	cl,1			
;the next four instructions don't alter the flags
	mov	dx,si			;Save pointer to alias
	pushf				;Remember if we found an alias
	GETRS_SEG es			
	mov	bx,[oPrs]		
	mov	ax,PTRRS[bx].PRS_ogNam	;[3] assume no alias

	mov	es,[SsDeclSeg]		; restore es as seg of declaration
	jnz	UseAlias		;Have alias, so copy to SD
;No alias, copy proc. name to SD
	cCall	FpNamOfOgNam,<ax>	; es:dx points to name, cx is cbName
UseAlias:
	; for LQB, all we have to do is compare against "ABSOLUTE"
	pop	ax			; discard flags on stack
	push	si			; save register
	mov	bx,offset DGROUP:pbAbsolute	; DS:BX = "ABSOLUTE"
	mov	si,dx			; ES:SI = proc name

	xor	dx,dx			; assume failure (DX:AX = 0)
	xor	ax,ax			
	cmp	cx,CB_Absolute		; length must be right
	jnz	NotAbsolute		; brif not -- exit

ChkForAbsolute:				
	lods	byte ptr es:[si]	; AL = proc name char
	cmp	al,[bx]			; does it match char of "ABSOLUTE"
	jz	NextChar		; brif so -- do next char
	and	al,0dfh			; make upper case
	cmp	al,[bx]			; match now?
	jnz	NotAbsolute		; brif not -- exit
NextChar:				
	inc	bx			; advance to next char
	loop	ChkForAbsolute		; compare next char

	mov	dx,SEG ABSOLUTE		; return the address of ABSOLUTE
	mov	ax,OFFSET ABSOLUTE	
NotAbsolute:				
	pop	si			; restore reg
NoName:
	call	RtPopHandler		;preserves ax,dx
cEnd					

MakeSdFail:
;Error handler should B$LDFS or B$ULGetProc fail
;Error code in ax
	mov	dx,UNDEFINED		
	jmp	short NoName


; emit code to release local strings and arrays - - - part of proc exit
; must be in CP for calls to FirstVar, NextVar

cProc	RelLocalVars,<FAR>		
cBegin
	call	FirstVar		;Get a variable in this proc
Deallocate:
	or	ax,ax
	jz	DoneDealloc
	mov	cx,PTRVAR[bx].VAR_flags ;[6]
FRAME=	    FVCOMMON+FVSTATIC+FVSHARED+FVFORMAL+FVFUN+FVVALUESTORED+FVREDIRECT
	    TestX   cx,FRAME		;Is it a local variable?
	jnz	GetNextVar

	    TestX   cx,FVARRAY		;Is it an array?
	    mov     ax,codeOFFSET exDeallocArray
	    jnz     @F
	    and     cx,FV_TYP_MASK	;Mask to oTyp
	    .errnz  HIGH FV_TYP_MASK	; Assure we can use CL
	    cmp     cl,ET_SD		;Is it a string?
	    jne     GetNextVar
	    mov     ax,codeOFFSET exDelLocSD
@@:
	    .errnz  AFRAME_oFrame
	mov	cx,PTRVAR[bx].VAR_value ;Get oBP of local
	jcxz	GetNextVar		;If oBP is zero, phantom variable
	call	far ptr Insert1Op_Far	; call to SCAN to call Insert1Op
GetNextVar:
	call	NextVar			;ax = oVar, bx = pVar
	jmp	Deallocate

DoneDealloc:
cEnd					

sEnd	CP
sBegin	SCAN
assumes cs,SCAN

cProc	Insert1Op_Far,<FAR>		
	;NOTE: oTx of insertion is in di
cBegin					
	GETSEGTXTCUR			
	mov	bx,di			; Insert right here
	call	Insert1Op
cEnd					


	;ES:BX = pPRS of procedure

InterpProc:
	    jcxz    CheckParams
	    mov     ax,MSG_DupLibPrs	;UL and interp proc. with same name
	    jmp     short ULError

OKtoUse:
	    jcxz    DeclError		    ;Didn't find proc in UL
	    mov     PTRRS[bx].PRS_txd.TXD_oCompiled,ax	    
	    mov     PTRRS[bx].PRS_txd.TXD_segCompiled,cx    

CheckParams:
	GETSEGTXTCUR			
	pop	ax			;Declared count of parameters
	mov	cx,[SsParmCnt]
	inc	ax			;Have parameter list?
	jz	NoParamList
	dec	ax
	cmp	ax,cx			;Same as actual count?
	jz	ParamCountOK
	inc	[SsDelayCnt]		;First ArgCntErr on line?
	jnz	NoParamList		;If not, don't update its oTx
	mov	[SsDelayLoc],si		;Save source oTx of error
	mov	[SsDelayErr],ER_AC	;It's an Arg Cnt error
NoParamList:
	or	dh,NoParamsFlag
ParamCountOK:
	pop	ax			;oRS to ax
	jcxz	SsParamsOK
NextParam:
;ax = oRS of declaration
;cx = count of remaining arguments
;dx has flags
;es:[di-2] = oTx of declaration, if any
	xchg	bx,ax			;oRS to bx
	and	dx,not ResetBits	;Reset ByVal, Seg, Array (and CallFlag)
	mov	ax,[SsCbParmCur]
	inc	ax
	inc	ax
	mov	[SsCbParmCur],ax	;Assume near reference - 2-byte param

	;Pop stack and check for BYVAL or SEG flag

	pop	ax			;Get oTyp with BYVAL/SEG flag
	test	dl,FP_DEFINED		; Interpreted function?
	jz	@F			; Brif not
	test	ah,ST_ByVal OR ST_Seg	; ByVal or Seg present
	call	TmErrorNZ		
@@:					
	xor	dh,ah			;Flip flag bits
	and	ah,not (ST_ByVal + ST_Seg) ;Mask out ByVal & Seg
	xor	dh,ah			;Restore all but ByVal & Seg
	pop	PTRTX[di+2]		;oTx of end of argument
	push	cx			;Save count of params
	call	CoerceParam		;Adjust parameter to match calling conv.

;ax = oRS of decl.
;bx adjusted to continue to point to end of argument
;dx preserved

	test	dl,FP_CDECL		;If CDECL, must re-order params
	jz	NoReorder
	cmp	[SsParmCnt],2		;If less than 2 parms, no re-order
	jl	NoReorder

;Re-order parameters for CDECL by inserting branch after each.
;Target of branch will be assigned later.

	push	ax
	call	InsertBranch
	pop	ax
NoReorder:
	pop	cx
	loop	NextParam

	DbPub	SsParamsOK
SsParamsOK:
;Done with parameter processing
;bx = oTx of end of first parameter (if any)
;dx = flags
;Start of [27]
;Find oTx before first parameter
	mov	cx,bx			;save otx of call if 1 parm
	cmp	sp,[SsBosStack]		;Any entries on stack for this stmt?
	mov	bx,[SsOTxStart] 	;Assume not - use start of stmt
;Note that SsBosStack is the SP at BOS, except it was saved with one extra
;word pushed on it.  Thus SP will actually be larger that SsBosStack when
;the stack is empty.
	ja	HaveStartOtx		;Stack empty?
;Get first oTx by looking at scanner entry on stack
	pop	ax			;oTyp
	pop	bx			;oTx
	push	bx			;Put them back
	push	ax
HaveStartOtx:
;bx = oTx of start of first parameter
;cx = oTx of call if 1 parameter
;Save 8087 registers if function call
	xchg	ax,cx			;ax = otx of call if 1 parm
	and	dh,ProcType
	cmp	dh,PT_SUB		;Was it a SUB?
	jz	No87Save
	push	ax
	mov	ax,codeOFFSET exSave87
	call	Insert
	pop	ax			;ax = otx of call if 1 parm
	inc	ax
	inc	ax			;adjust for insertion

No87Save:
;end of [27]
	cmp	[SsErr],0		;Any errors so far?
	DJMP	jnz	FuncCheck	;If so, don't try this stuff
	test	dl,FP_CDECL		;If CDECL, must re-order params
	jz	NotCDECL
	mov	cx,[SsParmCnt]
        jcxz    NotCDECL                ;No work if 0 parameters
        dec     cx
	xchg	ax,bx			;bx = otxCall if 1 parm, ax = otx first
					; param
        jz      EatParams               ;Just eat parameter on return
	xchg	ax,bx			;swap em back
	push	dx			;Save flags
	call	InsertBranch		;Jmp to last arg--don't know oTx yet
;Have inserted exBranch's to re-order parameters.  Now go find them
;and set their target operand.
	push	bx			;Remember this spot for patching later
	call	FindBranch
	pop	ax			;Restore "previous"
	push	bx			;1st param's jump to be patched later
	push	ax
PatchBranch:
;ax = oTx of previous branch
;bx = oTx of current branch
	push	ax
	push	bx
	call	FindBranch
	pop	ax			;oTx of previous branch
	pop	dx			;dx = target of branch
	call	PatchBranchRel		;compute and patch relative addr
	loop	PatchBranch
;bx = Otx after last branch == otx of call instruction
	mov	cx,bx			;save otx of call
	pop	bx			;oTx of 1st param's branch
	xchg	dx,ax			;target in dx
	call	PatchBranchRel		;patch relative addr
	pop	bx			;oTx of 2nd param's branch
	mov	dx,cx			;target is call instruction
	call	PatchBranchRel		
	mov	bx,cx			;Location of CALL
        pop     dx			;Restore flags
EatParams:
;Insert pcode to eat parameters
	mov	cx,[SsCbParmCur]
;cx = amount of stack space to release
;bx = oTx of CALL
	mov	ax,codeOFFSET exParamCnt
	call	Insert1Op
;bx = oTx of CALL
NotCDECL:
	test	dl,FP_DEFINED		;Interpreted function?
	jnz	FuncCheck
;Compiled code - add temp to save return oTx and cbParams
	mov	ax,4
	call	AllocTemp
FuncCheck:
	cmp	dh,PT_SUB			;Was it a SUB?
	je	ProcExit			;If so, we're done
;Make stack entry for function return value
	push	di				;Save oTx
	mov	al,[FuncOtyp]
	cbw
	push	ax			;Leave oType on stack
	test	dl,FP_DEFINED		;Interpreted function?
	jnz	ProcExit
	test	dl,FP_CDECL		;C function?
	jnz	ProcExit
;PL/M function - allocate temp for return value
	.erre	ET_MAX LT 100h		; Assure we can use AL
	cmp	al,ET_I4		;Returned in registers?
	jle	ProcExit
	cmp	al,ET_SD		;Returned in registers?
	jae	ProcExit		
	call	CbTypOTypSCAN		; Get size of this type
	call	AllocTemp		;Make space for return value
ProcExit:
	jmp	[ScanRet]

;*** PatchBranchRel
;
;	Added with [49].
;Inputs:
;    dx = location to jump to
;    bx = ptr to pcode AFTER exBranchRel operand
;Outputs:
;    [bx-2] is patched to contain offset of target relative to bx-2.
;Preserves:
;    all except dx.
;
PatchBranchRel:
	mov	PTRTX[bx-4],codeOFFSET exBranchRel ;backpatch exbranch
				; to exBranchRel
	sub	dx,bx		;dx = offset target relative to next pcode
	inc	dx
	inc	dx		;compute offset relative to ExBranchRel operand
	mov	PTRTX[bx-2],dx	;patch it
	ret

;*** SsFindOpNoList1,FindBranch
;
;Inputs:
;	ax = Executor whose opcode is opNoList1 (SsFindOpNoList1 only)
;	bx = start of search range
;	di = end of search range
;Outputs:
;	bx = oTx of point after opcode if found
;	Carry flag set if not found
;Preserves:
;	cx

	public	SsFindOpNoList1

FindBranch:
	mov	ax,codeOFFSET exBranch	;Look for this executor
SsFindOpNoList1:
	GetCodeIntoDs	SCAN		
assumes	ds,NOTHING
LookOpNoList1:
	mov	dx,PTRTX[bx]		;Get executor
	cmp	ax,dx			;Find it?
	jz	FoundNoList
	xchg	dx,bx
	mov	bx,[bx-2]		;Get opcode
	and	bx,OPCODE_MASK		;Just want the opcode!
	mov	bl,mpOpAtr[bx]		;Load attribute byte
	and	bx,OPA_CntMask		;Get the operand count from attribute
	cmp	bl,OPA_CntMask		;Check for cnt field in operand
	xchg	bx,dx
	jne	SkipOps 		;No cnt field
	inc	bx
	inc	bx
	mov	dx,PTRTX[bx]		;Get count of operands
	inc	dx
	and	dl,not 1		;Round up to even
SkipOps:
	add	bx,dx
	inc	bx
	inc	bx
	cmp	bx,di
	jb	LookOpNoList1
	stc				;No more found
FoundNoList:
	lea	bx,[bx+4]		;Point to next pcode w/o affecting flags
	push	ss
	pop	ds
	ret

;*** CoerceParam
;
;Purpose:
;	Perform whatever translations are necessary to make the
;	parameter match its declaration and get passed securely.
;Inputs:
;	ax = current type, high bits set
;	bx = oRS of declaration
;	dx = flags
;	es:[di+2] = oTx of end of argument
;	es:[di] = oTx of declaration
;Outputs:
;	ax = oRS of declaration
;	bx = oTx of end of argument (after any insertions)
;Preserves:
;	dx

	extrn	GetTrueType:near	; From ssrefarg.asm


	assumes ds,DATA

	DbPub	CoerceParam
CoerceParam:
	push	bx
	mov	cx,ax
	.erre	ST_Typ_Mask EQ 0FFh	;Assure XOR is sufficient
	xor	ch,ch			;Use current type if none declared
	test	dh,NoParamsFlag
	jnz	NoType
	xchg	si,PTRTX[di]		;Get oTx of declaration
	sub	si,6			;Point to next parameter
	call	GetDeclSeg		;ds:si point to parameter
	assumes ds,NOTHING		
	mov	bh,[si+DCLP_atr+1]	;Get high byte of ParamAtr
	and	bh,HIGH ResetBits
	or	dh,bh			;Combine ByVal and Seg bits
	mov	bx,[si+DCLP_oTyp]	;Get oType
	push	ss
	pop	ds			;Set ds = ss
	assumes ds,DATA 		
	xchg	si,PTRTX[di]		;Restore text source
	or	bx,bx			;Typed "as any"?
	jz	NoType	 		;If so, use attributes but not type
;Re-written with [37]
	pop	cx			;cx = oRS of declaration
	push	cx
	push	ax			;Current oTyp, high bits set
	push	bx			;Required oTyp
	push	cx
	mov	bx,PTRTX[di+2]
	call	GetTrueType
	xchg	ax,cx			;Actual oTyp to ax
	pop	bx			;Get oRs of declaration back
	pop	cx			;Get required oTyp back
	push	cx
	cmp	al,ET_FS		;Have an FS?
	jnz	LongCompare
	dec	ax			;FS-->SD, FT-->TX
	test	dh,ArrayFlag		;Passing whole array?
	jnz	NoMatch			;FS not allowed if so
LongCompare:
	push	dx
	xchg	ax,dx			;Current type to dx
	mov	ax,[grs.GRS_oRsCur]
	xchg	ax,bx			;oRS of decl. to ax, oRScur to bx
;bx:dx = oRS:oType of current type
;ax:cx = oRS:oType of target type
	cCall	CompareTyps,<ax,bx,cx,dx>	; Are types the same?
	REFRESH_ES			
	pop	dx
	or	ax,ax			; set PSW.Z flag
	jz	TypeMatch
NoMatch:
	or	dl,TypeMatchFlag
TypeMatch:
	pop	cx			;Restore target type
	pop	ax			;Restore current type with flag bits
;End of [37] re-write
NoType:
	mov	bx,PTRTX[di+2]		;Get oTx to end of argument

;ax = current type, high bits set
;bx = oTx of end of argument
;cx = target type (ET_RC ok)
;dx = flags

;Check for consistency between BYVAL, SEG, and CALLS
	test    dh,SegFlag+CallSFlag    ;Specified as segmented?
	jz	AttrOk			;If not, BYVAL would be OK
	test    dh,ByValFlag		;BYVAL and SEG/CALLS?
	jnz     BadType			;Can't have both
AttrOk:

	TestX	ax,ST_Var?		;Is it a variable or expression?
	jz	Expr
;Determine if whole array is being passed
	TestX	ax,ST_Array?		;Is this the actual array reference?
	jnz	NotWhole
	cmp	PTRTX[bx-4],0		;Any indices?
	jnz	NotWhole
;Passing whole array - see if that is what's needed
	test	dh,ArrayFlag+NoParamsFlag
	jnz	WholeArray
BadType:
	mov	ax,MSG_ParmTM
ReportErr:
	call	SsErrorBx
	jmp	short ParamXpop

NotWhole:
	test	dh,ArrayFlag		;Passing whole array?
	jnz	BadType
	test	dh,DefFnFlag+ByValFlag	;Pass by value?
	jnz	Expr
;Handle a variable
	test	dl,TypeMatchFlag	;Types match?
	jnz	BadType
	test	dh,FarArg		;SEG specified?
	jz	MakeSafeRef
;Passing by far reference.  Disable any copying of arguments,
;allowing user to screw himself with far heap movement.
	add	[SsCbParmCur],2		;Add 2 bytes to parm size
	mov	[SsOtxHeapMove],0	;Tell SsRefArg there's no heap movement
MakeSafeRef:
	call	SsRefArg
	mov	[SsOtxHeapMove],di	;Make sure we know we moved
ParamXpop:
	pop	ax			;Leave oRS of decl. in ax
	ret

CountSize:
	cmp	cx,ET_SD		;Don't allow string with ByVal
	jae	BadType 		;Error if string or form
	dec	ax
	dec	ax			;Already assumed near ref.
	add	[SsCbParmCur],ax
	mov	ax,codeOFFSET exR8ToStack
	sub	cx,ET_R8		; Is this an R8 ByVal param?
	    jz	    @F			; Brif yes, insert
	    .erre   ET_R4 EQ ET_R8-1	
	    inc     cx			; Is this an R4 ByVal param?
	    jnz     ParamXPop		; Brif not, no work to do
	    mov     ax,codeOFFSET exR4ToStack
@@:					
InsertExit2:
	call	Insert			; Insert executor to move to 8086
	jmp	short ParamXPop

WholeArray:
	test	dl,TypeMatchFlag	;Types match?
	jnz	BadType

	;Convert AIdLd to AdRf

	call	MakeArrayRef		
	jmp	short SegCheck

Expr:
	test	dh,ArrayFlag		;[J2] Is a whole array expected?
	jnz	BadType 		;[J2] Brif so
	cmp	cx,ET_MaxStr		;Is this numeric or string?
	ja	BadType			;Can't pass expr. to field/form/menu


	;If no type checking is enabled for this parameter, then fixed
	;strings will come here with an invalid target type of fixed
	;string or fixed text.	In this case, the target is converted to
	;variable string (ET_SD) to force the correct handling of temp
	;string arguments.  Using ET_SD is safe in FV_TEXT products
	;because the representation of Text and String are identical
	;as is the handling by this code.

	    .erre   ET_FS EQ ET_MaxStr	
	jb	@F			; Brif not ET_FS
	mov	cx,ET_SD		; Ok for ET_FT also
@@:

	push	dx
	push	cx			;Remember target type
	call	SsCoerceReg		;bx will be updated if coercion performed
	pop	cx
	pop	dx			;Recover flags bits
	mov	ax,cx			;oTyp to ax
	call	CbTypOTypSCAN		; AX = Size of this type
	test	dh,ByValFlag		;Passing by value?
	jnz	CountSize		;Done, but add up size of params
	    push    cx
	call	AllocTemp		;oTemp in cx, oTyp in ax
	xchg	ax,bx			;Type to bx, oTx in ax
	shl	bx,1			;Make it a word index
	mov	bx,[bx].tTmpType-2	;Get temp executor for this type
	xchg	ax,bx
	call	Insert1Op
	pop	ax			;Recover type
	cmp	al,ET_SD		;String?
	jne	SegCheck		;If not, go see if SEG specified
;Insert executor to delete string data
	mov     ax,codeOFFSET exDelTmpSD
;ax has executor, cx has oTemp
	push    bx
	mov     bx,di
	call    Insert1Op
	pop     bx
SegCheck:
	    test    dh,FarArg
	    jz	    ParamXPop
	    add     [SsCbParmCur],2	;Add 2 bytes to parm size
	    mov     ax,codeOFFSET exPushSeg ;Executor to add segment
	    jmp     short InsertExit2	; Insert opNoList0 and exit
@@:
	jmp	short ParamXPop

	;Added with [37]


	;End of [37]



DbPub	GetDecl 			
GetDecl:
;Get ds:si to point to procedure's declaration given pPRS in bx
;Returns oRS of declaration in ax
	mov	si,PTRRS[bx].PRS_oTxDef ; oText of declaration
	add	si,DCL_atr+2		;Position of attributes field
	mov	ax,PTRRS[bx].PRS_oRsDef ; Get oRs of decl.
	test	dh,DefFnFlag
	jz	GetDecSeg
	test	PTRRS[bx].PRS_flags,FP_DEFSCANNED ; Has definition been scanned?
	jnz	ValidDefFn
	xchg	ax,bx			;[J2] Save oRs across call to SsError
	mov	ax,ER_UF		;Undefined function
	xchg	si,PTRTX[di]		;[J2] Restore source pointer
	call	SsError
	xchg	si,PTRTX[di]		;[J2] Resave source pointer
	xchg	ax,bx			;[J2] restore AX = oRs
ValidDefFn:
	inc	si
	inc	si			;Skip over link field in DefFN
GetDecSeg:
	mov	bx,ax

GetDeclSeg:
;Get segment of text table into ds given oRS in bx.
;Inputs:
;	es:bx = pRS
;Outputs:
;	ds = segment of text table
;	es = text segment for scanning
;Preserves:
;	ax,cx,dx
	cmp	bx,[grs.GRS_oRsCur]	;Is it current?
	jz	CurText
	cmp	bx,[grs.GRS_oMrsCur]	;Current MRS?
	jz	IsMrsCur
	or	bx,bx
	jns	GetMRS
	and	bh,7Fh			;bx = oPrs
	lea	bx,[bx].PRS_txd.TXD_bdlText_seg		
	jmp	short SetDS		;	

IsMrsCur:
	test	[txdCur].TXD_flags,FTX_mrs ;does prs have a text table?
	jne	CurText 		;brif not, txdCur is for Mrs.
	mov	bx,dataOFFSET mrsCur.MRS_txd.TXD_bdlText_seg	
	jmp	short SetDS_2		;[5]

GetMRS:
	lea	bx,[bx].MRS_txd.TXD_bdlText_seg		
	jmp	short SetDS		;	

CurText:
	mov	bx,dataOFFSET txdCur.TXD_bdlText_seg	
SetDS_2:				;[5]
	SETSEG_EQ_SS  es		; set es = ds if Rs table is far
	jmp	short SetDS_1		;[5]
SetDS:					
	    RS_BASE add,bx		
	GETRS_SEG es,bx,<SIZE,LOAD>	;[9]
SetDS_1:				;[5]
	GETSEG	bx,PTRRS[bx],,<SIZE,LOAD> ;[9][5]
	GETSEGTXTCUR			
	mov	ds,bx			
	ret

sEnd	SCAN
	end
