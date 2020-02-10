page	49,132
	TITLE	ssdescan - Descan support routines
;***
;ssdescan - Descan dispatched routines
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;   This module contains descan routines.  There is one descan
;   routine for one to many scan routines.
;
;   The connection between scan routines and descan routines is as
;   follows:
;   - Each scan routine is declared with macro SsProc.	This macro
;     places the address of the descan routine in the word before the
;     scan routine (much like the connection between executors and their
;     opcodes).
;   - Each descan routine is declared with macro SsDProc.  This macro
;     declares a public entrypoint for the descan routine.
;
;   Descan routines are dispatched with:
;   ax = opcode
;   si = descan source
;   di = descan destination
;
;
;*****************************************************************************

	.xlist
	include 	version.inc
SSDESCAN_ASM = ON
	IncludeOnce	context
	IncludeOnce	opcontrl
	IncludeOnce	opmin
	IncludeOnce	optables
	IncludeOnce	pcode
	IncludeOnce	ssint
	IncludeOnce	txtmgr
	IncludeOnce	variable
	.list


assumes ds, DATA
assumes es, NOTHING
assumes SS, DATA


subttl	Descan routines
sBegin	SCAN
assumes cs,SCAN

extrn	mStNextOpExe:word

page
SsDProc Eot
	mov	ScanRet,SCANOFFSET DescanTerm	  ;Cause descan termination
;***
;SsD_BOS, Others with no operands.
;
;Purpose:
;
;   Descan executors that have no operands.
;
;   NOTE: Eot causes descan termination by modifying the dispatch
;	  loop return address.
;
;Input:
;
;   standard descan dispatch
;
;Output:
;
;   standard descan dispatch
;
;******************************************************************

SsDProc LUBound1		
SsDProc LUBound2		
SsDProc ReDim
SsDProc ReDimScalar		
SsDProc 2FnSamePExe
SsDProc Shared
SsDProc 0FnEtExe
SsDProc FnString
SsDProc	UMi
SsDProc 1FnPExe
SsDProc 1FnPExeR8		
SsDProc Do
SsDProc CaseElse
SsDProc CaseTo
SsDProc Case
SsDProc EndSelect
SsDProc	StView

IdOpEmitId:
	STOSWTX 			;Emit the opcode or oNam or variable offset
	jmp	[ScanRet]		;Return to descan loop

;Added with [18]
;end of [18]

SsDProc NoList3
	inc	si
	inc	si		;Skip over operand
SsDProc NoList2
	inc	si
	inc	si		;Skip over operand
SsDProc NoList1
	inc	si
	inc	si		;Skip over operand
SsDProc NoList0
	jmp	[ScanRet]

SsProc	NoList3,,local		;No scan routines, just descan pointers
SsProc	NoList2,,local

tNextType	label	word
	db	ET_R8,0
	db	ET_R4,0
	db	ET_R4,0
	db	ET_I4,0
	db	ET_I4,0
	db	ET_I2,STYP_Step
	db	ET_I2,0

SsDproc Next
	test	byte ptr es:[si+2],1	;NEXT without IdRf?
	jnz	SsD_NextId		;Bit set means no IdRf inserted
	sub	di,4			;Eat preceding IdRf
SsDproc NextId
	mov	dx,PTRTX[si-2]		;Get executor
	STOSWTX
	MOVSWTX	   			;Copy oFrame
	xchg	ax,dx			;Executor to ax
	push	di
	push	es
	push	cs
	pop	es
	mov	di,SCANOFFSET mStNextOpExe    ;Point to NEXT executors
	mov	cx,ET_MaxNum*2*2	    ; No. of executors
	repne	scasw			    ;Find which executor
	DbAssertRel	cx,ne,0,SCAN,<ssdescan:NEXT executor not found in map>
	cmp	cx,ET_MaxNum*2		    ; Still in first map?
	jb	LookUpNext
	sub	cx,ET_MaxNum*2		    
LookUpNext:
	pop	es
	pop	di
	mov	bx,cx			    ;Index of match
	shl	bx,1
	mov	ax,[bx-2].tNextType	    ;bx >= 2
	STOSWTX
	jmp	SsD_NoList1		    ;Skip source oTx operand

page
;***
;SsD_NOps - Descan routine
;Purpose:
;	Descan opcodes with:
;	- 0 to n operands that take no special processing
;
;	This includes IF syntax and label reference syntax as the label
;	references were descanned before executing the descan loop.
;
;	This routine is used as the dispatch point for SS_RUDE scan and
;	descan for the case that the opcode requires no work for this
;	transition.
;
;**********************************************************************
SsDProc FPutGet3
SsDProc FPutGet2
SsDProc LRSetMid
SsDProc LSet
SsDProc FnLen
SsDProc StLock
SsDProc Input
SsDProc Swap
SsDProc LineInput
SsDProc InputPrompt
SsDProc PaintWidth

SsDProc StConst

SsDProc AsType
SsDProc OffLd
SsDProc OffSt
SsDProc	ElemRef

SsDProc GPutGet
SsDProc PaletteUsing
SsDProc FieldItem
SsDProc StShared
SsDProc StCommon
SsDProc	Static
SsDProc	Dynamic
SsDProc StCallS
SsDProc	Sadd
SsDProc	Varptr
SsDProc Varptr$


SsDProc StStatic
SsDProc StDim				

SsDProc BOS
SsDProc	OptionBase0
SsDProc OptionBase1

SsDProc StEndProc
SsDProc StExitProc
SsDProc StCallLess
SsDProc StCall
SsDProc ByVal_Seg
SsDProc LParen

SsDProc For
SsDProc Exit

SsDProc IfBlock
SsDProc EndIf
SsDProc IfLab
SsDProc If
SsDProc Else
SsDProc ElseNop
SsDProc ElseIf

SsDProc While
SsDProc Wend
SsDProc DoLoop
SsDProc Loop
SsDProc LoopWhile

SsDProc Select

SsDProc Erase
SsDProc 4ET_ET
SsDProc StCnt
SsDProc Lit
SsDProc LitI2				
SsDProc Coerce
SsDProc Bol
SsDProc BolEmit
SsDProc NotInProc
SsDProc LabelRef		;Label references are descanned in a separate
SsDProc MrsLabelRef		; pass on the text - they are already descanned
SsDProc MrsMrsLabRef		; by the time this dispatch is taken.
SsDProc nLabelRef





SsDProc 0_0

public	SsD_NOps
SsD_NOps:
	STOSWTX 		;Emit the opcode
	and	ax,OPCODE_MASK
CopyOp:
	mov	bx,ax		;Move opcode to bx for CopyOperands
	shl	bx,1		;CopyOperands takes opcode * 2
	call	CopyOperands
	jmp	[ScanRet]	;Return to descan loop

;***
;SsD_AId - Descan Array Variables
;
;Purpose:
;
;   Arrays may be $STATIC or $DYNAMIC.	This is calculated each time
;   the array is scanned.  The FV_STATICSET bit indicates whether the
;   array type is yet known.  If the bit is not cleared, the user will
;   get a ReDim of $STATIC array error the second time the program is
;   scanned.
;
;**********************************************************************

SsDProc AVtRf
SsDproc AIdSt
SsDproc AIdLd
	STOSWTX 			;Emit the opcode
	LODSWTX 			;Pick up the count operand

	;Fall through .....

SsDProc VtRf
SsDproc IdSt
SsDproc IdLd
	STOSWTX 			;Emit the opcode
	LODSWTX 			;Pick up the oVar operand
	STOSWTX 			;Emit the oVar operand
	    add     ax,MrsCur.MRS_bdVar.BD_pb	;oVar --> pVar
	xchg	ax,bx			;BX = oVar
	DbChk	pVar,bx 		;Verify that this is a variable

	test	byte ptr [grs.GRS_oRsCur+1],80H ;Descanning module level?
	jz	Reset			;Brif yes, always reset.
	    TestM   [bx].VAR_Flags,FVSHARED ;[3] Shared variable?
	    jnz     NoReset		;Don't reset if SHARED and in proc
Reset:
	and	[bx].VAR_Flags,NOT FV_STATICSET 
	and	byte ptr [bx].VAR_fStat,NOT FV_Static
					;Indicate to recalc $STATIC
NoReset:
	jmp	[ScanRet]

	page
;***
;SsLabelDefDebind
;
;Purpose:
;
;   Descan opcodes that define labels.	These opcodes need link field
;   maintenance, and the oName must be copied.	Some varients may have
;   other operands.
;
;   This routine uses ssLinkCtl to maintain the address of the last
;   label definition link field.
;
;   txdCur is updated when the first definition is encountered.
;
;   The last label link is set to undefined after exiting from the
;   descan loop.
;
;   No forward reference links will be encountered as label references
;   are already descanned.
;
;***************************************************************************

SsDProc BolLabDef
SsDProc LabDef

SsLabelDefDebind:
	STOSWTX 				;Emit the opcode
	push	ax				;Save the pcode
	mov	bx,SsLinkCtl			;Address of link control struc
	mov	ax,[bx].TXLNK_LabDefLast	;oTx of last label def field
	or	ax,ax				;First link?
	jz	SsLabelDefFirst 		;No pervious label
	xchg	ax,bx
	mov	PTRTX[bx],di			;Update last link to here
	xchg	bx,ax
SsLabelDefCont:
	mov	[bx].TXLNK_LabDefLast,di	;Update address of last link
	pop	ax
	jmp	short CopyOp			;Copy operands and exit

SsLabelDefFirst:
	mov	txdCur.TXD_otxLabLink,di	;Update txd head pointer
	jmp	short SsLabelDefCont


page

sEnd	SCAN
sBegin	CODE
;Table of opcodes with label reference operands that must be descanned.
tLabelRefExecs	label	word
	DW cLabelRefOpcodes		;Count of entries in table
	DW opEot			;SsLabelRefDebind assumes opEot
					; is the 1st opcode in table
	DW opEvGosub
	DW opStGosub
	DW opStGoto
	DW opStOnError
	DW opStOnGosub
	DW opStOnGoto
	DW opStRestore1
	DW opStResume
	DW opStReturn1
	DW opStRunLabel
	DW opStElseLab
	DW opStIfLab
	DW opStIfGotoLab

cLabelRefOpcodes EQU ($-tLabelRefExecs-2) SHR 1
sEnd	CODE

sBegin	SCAN
assumes cs,SCAN

;***
;SsLabelRefDebind
;
;Purpose:
;
;   Debind all label references in the current text table.
;
;   When descanning, label references are unbound in a separate pass.
;   A separate pass is used because text is compressed during descan,
;   causing problems for backward label references.
;
;   The rule byte for label reference opcodes contains a specification
;   of the number of label operands.  It is either a count, or LOWUND,
;   which indicates that the first operand is a count of bytes of label
;   reference operands.
;
;NOTE:	This routine will not handle opcodes that can have 0 label references,
;   due to the structure of the PerReference loop.
;
;   Label reference opcodes descanned by this module are found in
;   tLabelRefExecs.  Txtmgr routine TxtFindOpExec searches text for
;   executors that match an opcode in this table.
;
;   Whenever a scan-time error occurs, further label binding is stopped.
;   Thus debinding is stopped when the location of the error is reached.
;
;Input:
;
;   di = offset of first pcode to consider descanning
;   es = current text segment
;
;Output:
;
;***************************************************************************
	public SsLabelRefDebind
SsLabelRefDebind:

PerLabelOpcode:
	mov	bx,CODEOFFSET tLabelRefExecs	;Table of opcodes containing
						; label references.
	push	bx
	PUSH_ES 				
	cCall	TxtFindOpExec,<di,bx>		;index to next label reference
						;dl = tbl index of opcode found
	POP_ES					; restore es = text tbl seg
	pop	bx				;restore bx -> tLabelRefExecs
	xchg	di,ax				;di = oTx of found pcode
	xchg	ax,dx				;Index to opcode found
	or	al,al				;Found EOT?
	jz	SsLabelRefDebindX		;Eot found - no more refs

;Process label reference pcode
	cbw					;ax = index to opcode found
	xchg	si,ax				;si = index to opcode found
	inc	di
	inc	di				;Skip to first operand field
	inc	si				;Skip over count of table entrys
	shl	si,1				;To word index
	GetCodeIntoDs	SCAN		
	mov	bx,[bx+si]		; Load opcode that was found
	push	ss			
	pop     ds			
	mov	si,di				;Sync source and dest on pcode
	call	GetRuleInfo
	cmp	al,LOWUND			;Test for operand = cblabel refs
	jnz	GotRefCount			;ax = actual label ref count
	LODSWTX 				;Pick up label ref count
	shr	ax,1				;cb to cLabel refs

GotRefCount:
	mov	dx,es				;default lab def to cur txt seg
	test	al,80H				;is def for ref in mrs?
	jz	GotTextSeg			;brif not, user cur txt seg
	test	txdCur.TXD_flags,FTX_mrs	;is mrs the cur txt table?
	jnz	GotTextSeg			;brif so, use cur txt table
	GETSEG	dx,[mrsCur.MRS_txd.TXD_bdlText_seg],,<SIZE,LOAD> 
					; get mrs txt table seg
					; text tbl in case we are already 
					; at module level

GotTextSeg:
	and	ax,80h-1			;High bit indicates scope
	xchg	cx,ax				;cx = Count
	mov	di,si				;Sync source and destination

PerRefLoop:
	cmp	si,[SsErrOTx]			;Outside error range?
	jae	SsLabelRefDebindX		;If so, terminate debinding
	LODSWTX 				;Pick up bound label ref
	push	es				;save label ref txt seg
	mov	bx,ax				
	inc	bx				;Test for label UNDEFINED
	jz	LabRefDebSkip			;Skip unbinding this label
	mov	es,dx				;use label def txt seg
	mov	ax,PTRTX[bx+3]			;Load oName from def pcode
						; NOTE:[bx+3] = [bx+4] w/ inc bx
LabRefDebSkip:
	pop	es				;recover label ref txt seg
	STOSWTX 				;Emit the oName
	loop	PerRefLoop			;Loop if more references

	jmp	short PerLabelOpcode		;Go search for the next opcode

SsLabelRefDebindX:
	ret

page
;***
;
;**********************************************************************
SsDProc StDeclare
SsDProc StSub
SsDProc StFunction
	STOSWTX
	xchg	ax,bx			;Opcode to bx
	shl	bx,1			;Must pass opcode*2
	mov	ax,PTRTX[si+2]		;Get oPRS
	push	[ScanRet]
	mov	dh,-1			;Set descan direction
	jmp	SsReLinkDecl

sEnd	SCAN
end
