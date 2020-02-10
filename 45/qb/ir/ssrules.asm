page	49,132
	TITLE	ssrules - Scanner Table Definitions
;***
;ssrules.asm - Scanner Table Definitions
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;   This module defines the coercion and rules tables used by the scanner.
;   The following is a description of how all scan tables are organized.
;
;   tOpSsDisp:	    tOpAtr:	    tOpRule:	    tOpExe:
;	    ssOp1	    op1Atr	    op1Rule	    ex1
;	    ssOp2	    op2Atr	    op2Rule	    ex2
;	    ... 	    ... 	    ... 	    ...
;
;   tOpSsDisp is the scan dispatch for the opcode.  Entries are 1 word.
;
;   tOpAtr is the atribute byte for the opcode. Entries are 1 byte.  The
;   atribute value is the number of operand bytes owned by the opcode.
;   Some opcodes own a variable number of operand bytes.  In these cases
;   the first operand word is the number of operand bytes - 2.	This is
;   signified by LOW UNDEFINED in the atribute table.
;
;   tOpRule is a scan helper byte that can be:
;	1. a rule table index.	Used in this way when there are more than one
;	   variables in the opcode to executor algorithm.  The rule table index
;	   is used to index tRuleWord and tRuleByte desribed below.
;	2. an ET type.	Used in this manner when the emitted type is the only
;	   variable in the opcode to executor algorithm.  An example is all
;	   intrinsic functions that have no operands (ERR, ERL,...).
;
;   tOpExe is the executor for the opcode.  If an opcode has several executors
;   then tOpExe contains the address of an ordered list of executors for
;   the opcode.
;
;   tRuleWord:
;	This table usually contains one of the following:
;	1. coercion table address
;	2. other scan routine specific information such as an ET type.
;
;   tRuleByte:
;	This table almost always contains an ET type which is the emitted
;	type for opcodes using this rule.  The special emitted type
;	LOW UNDEFINED indicates that the coerced input type is to be emitted.
;	For example, opAdd emits an I2 when the arguments are I2.
;
;   tCo<rule>
;	These are the coercion tables.	They are either one or two dimensional
;	and overlay each other as much as possible in order to save space.
;	Coercion tables contain:
;	1. ET types.  These are present when coercion may be required.	The
;	   type is the target type to which the current argument(s) must be
;	   coerced.
;	2. 0.  This value is used as a speed hack.  It is present when it is
;	   known that coercion is not required.
;	3. LOW UNDEFINED.  This value is used to indicate "type mismatch"
;	   was detected.
;
;   tCoerceExe is a 2 dimensional (typeXtype) table of coercion executor
;   addresses.
;
;In general, these tables are accessed as follows (refer to following diagram):
;	1. The opcode is used to index into tOpRule to find the rule index
;	   for the current op.	This allows a single scan routine to scan
;	   a family of related opcodes.
;	2. The rule index is used to find the coercion table and the emitted
;	   type for the current op.
;	3. Arguments are popped from the scan stack and used to index the
;	   coercion table.
;	4. For opcodes that emit a value the emitted type and current
;	   expression fragment pcode address are pushed on the stack.
;
;		tOpRule:	   tRuleWord: tRuleByte:
;		    [index]
;    opcode-------->[index]--------> [ptCoerce][ET_emit]
;		    [...  ]		 |
;					 +---------------->tCoerce
;
;
;****************************************************************************

	.xlist
	include 	version.inc
SSRULES_ASM = ON
	IncludeOnce	context 	    
	IncludeOnce	opintrsc
	IncludeOnce	pcode
	IncludeOnce	qbimsgs
	IncludeOnce	ssint
	IncludeOnce	txtmgr
	IncludeOnce	variable
	.list


assumes cs, SCAN
assumes ds, DATA
assumes SS, DATA

sBegin	CODE

	extrn	exLitI21:near		;Literal executor

	    extrn   exPushSeg:near	
	    extrn   exCopyTmpAr:near	

sEnd	CODE

subttl	Coercion Label Definitions

sBegin	SCAN

	extrn	GetTrueType:near	

mOpExLitR8	label word	    
	DWEXT	exLitR80	    
	DWEXT	exLitR81	    
	DWEXT	exLitR82	    
	DWEXT	exLitR83	    
	DWEXT	exLitR84	    
	DWEXT	exLitR85	    
	DWEXT	exLitR86	    
	DWEXT	exLitR87	    
	DWEXT	exLitR88	    
	DWEXT	exLitR89	    
	DWEXT	exLitR810	    

subttl	Coercion Support Functions
page
;***
;SsCoerceN - coerce n expressions on the scan stack to specified type
;Purpose:
;	Coerce n expressions on the scan stack to specified type.
;
;Input:
;	ax = oTyp of target (Record = ET_RC)
;	cx = count to coerce
;	si/di scan source and destination in pcode
;Output:
;	si/di updated if any coercion was required.
;	f_StaticCalc set FALSE if any expression was not a literal
;
;Preserves:
;	ax,bx
;***************************************************************************

	public	SsCoerceN
SsCoerceN:
	pop	[SsParmCnt]		; Get return address out of the way
	jcxz	EndCoerceN		
	.erre	ET_RC EQ 0		; Assure DEC is sufficient
	dec	ax			; Force ET_RC -> 0ffffh
	cmp	ax,ET_MaxNum		; Numeric type?
	jb	SsCoerceNStart		
	call	TMError 		
	xor	ax,ax			; Use valid type (ET_I2)
SsCoerceNStart: 			
	inc	ax			; Restore correct oTyp
SsCoerceNLoop:
	pop	dx			; Get oType
	push	dx			; But keep it on stack
	xor	dh,high ST_Lit? 	;High bits now zero if literal
	test	dh,0C0H 		;Determine if this index is a literal.
	jz	SsCoerceNNotLit 	;Not a literal index
	mov	f_StaticCalc,FALSE	;Indicate not a $STATIC array if this
SsCoerceNNotLit:			; is the first reference.
	test	dh,ST_ByVal+ST_Seg	; Have ByVal or Seg been used?
	jz	@F			; Brif no
	call	TMError 		
@@:
	call	EnsureArgType		
	loop	SsCoerceNLoop		;Go check next index
EndCoerceN:				
	jmp	[SsParmCnt]		; Return to caller

	page
;***
;EnsureArgType
;Purpose:
;	Ensure that the expression entry on the stack is of the desired
;	type.
;Input:
;	ax = type required for this expression
;	si/di scan source and emit
;
;	ParmW	oTxExp
;	ParmW	oTypExp (Records use ET_RC)
;
;Output:
;	ax = oTyp of target (Records use true oTyp)
;Preserves:
;	bx,cx,dx
;****
cProc	EnsureArgType,<NEAR,PUBLIC>,<ax,bx,cx,dx>
ParmW	oTxExp
ParmW	oTypExp
cBegin
	mov	bx,oTxExp		;Expression location
	cmp	ax,ET_MaxNum		; Is target numeric ?
	xchg	cx,ax			;CX = oTyp of target
	jbe	Numeric 		
	mov	ax,oTypExp		;AX = oTyp of source (Record = ET_RC)
	TestX	ax,ST_Var?		;Is it a variable?
	jz	Coerce			;If not, it won't move
	mov	dh,0			;Assume near - SD/FS
	or	al,al			;[5] Is it a record?
	jnz	SafeRef 		; Make sure reference is safe
	mov	dh,FarArg		;Must be record - want far ref
SafeRef:
	call	SsRefArg		;Make sure it's safe from moving
Numeric:
	mov	ax,oTypExp		;AX = oTyp of source (Record = ET_RC)
Coerce:
	call	SsCoerceReg
cEnd

;SsCoerceReg
;
;Purpose:
;	Insert a coercion token to cause an expression to be coerced to a
;	specified type.
;
;	Literals 0 through 10 may be coerced from I2 to R4 by modification
;	of the executor for these literals.
;
;	If types match, no action is performed.
;
;Inputs:
;	ax = oTyp of expression (with high bits set) (Record = ET_RC)
;	bx = oTx of expression
;	cx = oTyp of target
;Outputs:
;	bx = oTx of expression, after coercion
;	si and di updated if insertion needed
;Exceptions:
;	If expression or target type is not numeric, Type Mismatch Error
;	is generated.
;
;	Coercion token insertion may cause the text table to grow.  This
;	may result in OME.

	public	SsCoerceReg
SsCoerceReg:
	cmp	ax,ET_I2+ST_Lit 	;Determine if lit without operand
	jne	NoLitCoerce		;Can't optimize by replacing lit executor
	cmp     cx,ET_R4		
	jz	LitCoerce		
	cmp	cx,ET_R8
	jne	NoLitCoerce
LitCoerce:
;Target type is R4/R8, so replace executor with 1 word 8087 literal
	mov	dx,bx			;Preserve address of this executor
	mov	bx,PTRTX[bx-2]		;Load literal executor
	GetCodeIntoDs	SCAN		
	mov	bl,byte ptr [bx-1]	; Load MSB of opcode
	push	ss			
	pop     ds			
	.erre	OPCODE_MASK EQ 03ffh	; Assure following code is ok
;	and	bx,HIGH (NOT OPCODE_MASK)
	and	bx,0fch 		; Clear unwanted bits
	shr	bx,1			; Convert to word offset
	mov     ax,mOpExLitR8[bx]	; Get corresponding R8 lit executor
	mov	bx,dx			;Restore address of I2 executor in text
	mov	PTRTX[bx-2],ax		;Store the new R4 literal executor.
EndCoerce:
	ret

RecordChk:
;	At this point, the coercion has been determined to be from a record.
;	However, the true oTyp of the record is not saved on the scan stack
;	ET_RC was used instead.  This code will use the oTx of the IdLd to
;	find the oVar and in turn the true oTyp of the source.	This is
;	then compared to CX to check that types match.	No coercion is
;	possible among records so there is either an error or nothing.
;
;	ax = oTyp of source expression
;	bx = oTx of source expression
;	cx = oTyp of target

	;In non-SizeD versions, it is possible that the oTx from the scan
	;stack will point after an exCopyTmpAr and exPushSeg instead of an
	;exIdLd, exAIdLd, or exOffLd.  To differentiate between the two, the
	;exPushSeg executor is forced to start at an odd address.  Since an
	;oVar and oElem must be even, the exPushSeg can be recognized.

	    cmp     PTRTX[bx-2],codeOFFSET exPushSeg
	    jne     @F			; Brif exIdLd, exAIdLd, or exOffLd
	    DbAssertRel PTRTX[bx-10],e,<codeOFFSET exCopyTmpAr>,SCAN,<SsCoerceReg: exCopyTmpAr>
	    sub     bx,10		; Backup over inserted executors
@@:
	mov	dx,cx			; DX = oTyp of target
	call	GetTrueType		; CX = oTyp of source
	cmp	cx,dx			; Do oTyps match?

	public	TMError,TMErrorNZ
TMErrorNZ:
	;Report type mismatch error if Z flag not set.
	jz	NoError
TMError:
	;Report type mismatch error.  All registers preserved.
	push	ax
	mov	ax,ER_TM		;Type mismatch error
	call	SsError
	pop	ax
NoError:
	ret

NoLitCoerce:
;Calculate the implicit coercion executor by entering tImpCo as a two
;dimensioned table of executor addresses.
	.erre	ET_RC EQ 0		; Assure JZ is sufficient
	or	al,al			
	jz	RecordChk		; Brif source is record
	cmp	cx,ET_MAX		; Is target a primitive type?
	ja	TMError 		; Brif not.  This is an error
	call	MSdFs			; Map ET types: FS=SD
	xchg	ax,cx			; AX = Target type
	call	MSdFs			; Map target type too
	cmp	ax,cx			;Types already match?
	je	EndCoerce		; If match, skip coercion

        .erre   ET_SD EQ 5

	add	ax,cx			;AX = Source + Target
	shl	cx,1			;CX = Source * 2
	shl	cx,1			;CX = Source * 4
	add	ax,cx			;AX = Source * (5|7) + Target
	xchg	ax,bx			;AX = oTx, BX = Table index
	shl	bx,1			;To word index
	mov	cx,tImpCo[bx]		;Enter coercion executor table for executor
	xchg	bx,ax			; oTx to bx
	jcxz	EndCoerce
	mov	ax,cx			;Executor to insert to ax
	dec	cx			; Is this 1 (incompatible types)?
	jcxz	TMError 		; Brif yes
;	jmp	short Insert		;Fall into Insert, below


;*** Insert - Insert word into pcode
;
;Inputs:
;	ax = word to insert
;	bx = oTx to insert
;	cx = Number of bytes to make room for (InsertCx only)
;	si = scan source
;	di = scan destination
;Outputs:
;	cx = number of bytes inserted
;	bx = input bx + output cx: oTx of point after insertion
;	si, di updated for text moves
;Exceptions:
;	If OME:
;		Error recorded by SsError
;		Carry flag set
;Preserves:
;	ax,bx,dx (bx updated)

public	Insert,InsertCx,InsertBranchBos,InsertBranch,Insert1Op
extrn	exBranch:near

Insert:
	mov	cx,2			;Make room for two bytes
InsertCx:
	cmp	[SsErr],0		;Any errors?
	jnz	InsertFail		;Don't let error location get invalid
	push	ax
	push	bx
	push	cx
	push	dx
;Ensure room.  This may cause source side of text to move within the segment.
	call	SsEnsureGap		;Ensure reasonable gap size
	pop	dx
	pop	cx
	pop	bx
	pop	ax
	jc	InsertOME		;Out of memory error
	or	[SsBosFlags],SSBOSF_Inserted	;Remember insertion occured

;Expand the destination text to make room for add'l executor
	push	si
	mov	si,di
	add	di,cx			;New emit address
	push	di
	push	cx
	mov	cx,si
	sub	cx,bx			;cb between insert oTx and old emit oTx
	shr	cx,1			;count of words
	inc	cx			;Include word at old emit address
	push	es
	pop	ds			;Prepare for es -> es copy
	std				;Copy direction is down
	rep	movsw			;Copy to make room for executor
	cld
	mov	[bx],ax 		;Insert additional executor
	pop	cx
	add	bx,cx			;Point bx to next insertion point
	push	ss
	pop	ds			;Restore data segment
	pop	di
	pop	si
	ret

InsertOME:
	push	ax
	mov	ax,ER_OM	;Signal out-of-memory error
	call	SsError
	pop	ax
InsertFail:
	stc			;Inform caller to make sure nothing is inserted
	ret


;***
;InsertBranchBos
;
;Purpose:
;	Inserts an exBranch after opBos in current line.
;Entry:
;	cx = operand
;Exit:
;	bx - oTx after inserted exBranch
;	si, di - adjusted for inserted pcode
;Preserves:
;	cx
;******************************************************************************
InsertBranchBos:			;Insert Branch after opBos
	mov	bx,[SsOTxStart] 	; Points to first opcode after BOS
InsertBranch:
	mov	ax,codeOFFSET exBranch
Insert1Op:
;Insert opcode in ax with operand in cx
;Preserves all (bx updated)
	push	cx
	mov	cx,4			;Has a 2-byte operand
	call	InsertCx
	pop	cx
	jc	NoOperand
	mov	PTRTX[bx-2],cx
NoOperand:
	ret



subttl	Executor Input Types Maps
page

;	These tables allow rapid calculation of what coercion is required
;	for parameters for most BAsiC statements and functions.
;
;	The naming convention is:
;		tCo<input arg count><input type reqs|>to<required type>
;
;	where:
;		<input arg count> is the number of dimensions in the table.
;		<input type reqs> shows the limitations on the types handled
;				  by the statements/functions using this table.
;				  This field may be empty.
;		<required type>   This is the required final coerced type rule.
;				  This is different for "+" and "*" for
;				  example, as both have the same required type
;				  rule (types must be the same), but "*" has
;				  an input types restriction (no strings).
;
;
;	These tables are used to calculate the required input type for unary
;	operators and cases where binary operators require input types coerced
;	to be equal.
;
;	RulTab and related macros added with [14]
;
;------------------------------------------------------------

;Initialize a two-dimensional coercion table
StartTab	MACRO
LinNo	=	0
	ENDM

;Only define a byte if condition is true
DbIf	MACRO	Byte,Cond
IF	Cond
	db	Byte
ENDIF
	ENDM

;Associate a line number (LinNo) with a type
LinNo	=	0
	IRP	Typ,<I2,I4,R4,R8,Cy,Sd,Tx,Field>
LinNo	=	LinNo+1
Lin&Typ	=	LinNo
	ENDM

;Make a line of two-dimensional table
RulTab	MACRO	I2,I4,R4,R8,Cy,Sd,Tx,Field
LinNo	=	LinNo+1

IF	LinNo EQ LinCy
	EXITM
ENDIF
IF	LinNo EQ LinTx
	EXITM
ENDIF
IF	LinNo EQ LinField
	EXITM
ENDIF

	RulLine	I2,I4,R4,R8,Cy,Sd,Tx,Field

	ENDM

;Make a line of a coercion table using only those types enabled
RulLine	MACRO	I2,I4,R4,R8,Cy,Sd,Tx,Field
	Db	I2
	Db	I4
	Db	R4
	Db	R8
	Db	Sd
	ENDM

tCo2toSame:
	StartTab
	public	tCo1toNotSD		
tCo1toNotSD:
RulTab	ET_I2,	ET_I4,	ET_R4,	ET_R8,	ET_CY,	LOWUND,   LOWUND, ET_FIELD;I2
RulTab	ET_I4,	ET_I4,	ET_R8,	ET_R8,	ET_CY,	LOWUND,   LOWUND, ET_FIELD;I4
RulTab	ET_R4,	ET_R8,	ET_R4,	ET_R8,	ET_CY,	LOWUND,   LOWUND, ET_FIELD;R4
RulTab	ET_R8,	ET_R8,	ET_R8,	ET_R8,	ET_CY,	LOWUND,   LOWUND, ET_FIELD;R8
RulTab	ET_CY,	ET_CY,	ET_CY,	ET_CY,	ET_CY,	LOWUND,   LOWUND, ET_FIELD;CY
RulTab	LOWUND, LOWUND, LOWUND, LOWUND, LOWUND, ET_SD,	  ET_TX,  ET_FIELD;SD
RulTab	LOWUND, LOWUND, LOWUND, LOWUND, LOWUND, ET_TX,	  ET_TX,  ET_TX   ;TX
RulTab	ET_FIELD,  ET_FIELD,  ET_FIELD,  ET_FIELD,  ET_FIELD,  ET_FIELD, ET_TX,  ET_FIELD;Field

;------------------------------------------------------------

tCo2NotSdtoSame:
	StartTab
RulTab	ET_I2,	ET_I4,	ET_R4,	ET_R8,	ET_CY,	LOWUND, LOWUND, ET_FIELD;I2
RulTab	ET_I4,	ET_I4,	ET_R8,	ET_R8,	ET_CY,	LOWUND, LOWUND, ET_FIELD;I4
RulTab	ET_R4,	ET_R8,	ET_R4,	ET_R8,	ET_CY,	LOWUND, LOWUND, ET_FIELD;R4
RulTab	ET_R8,	ET_R8,	ET_R8,	ET_R8,	ET_CY,	LOWUND, LOWUND, ET_FIELD;R8
RulTab	ET_CY,	ET_CY,	ET_CY,	ET_CY,	ET_CY,	LOWUND, LOWUND, ET_FIELD;CY
RulTab	LOWUND,	LOWUND,	LOWUND,	LOWUND,	LOWUND,	LOWUND,	LOWUND,	LOWUND	;SD
RulTab	LOWUND,	LOWUND,	LOWUND,	LOWUND,	LOWUND,	LOWUND,	LOWUND,	LOWUND	;TX
RulTab	ET_FIELD,  ET_FIELD,  ET_FIELD,  ET_FIELD,  ET_FIELD,  LOWUND, LOWUND, ET_FIELD   ;Field

;------------------------------------------------------------


;------------------------------------------------------------

tCo2toI2I4:
	StartTab
RulTab	ET_I2,	ET_I4,	ET_I4,	ET_I4,	ET_I4,	LOWUND,	LOWUND,	ET_I4	;I2
RulTab	ET_I4,	ET_I4,	ET_I4,	ET_I4,	ET_I4,	LOWUND,	LOWUND,	ET_I4	;I4
RulTab	ET_I4,	ET_I4,	ET_I4,	ET_I4,	ET_I4,	LOWUND,	LOWUND,	ET_I4	;R4
RulTab	ET_I4,	ET_I4,	ET_I4,	ET_I4,	ET_I4,	LOWUND,	LOWUND,	ET_I4	;R8
RulTab	ET_I4,	ET_I4,	ET_I4,	ET_I4,	ET_I4,	LOWUND,	LOWUND,	ET_I4	;CY
RulTab	LOWUND,	LOWUND,	LOWUND,	LOWUND,	LOWUND,	LOWUND,	LOWUND,	LOWUND	;SD
RulTab	LOWUND,	LOWUND,	LOWUND,	LOWUND,	LOWUND,	LOWUND,	LOWUND,	LOWUND	;TX
RulTab	ET_I4,	ET_I4,	ET_I4,	ET_I4,	ET_I4,	LOWUND,	LOWUND,	ET_I4	;Field

;------------------------------------------------------------

tCo2toR4R8:
	StartTab
RulTab	ET_R4,	ET_R8,	ET_R4,	ET_R8,	ET_R8,	LOWUND,	LOWUND,	ET_R8	;I2
RulTab	ET_R8,	ET_R8,	ET_R8,	ET_R8,	ET_R8,	LOWUND,	LOWUND,	ET_R8	;I4
RulTab	ET_R4,	ET_R8,	ET_R4,	ET_R8,	ET_R8,	LOWUND,	LOWUND,	ET_R8	;R4
RulTab	ET_R8,	ET_R8,	ET_R8,	ET_R8,	ET_R8,	LOWUND,	LOWUND,	ET_R8	;R8
RulTab	ET_R8,	ET_R8,	ET_R8,	ET_R8,	ET_R8,	LOWUND,	LOWUND,	ET_R8	;CY
RulTab	LOWUND,	LOWUND,	LOWUND,	LOWUND,	LOWUND,	LOWUND,	LOWUND,	LOWUND	;SD
RulTab	LOWUND,	LOWUND,	LOWUND,	LOWUND,	LOWUND,	LOWUND,	LOWUND,	LOWUND	;TX
RulTab	ET_R8,	ET_R8,	ET_R8,	ET_R8,	ET_R8,	LOWUND,	LOWUND,	ET_R8	;Field

;------------------------------------------------------------


;------------------------------------------------------------
tCo2toI2R4:
	StartTab
RulTab	ET_I2,	ET_R4,	ET_R4,	ET_R4,	ET_R4,	LOWUND,	LOWUND,	ET_R4	;I2
RulTab	ET_R4,	ET_R4,	ET_R4,	ET_R4,	ET_R4,	LOWUND,	LOWUND,	ET_R4	;I4
RulTab	ET_R4,	ET_R4,	ET_R4,	ET_R4,	ET_R4,	LOWUND,	LOWUND,	ET_R4	;R8
RulTab	ET_R4,	ET_R4,	ET_R4,	ET_R4,	ET_R4,	LOWUND,	LOWUND,	ET_R4	;R8
RulTab	ET_R4,	ET_R4,	ET_R4,	ET_R4,	ET_R4,	LOWUND,	LOWUND,	ET_R4	;CY
RulTab	LOWUND,	LOWUND,	LOWUND,	LOWUND,	LOWUND,	LOWUND,	LOWUND,	LOWUND	;SD
RulTab	LOWUND,	LOWUND,	LOWUND,	LOWUND,	LOWUND,	LOWUND,	LOWUND,	LOWUND	;TX
RulTab	ET_R4,	ET_R4,	ET_R4,	ET_R4,	ET_R4,	LOWUND,	LOWUND,	ET_R4	;Field

;------------------------------------------------------------
tCo1toI2SD:
RulLine	ET_I2,	ET_I2,	ET_I2,	ET_I2,	ET_I2,	ET_SD,	ET_TX,	ET_FIELD


;------------------------------------------------------------
tCo1toI2I4:
RulLine	ET_I2,	ET_I4,	ET_I4,	ET_I4,	ET_I4,	LOWUND,	LOWUND,	ET_I4

;------------------------------------------------------------
tCo1toSame:
RulLine	ET_I2,	ET_I4,	ET_R4,	ET_R8,	ET_CY,	ET_SD,	ET_TX,	ET_FIELD

	subttl	Coercion Rules Table
	page

MakRule MACRO
irule = 0
;Rule names are composed of two parts - the input specification
;   and the output specification.  The naming convention is:
;
;	SSR_<input spec>_<output spec>
;
;	<input spec> starts with the number of arguments
;	<output spec> of <LOWUND> indicates op emits the input type.
;
;NOTE:
;   Rules that have an output type the same as the only input type could
;   be represented with <LOWUND> or an explicit type as <output spec>.
;   These are not the same rule, as some scan routines require explicit
;   type as <output spec>.  However, there are no scan routines that
;   handle <LOWUND> as <output spec> but do not handle an explicit type.
;
;NOTE:
;   If output type = ET_MAX then no coercion routine uses that output type.
;   That rule field could be modified for use by some other scan routine.
;
;params:       <ispec><ospec>,	coercion table,  output type

	ruler	2Any_Same,	tCo2toSame,	<LOWUND>
	ruler	2NotSD_Same,	tCo2NotSDtoSame,<LOWUND>
	ruler	2I2I4_Same,	tCo2toI2I4,	<LOWUND>
	ruler	2R4R8_Same,	tCo2toR4R8,	<LOWUND>

	public	SSR_Div 				    

SSR_Div equ	SSR_2R4R8_Same				    

	ruler	2I2R4_I2,	tCo2toI2R4,	ET_I2
	ruler	2Any_I2,	tCo2toSame,	ET_I2


	ruler	2I2R4_None,	tCo2toI2R4,	0



	ruler	1NotSD_Same,	tCo1toNotSD,	<LOWUND>
	ruler	1I2I4_Same,	tCo1toI2I4,	<LOWUND>

	ruler	1I2I4_SD,	tCo1toI2I4,	ET_SD
	ruler	1I2SD_SD,	tCo1toI2SD,	ET_SD


	ruler	1I2SD_I4,	tCo1toI2SD,	ET_I4
	ruler	1NotSD_I2,	tCo1toNotSD,	ET_I2
	ruler	1NotSD_SD,	tCo1toNotSD,	ET_SD

	ruler	1Any_None,	tCo1toSame,	0
	ruler	1I2SD_None,	tCo1toI2SD,	0
;Label Reference Rules
;	       <#labs,#args>	<arg coercion>	<#label>
	ruler	1Lab0Arg,	UNDEFINED,	1
	ruler	1LabMain0Arg,	UNDEFINED,	<1 OR 80h>
	ruler	nLab1Arg,	0,		LOWUND

;Rules for SS_4ET_ET
;     NOTE: The low order nibble applies to the LAST argument in the syntax.
;	    The rule names are in syntax order.

	ruler	I2_None,	<ET_I2>, 		0
	ruler	I4_None,	<ET_I4>, 		0
	ruler	R4_None,	<ET_R4>, 		0
	ruler	R8_None,	<ET_R8>, 		0
	ruler	SD_None,	<ET_SD>, 		0
	ruler	I4I2_None,	<ET_I4,ET_I2>,		0
	ruler	I2I4_None,	<ET_I2,ET_I4>,		0
	ruler	R4I2_None,	<ET_R4,ET_I2>,		0
	ruler	I2SD_None,	<ET_I2,ET_SD>,		0
	ruler	2I2_None,	<ET_I2,ET_I2>,		0
	ruler	2SD_None,	<ET_SD,ET_SD>,		0
	ruler	I2R4_None,	<ET_I2,ET_R4>,		0
	ruler	SDI2_None,	<ET_SD,ET_I2>,		0
	ruler	SDI4_None,	<ET_SD,ET_I4>,		0
	ruler	SD2I4_None,	<ET_SD,ET_I4,ET_I4>,	0
	ruler	I42I2_None,	<ET_I4,ET_I2,ET_I2>,	0
	ruler	SD2I2_None,	<ET_SD,ET_I2,ET_I2>,	0
	ruler	SDI2SD_None,	<ET_SD,ET_I2,ET_SD>,	0
	ruler	4I2_None,	<ET_I2,ET_I2,ET_I2,ET_I2>,0
	ruler	4R4_None,	<ET_R4,ET_R4,ET_R4,ET_R4>,0
	ruler	SDI2SDI2_None,	<ET_SD,ET_I2,ET_SD,ET_I2>,0

	ruler	I2_I2,		<ET_I2>, 		ET_I2
	ruler	I2_I4,		<ET_I2>, 		ET_I4
	ruler	I2_R4,		<ET_I2>, 		ET_R4
	ruler	I2_SD,		<ET_I2>, 		ET_SD
	ruler	I4_I2,		<ET_I4>, 		ET_I2
	ruler	I4_I4,		<ET_I4>, 		ET_I4
	ruler	I4_SD,		<ET_I4>, 		ET_SD
	ruler	R4_R4,		<ET_R4>, 		ET_R4
	ruler	R4_SD,		<ET_R4>, 		ET_SD
	ruler	R8_SD,		<ET_R8>, 		ET_SD
	ruler	SD_I2,		<ET_SD>,		ET_I2
	ruler	SD_I4,		<ET_SD>,		ET_I4
	ruler	SD_R4,		<ET_SD>,		ET_R4
	ruler	SD_R8,		<ET_SD>,		ET_R8
	ruler	SD_SD,		<ET_SD>,		ET_SD

	ruler	I2I4_I2,	<ET_I2,ET_I4>,		ET_I2
	ruler	I2I2_SD,	<ET_I2,ET_I2>,		ET_SD
	ruler	I2I2_I2,	<ET_I2,ET_I2>,		ET_I2
	ruler	I2I2_I4,	<ET_I2,ET_I2>,		ET_I4
	ruler	SDSD_I2,	<ET_SD,ET_SD>,		ET_I2
	ruler	I2SD_SD,	<ET_I2,ET_SD>,		ET_SD
	ruler	SDI2_SD,	<ET_SD,ET_I2>,		ET_SD


	ruler	R4I2_R4,	<ET_R4,ET_I2>,		ET_R4
	ruler	3I2_I2, 	<ET_I2,ET_I2,ET_I2>,	ET_I2
	ruler	I2SDSD_I2,	<ET_I2,ET_SD,ET_SD>,	ET_I2
	ruler	SD2I2_SD,	<ET_SD,ET_I2,ET_I2>,	ET_SD



	ENDM

;First define the rule name constant and build a table of the word parameter
ruler	MACRO rname,ptCoerce,oTypOut
	;params are rule name, coercion table,	output type
	SSR_&rname	=	irule
	irule		=	irule + 1
Rule	=	0
	IRP	Arg,<ptCoerce>
Rule	=	Rule*10H+Arg
	ENDM
	dw	Rule
	ENDM

PUBLIC	tRuleWord
tRuleWord:
	MakRule

;Next build a table of the byte parameter
ruler	MACRO rname,ptCoerce,oTypOut
	;params are rule name, coercion table,	output type
	db	oTypOut
	ENDM

PUBLIC	tRuleByte
tRuleByte:
	MakRule

	.xlist
	include ssoprule.inc
	.list
sEnd	SCAN
	end
