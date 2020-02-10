TITLE	LSID - Contains functions for listing id related opcodes

;======================================================================
; Module: lsid.asm
;
; Purpose:
;	Contains functions for listing id related opcodes
;
;
;=======================================================================*/

	include		version.inc
	LSID_ASM = ON
	includeOnce architec
	includeOnce context
	includeOnce lister
	includeOnce lsint
	includeOnce names
	includeOnce opmin
	includeOnce pcode
	includeOnce prsorw
	includeOnce prstab
	includeOnce qblist
	includeOnce scanner
	includeOnce txtmgr
	includeOnce types
	includeOnce variable

	PERIOD_ID EQU ON		;record.element, not record->element

	assumes	DS,DGROUP
	assumes	SS,DGROUP
	assumes	ES,NOTHING

FV_ARYELEM EQU 1

sBegin	DATA

orwDecl DW	0			;temporary location used to
					;store the reserved word for the
					;the declaration stmt currently
					;being listed: AUTO,PUBLIC,STATIC,DIM



ET_MAX_NOFIELDS = ET_MaxStr
sEnd	DATA

sBegin	LIST
assumes	CS,LIST

; Code Segment Tables

;Table for mapping ET_xxx to DEFxxx reserved word
tRwDefType LABEL WORD
	DW	ORW_DEFINT		;res word for ET_I2
	DW	ORW_DEFLNG		;res word for ET_I4
	DW	ORW_DEFSNG		;res word for ET_R4
	DW	ORW_DEFDBL		;res word for ET_R8
	DW	ORW_DEFSTR		;res word for ET_SD



subttl	Beginning/End of line/statement opcodes

;------------------------------------------------------------------
;	Beginning/End of line/statement opcodes
;------------------------------------------------------------------

;===========================================================================
;			 List Rules
;
;	Each of these functions takes 0 or more nodes off oNodeRoot's list
;	and adds 0 or more nodes as indicated in its header comment.
;
;	On Entry, each of these routines can expect:
;		SI points to opcode's operand (if any)
;		DI points to node to be created (if any)
;		AX = opcode being listed
;		BX = 2 * opcode being listed
;
;===========================================================================

;	+-------------------------------------------+
;	| Beginning-of-line / End-of-text List Rules|
;	+-------------------------------------------+

;	[...] ==> [linenum ...] or [: label ...]
;
EatLabelDef PROC NEAR
	inc	si			;skip link field
	inc	si
	lods	WORD PTR es:[si]	;ax = label's oNam operand
	call	NewLabelONam		;Create either LNT_ONAM for label
					; or LNT_NUM for line number,
					; ax = offset to newly created node
	or	dl,dl
	jne	GotLineNum		;brif not an alphanumeric label
	call	PushRoot		;push label's oNam node
	mov	al,':'
	call	NewChar			;ax = offset to ":" node
GotLineNum:
	jmp	PushRoot		;push line number node or ':' 
EatLabelDef ENDP

; Emit n spaces. CX = emitted space count on return
Spc	PROC NEAR
	lods	WORD PTR es:[si]	;ax = space count
Spc	ENDP
	;fall into SpcAX
SpcAX	PROC NEAR
	push	ax			;save cSpaces
	call	NewSpaces		;ax = offset to spaces node
	pop	cx			;cx = space count
	jmp	PushRoot		;return to caller
SpcAX	ENDP

LrEotInclude:
	cmp	[fViewInclude],0
	jne	LrEot1			;brif INCLUDEd lines are visible
	dec	si
	dec	si			;si points to the BolInclude[Sp] opcode
	mov	[otxListNextInc],si	;save it for caller of ListLine
	push	es			;preserve seg addr of text table
	push	si
	call	OtxNoInclude		;ax = otx to opBol/opEot for next line
					; which has no $INCLUDE
	pop	es			;restore seg addr of text table
	xchg	si,ax			;si = otx to opBol/opEot for next line
					; which has no $INCLUDE
	jmp	Stage2Inc


ListRule LrEot
 	cmp	[fGotBol],0		
 	jne	LrEot1			;brif we've seen an opBol
 	mov	[ndLsCursor],UNDEFINED	;if called to list beyond eot, don't
 					; highlight it as current stmt
LrEot1:
	dec	si
	dec	si			;si = text offset to terminating opcode
J1_Stage2:
	jmp	Stage2

ListRule LrBolLab
	cmp	[fGotBol],0
	jne	LrEot1			;brif this is 2nd Bol opcode
	;fall into LrLab
ListRule LrLab
	call	EatLabelDef
	call	PushRootSpc		;terminate with a space
	jmp	SHORT LrBol2

ListRule LrBolLabSp
	cmp	[fGotBol],0
	jne	LrEot1			;brif this is 2nd Bol opcode
	;fall into LrLabSp
ListRule LrLabSp
	call	EatLabelDef
	call	Spc			;consume and handle space-count operand
	jmp	SHORT LrBol2

ListRule LrBolInclude
	cmp	[fGotBol],0
	jne	LrEotInclude		;brif this is 2nd Bol opcode
					; seen so far in this line
	lods	WORD PTR es:[si]	;ax = $INCLUDE nesting depth
	mov	[fLsIncluded],al	;tell ListLine's caller this is
	jmp	SHORT LrBol1		; an INCLUDEd line

ListRule LrBolIncludeSp
	cmp	[fGotBol],0
	jne	LrEotInclude		;brif this is 2nd Bol opcode
					; seen so far in this line
	lods	WORD PTR es:[si]	;ax = $INCLUDE nesting depth
	mov	[fLsIncluded],al	;tell ListLine's caller this is
					; an INCLUDEd line
	;fall into LrBolSp
ListRule LrBolSp
	cmp	[fGotBol],0
	jne	LrEot1			;brif this is 2nd Bol opcode
	call	Spc			;consume and handle space-count operand
	jmp	SHORT LrBol3		;cx = space count

ListRule LrBol
	cmp	[fGotBol],0
	jne	LrEot1			;brif this is 2nd Bol opcode
	mov	ax,[opList]		;ax = opcode (as saved by DispLs1)
;ax = opcode and high-bit operands
	.errnz	OPCODE_MASK - 3FFh
	shr	ah,1
	shr	ah,1			;ah = high-bit operand
	je	LrBol1			;brif no leading spaces
	.errnz	opBol
	xchg	ah,al			;ax = high-bit operand
	call	SpcAX
	jmp	SHORT LrBol3

ListRule LrEndProg

ListRule LrWatchExp
ListRule LrWatchStop
	cmp	[fGotBol],0
	jne	LrEot1			;brif this is 2nd Bol opcode
					; seen so far in this line
LrBol1:
	sub	al,al
	call	PushRootChar		;list null node, so Stg2Loop will
					; have something after ndLsCursor
					; in case this is the opcode identified
					; by otxLsCursorTmp
	;fall into LrBol2
LrBol2:
	mov	cl,LOW UNDEFINED	;no leading spaces on line
LrBol3:

;NOTE: the leading space count is used when entabbing leading spaces during
;	 ASCII save.  This won't work if cbLeading > 254, but who cares?

	mov	[fGotBol],cl		;remember we've seen the first BOL
					;also specifies number of leading spaces
	mov	[lsBolFlags],0		;reset beginning of line flags
LrBos1:
	mov	[lsBosFlagsWord],0	;reset beginning of stmt flags
					;sets lsBosFlags & lsBosFlags2
J1_Stg1Loop:
	jmp	Stg1Loop		;return to outer loop

ListRule LrBos
	mov	ax,' :'
	call	PushRootChars		;list ": "
	jmp	SHORT LrBos1

ListRule LrBosSp
	mov	al,':'
	call	PushRootChar
	lods	WORD PTR es:[si]	;ax = column operand
	call	NewCol1			;ax = "advance to column(ax)" node
	call	PushRoot		;list it
	jmp	SHORT LrBos1

subttl Expression related list rules

;------------------------------------------------------------------
;		Expression related list rules
;------------------------------------------------------------------

;***************************************************************************
; LrBinaryOp
; Purpose:
;	List a binary operator opcode like opAdd
;	[expRight expLeft] ==> [[expRight operator expLeft]]
;
;***************************************************************************
ListRule LrBinaryOp
	call	PopRootPushTemp		;move expRightNode from root to temp stk
	call	PushTempSpc		;surround operator with blanks
	call	PushTempOpChars		;list opcode's char(s)
	call	PushTempSpc		;surround operator with blanks
BinaryOp1:
	call	PopRootPushTemp		;move expLeftNode from root to temp stk
PUBLIC	PushRevListStg1
PushRevListStg1:
	call	PushRevList		;move temp stk to root in reverse order
	jmp	Stg1Loop		;return to outer loop

;***************************************************************************
; LrBinaryRw
; Purpose:
;	List a binary reserved-word operator opcode like opXor
;	[expRight expLeft] ==> [[expRight operator expLeft]]
;
; Algorithm:
;	opNode = NewRw(opcode)
;	expRightNode = PopRoot()
;	expLeftNode = PopRoot()
;	PushTemp(expLeftNode)
;	PushTemp(opNode)
;	PushTemp(expRightNode)
;	PushList()
;
;***************************************************************************
ListRule LrBinaryRw
	call	PopRootPushTemp		;move expRightNode from root to temp stk
	call	PushTempSpc		;surround operator with blanks
	call	PushTempOpRwSpc		;push res word node to temp stack
	jmp	SHORT BinaryOp1

;***************************************************************************
; LrLParen
; Purpose:
;	List the opcode (exp)opLParen
;	[exp] ==> [[")" exp "("]]
;
;***************************************************************************
ListRule LrLParen
	call	PushTempLParen		;push '(' onto temp stack
	call	PopRootPushTemp		;move expNode from root to temp stk
	call	PushTempRParen		;push ')' onto temp stack
	jmp	PushListStg1		;push temp list to root as 1 node

subttl Id related opcodes

;------------------------------------------------------------------
;			Id related opcodes
;------------------------------------------------------------------

PUBLIC PushTempONam
PushTempONam PROC NEAR
	call	NewONam 		;ax = offset to new ONam node
	jmp	PushTemp		;push node to temp stack
					; and return to caller
PushTempONam ENDP

;***************************************************************************
; PushTempId
; Purpose:
;	Consume an id opcode and push the following nodes to the temp stack:
;	(typeChar oNam)
;
; Entry:
;	si points to id's operand
;	di = offset to next free byte in bdNodes
;	FBOS_DoIdCommas is set if ", " is to be pushed to the root stack
;	   before this id is pushed to the temp stack
; Exit:
;	si points beyond id's operand
;	di is updated
;	typeChar oNam are pushed to the temp stack
;
;***************************************************************************
PushTempId PROC NEAR
	call	PushTempIdComma		;list ", " if we're in COMMON/SHARED
	call	NewId			;consume id's operand, ax = node
	call	PushTemp
PushTempId ENDP
	;fall into PushEt		;push explicit type (if any)
PushEt	PROC NEAR
.errnz	OPCODE_MASK - 3FFh
	mov	bl,BYTE PTR[opList+1]	;bl = high byte of original opcode
	shr	bl,1			
	shr	bl,1			;shift off low two bits
	jz	J1_Ret			;brif not an explicit type
	xor	bh,bh			;bx = ET_<type>
	mov	al,CS:tcEt[bx - 1]	;al = explicit type char
	call	PushTempChar		;push a char node (al) onto temp stack
	jmp	CharToCharTok		;convert it to a LNT_CHARS_TOK node
					;and return to caller
PushEt	ENDP

;***************************************************************************
; PushTempElem
;	Added as part of revision [14]
; Purpose:
;	Load the next word representing a record element
;	(which is an oNam if we're in rude or an oElem if we're not).
;	Push a "." and then a node containing the oNam of the element
;	onto the Temp stack.
;		[] ==> [ElementName "."]
; Entry:
;	[pIdLdOperand] is a pointer to the operand of the IdLd opcode
;	whose element is referenced.
;
;***************************************************************************
PushTempElem PROC NEAR
	mov	al,'.'
	call	PushTempChar		;push "." onto temp stack
	call	CharToCharTok		;convert it to a LNT_CHARS_TOK node
	lods	WORD PTR es:[si]	;ax = oNam or oElem
	cmp	[txdCur.TXD_scanState],SS_RUDE
	jae	PTEExit 		;brif table is in rude-edit state
	;ax is oElem parm to ONamOElem
	push	es			;preserve seg addr of text table
	cCall	ONamOElem,<ax>		; ax = oNam for element oElem
GotONam:
	pop	es			;restore seg addr of text table
PTEExit:
	call	PushTempONam		;push oNam
	jmp	SHORT PushEt		;push explicit type (if any)
					; and return to caller
PushTempElem ENDP

;***************************************************************************
; PushOffId
; Purpose:
;	Consume an id offset opcode's operand, and an id from the root
;	stack, and push the following nodes to the temp stack:
;	  [id ...]  ==>  [[typeChar oNam '.' id] ...]
; Entry:
;	mpOpLsArg[bx] = explicit type char (0 if none)
;	si points to opOffxxx's operand
;	di = offset to next free byte in bdNodes
; Exit:
;	si points beyond opcode's operand
;	di is updated
;
;***************************************************************************
PUBLIC	PushOffId
PushOffId PROC NEAR
	call	PopRootPushTemp		;move id node from Root to temp stack
	jmp	SHORT PushTempElem	;push "." and "elem_name"
PushOffId ENDP

;***************************************************************************
; PushTempAId
; Purpose:
;	push array id & indicies onto temp stack
;	  [[")" indexN "," ... index1 "(" typeChar oNam]]
; Entry:
;	root stack: [indexN ... index1 ...] ==>
;	temp stack: [...]
; Exit:
;	root stack: [...]
;	temp stack: [")" indexN "," ... index1 "(" typeChar oNam ...]
;
;***************************************************************************
NO_ARY_ARGS EQU 8000H

PushTempAId PROC NEAR
	call	PopAsClause		;ax = offset to [AS <type>] node
					; (0 if no AS <type> clause)
	push	ax			;list AS <type> later
	lods	WORD PTR es:[si]	;ax = count of indicies
	mov	[cLsArgs],al		;save count of indicies
	push	ax
	call	PushTempId		;consume oNam operand, push nam node
	pop	ax
.errnz	NO_ARY_ARGS - 8000H
	or	ax,ax
	js	NoAryArgs		;got an array like ERASE A
					; with no indecies
	call	PushTempLParen		;push '(' onto temp stack
	call	CharToCharTok		;convert it to a LNT_CHARS_TOK node
	call	PushCommaArgs		;copy cLsArgs from root to temp
					; and separate them by commas
	call	PushTempRParen		;push ')' onto temp stack
NoAryArgs:
	pop	ax			;ax = offset to [AS <type>] (if any)
	or	ax,ax
	je	J1_Ret			;brif no AS <type> clause  (return)
	call	PushTemp		;list after id(...)
J1_Ret:
	ret
PushTempAId ENDP

;
; push a comma onto temp stack if FBOS_DIM flag is set
; Added as part of revision [36]
PushTempIdCommaDim PROC NEAR
	test	lsBosFlags2,FBOS2_Dim
	je	NoIdComma		;brif not in DIM/AUTO etc.
	test	lsBosFlags,FBOS_NextIdComma
	je	FirstId			;brif first id in list, no leading comma
	call	PushTempCommaSpc	;output a ', '
FirstId:
	or	lsBosFlags,FBOS_NextIdComma
NoIdComma:
	ret
PushTempIdCommaDim ENDP

;***************************************************************************
; LrIdLdxxx
; Purpose:
;	List the id opcodes as follows:
;	opIdLdxxx:  [...] ==> [typeChar oNam ...]
;	opIdRfxxx:  [...] ==> [typeChar oNam ...]
; Entry:
;	mpOpLsArg[bx] = explicit type char (0 if none)
;            
;***************************************************************************
ListRule LrVtRf
	call	PushTempIdCommaDim	;list ',' if in DIM type statement
SkipCommaDim:
ListRule LrIdLd				
ListRule LrIdRf
ListRule LrIdRfTyp
	call	PushTempId		;PushTemp(<typeChar> <oNam>)
	call	PopAsClause		;ax = offset to [AS <type>] node
					; (0 if no AS <type> clause)
	or	ax,ax
	je	NoIdAsClause
	call	PushTemp		;list after id
NoIdAsClause:
PUBLIC	PushListStg1
;push temp list to root as 1 node and return to outer loop
PushListStg1:
	call	PushList		;move temp stk to root as 1 node
	jmp	Stg1Loop		;return to outer loop

ListRule LrIdSt
ListRule LrIdStTyp
	test	[lsBosFlags2],FBOS2_CONST
	je	NotInConst		;brif not listing CONST stmt
	test	[lsBosFlags2],FBOS2_CONST_COMMA
	je	Not1stConst		;brif not listing CONST stmt
	call	PushTempCommaSpc	;list ", "
Not1stConst:
	or	[lsBosFlags2],FBOS2_CONST_COMMA
NotInConst:
	call	PushTempId		;PushTemp(<typeChar> <oNam>)
ListAStType1:
	call	PushTempSpc		;surround '=' with spaces
	mov	ax,' ='
	call	PushTempChars		;push a char node (ax) onto temp stack
	call	PopRootPushTemp		;move expNode from root to temp stk
	jmp	SHORT PushListStg1	;push temp list to root as 1 node
					; and return to outer loop

ListRule LrOffALd
	call	PushTempOffAId
	jmp	SHORT PushListStg1
ListRule LrOffASt
	call	PushTempOffAId
	jmp	SHORT ListAStType1

;***************************************************************************
; PushTempOffAId
; Purpose:
;	List the array type element opcode
;	[indexN ... index1 id] ==>
;	  [[")" indexN "," ... index1 "(" typeChar oNam "." id]]
;
;***************************************************************************
PushTempOffAId PROC NEAR
	lods	WORD PTR es:[si]	;ax = cArgs
	mov	[cLsArgs],al
	call	PushTempElem		;next word is the oNam/oElem of field
					;  push "." and fieldname to temp stk
	call	PushTempLParen		;push '(' onto temp stack
	call	CharToCharTok		;convert it to a LNT_CHARS_TOK node
	call	PushCommaArgs		;copy cLsArgs from root to temp
					; and separate them by commas
	call	PushTempRParen		;push ')' onto temp stack
	call	PopRoot			;ax = node for record of which
					;  the array is an element
	push	ax			;save ptr to node
	call	PushList		;create single node for array ref
					;  of form ".id(1,2)"
	pop	ax			;recover node for record
	call	PushTemp		;push it onto Temp stack		
	call	PopRootPushTemp		;mov ".id(1,2)" to Temp stack
	ret
PushTempOffAId ENDP

;***************************************************************************
; LrAIdLd [48]
; Purpose:
;	[indexN ... index1] ==>
;	  [[")" indexN "," ... index1 "(" typeChar oNam]]
;
;***************************************************************************
ListRule LrAIdLd
	call	PushTempAId		;push array id & indicies onto temp stk
	jmp	PushListStg1		;push temp list to root as 1 node
					; and return to outer loop

;***************************************************************************
; LrAVtRf [48]
; Purpose:
;	[exp ", " exp ", ..., " exp ] ==>
;	  [[")" exp "," ... exp "(" typeChar oNam]]
;	StripOptBase called to convert every other "," to TO
;
;***************************************************************************
ListRule LrAVtRf			
	call	PushTempIdCommaDim	;list ',' if in DIM type statement
	call	PushTempAId		;push array id & indicies onto temp stk
	call	PushList		
	test	[lsBosFlags2],FBOS2_DIM	
	je	AVtRfNotDim		;brif not listing DIM/AUTO etc.			
	call	PopRoot			;ax = offset to node to be DIMed
	call	StripOptBase		;walk through list ax, eliminating
					; nodes created by opDimOptionBase
					; and converting ',' to TO.
					; ax still = offset to node to DIM
	call	PushRoot		;push array node to temp stack
AVtRfNotDim:				
	jmp	Stg1Loop		;					; and return to outer loop

;***************************************************************************
; LrAIdSt, LrAIdStTyp
; Purpose:
;	List the array id assignment opcodes
;	[indexN ... index1 exp ...] ==>
;	  [[exp = [")" indexN "," ... index1 "(" typeChar oNam]] ...]
;
;***************************************************************************
ListRule LrAIdSt
ListRule LrAIdStTyp
	call	PushTempAId		;push array id & indicies onto temp stk
	jmp	ListAStType1

;***************************************************************************
; LrOffLd, LrOffRf, LrOffLdTyp, LrOffRfTyp
; Purpose:
;	List the id opcodes as follows:
;	opIdOffLdxxx:  [id ...] ==> [[typeChar oNam '.' id] ...]
;	opIdOffRfxxx:  [id ...] ==> [[typeChar oNam '.' id] ...]
; Entry:
;	mpOpLsArg[bx] = explicit type char (0 if none)
;            
;***************************************************************************
ListRule LrOffLd
ListRule LrOffRf
ListRule LrOffLdTyp
ListRule LrOffRfTyp
	call	PushOffId
	jmp	PushListStg1		;push temp list to root as 1 node
					; and return to outer loop
	
;***************************************************************************
; LrOffSt, LrOffStTyp
; Purpose:
;	List the id opcodes as follows:
;	opOffStxxx:  [id exp ...] ==> [exp = [typeChar oNam '.' id] ...]
; Entry:
;	mpOpLsArg[bx] = explicit type char (0 if none)
;            
;***************************************************************************
ListRule LrOffSt
ListRule LrOffStTyp
	call	PushOffId
	jmp	SHORT ListAStType1

;Table for mapping ET_xxx to INTEGER, LONG, ..., STRING
tRwET LABEL WORD
	DW	ORW_ANY			;res word for ET_I2
	DW	ORW_INTEGER		;res word for ET_I2
	DW	ORW_LONG		;res word for ET_I4
	DW	ORW_SINGLE		;res word for ET_R4
	DW	ORW_DOUBLE		;res word for ET_R8
	DW	ORW_STRING		;res word for ET_SD
	DW	ORW_STRING		;res word for ET_TX

;***************************************************************************
; PushTempAsClause
;	rewritten for revision [11]
; Purpose:
;	Called for opAsType, opAsTypeExp, opAsType2 and for proc. parameters
;	to push a spaces node, "AS " and <type> to temp stack.
; Entry:
;	ax = column to advance to (0 if 1 space)
;	bx = if <= ET_MAX then
;		it is a predefined "ET_" type 
;	     or if high bit not set 
;		it is the oNam of a user defined type
;	     otw if high bit is set it is a command equivalent [EB]
;		
;***************************************************************************
PushTempAsClause PROC NEAR
	push	bx			;save Type
	or	ax,ax
	je	OneSpace
	call	NewCol1			;ax = "advance to column(ax)" node
	call	PushTemp		;list it
	jmp	SHORT OneSpace1

OneSpace:
	call	PushTempSpc		;list " "
OneSpace1:

	mov	ax,ORW_As
	call	PushTempRwSpc		;list "AS "
	pop	ax			;ax = Type
	cmp	ax,ET_MAX
	jbe	AsExplicitType		;brif AS INTEGER...STRING
	call	NewONam			;ax = offset to oNam's node
	jmp	SHORT FinishAsClause


AsExplicitType:
	xchg	bx,ax
	shl	bx,1			;bx = type * 2
	mov	ax,tRwET[bx]		;ax = ORW_ANY,ORW_INTEGER .. ORW_DOUBLE
	call	NewRw			;ax = offset to reserved word node
FinishAsClause:
	call	PushTemp
	ret
PushTempAsClause ENDP

;***************************************************************************
; PopAsClause
; Purpose:
;	See if PushTempAsClause has been called since the last PopAsClause,
;	If so, return with ax = offset to [ AS <type>] node.
;	Else, ax = 0
;
;***************************************************************************
PopAsClause PROC NEAR
	sub	ax,ax			;0 = default return value
	test	[lsBolFlags],FBOL_AsClause
	je	NoAryType		;brif not A(...) AS <type>
	and	[lsBolFlags],0FFH - FBOL_AsClause
	call	PopRoot			;ax = offset to [AS <type>] node
NoAryType:
	ret
PopAsClause ENDP


;	[id] ==> [[type AS id]]
;
ListRule LrAsTypeFixed
	lods	WORD PTR es:[si]	;ax = oTyp operand
	xchg	bx,ax			;bx = oTyp operand
	lods	WORD PTR es:[si]	;ax = cb or oNam
	push	ax			;preserve cb or oNam
	sal	bx,1			;carry = 1 means its an oNam
	pushf				;save carry to test later
	shr	bx,1			;restore bx = oTyp
	lods	WORD PTR es:[si]	;ax = column for AS
	call	PushTempAsClause	;push "AS " and <type>
	mov	al,' '
	call	PushTempChar		;push " " to temp stack
	mov	ax,' *'
	call	PushTempChars		;push "* " to temp stack
	popf				;restore flags word
	pop	ax			;restore ax = cb or oNam
	jc	ItsAnONam		;brif its an oNam
	push	si			;preserve text pointer
	xchg	si,ax			;si = string length constant
	mov	ax,LIT_LINENUM * 256 + 2;al = length, ah = constant type
	call	NewNum			;ax = offset to numeric constant node
	pop	si			;restore text pointer
	jmp	SHORT AT2PushTemp	
ItsAnONam:
	call	NewONam			;ax = new ONam node
AT2PushTemp:
	call	PushTemp		;push the new node
	jmp	SHORT EndAsClause

;	[id] ==> [[type AS id]]
;
ListRule LrAsTypeExp
ListRule LrAsType
	lods	WORD PTR es:[si]	;ax = oTyp operand
	xchg	bx,ax			;bx = oTyp operand
	lods	WORD PTR es:[si]	;ax = column for AS
	call	PushTempAsClause	;Push [ AS <type>] to temp stack
EndAsClause:
	or	[lsBolFlags],FBOL_AsClause;remember to call PopAsClause
	jmp	PushListStg1		;return to outer loop

;	[...]  =>  [id]
;
ListRule LrElemRef
	lods	WORD PTR es:[si]	;ax = id's oNam
	call	PushRootONam		;list id
	jmp	Stg1Loop		;return to outer loop

;	[...]  =>  [id()]
;
ListRule LrAElemRef
	inc	si			;until static arrays are allowed
	inc	si			; the index count is ignored
	lods	WORD PTR es:[si]	;ax = id's oNam
	call	NewONam 		;ax = offset to new ONam node
	call	PushTemp		;push oNam note onto temp stack
	mov	ax,')('	
	call	PushTempChars		;push "()" onto temp stack
	jmp	PushListStg1		;push temp list to root as 1 node

;	[...]  ==>  [<id> TYPE]
;
ListRule LrStType
	call	PushRootOpRwSpc		;list "TYPE "
	inc	si			;skip opStType's link operand
	inc	si
	lods	WORD PTR es:[si]	;ax = opStType's oNam operand
	call	PushRootONam		;list type's id
	jmp	Stg1Loop		;return to outer loop


subttl Declarative opcodes

;---------------------------------------------------------------------------
;			Declarative opcodes
;---------------------------------------------------------------------------

;---------------------------------------------------------------------------
; DIM related opcodes
;	Original ASCII Text:
;	  DIM x,a(x),b(y TO z)
;	pcode:
;	  opVtRfImp(x),opStDimScalar,
;	  opDimOptionBase,opIdLdImp(x),opAVtRfImp(2,a),opStDimTo,
;	  opIdLdImp(y),opIdLdImp(z),opAVtRfImp(2,b),opStDimTo
;
;---------------------------------------------------------------------------
ListRule LrDimOptionBase
	mov	al,'?'
	call	PushRootChar		;for now, just list a "?"
	jmp	Stg1Loop		;return to outer loop

; The stmt "DIM SHARED a,b" produces the pcode
; opShared, opIdRef(a), opStDimScalar, opIdRef(b), opStDimScalar
; When opShared is seen, it sets a flag telling opStDimScalar (or
; similar opcode) to list SHARED after DIM.
;
ListRule LrShared
	or	[lsBolFlags],FBOL_Shared
	jmp	Stg1Loop		;return to outer loop

; If opShared has been seen, push SHARED to temp stack
PushTempShared PROC NEAR		
	mov	bx,LISTOFFSET PushTempRwSpc 
	jmp	SHORT PushSharedCommon
PushTempShared ENDP

; If opShared has been seen, push SHARED to root stack
PushRootShared PROC NEAR		
	mov	bx,LISTOFFSET PushRootRwSpc 
PushSharedCommon:
	test	[lsBolFlags],FBOL_Shared
	je	NoShared
	and	[lsBolFlags],0FFH - FBOL_Shared
	mov	ax,ORW_SHARED
	jmp	bx			;push reserved word ax
NoShared:
	ret
PushRootShared ENDP


;***************************************************************************
; LrStReDimTo
; Purpose:
;	if 1st DIM opcode in this statement, list DIM, else list ','
;
;	[[")" expZ "," expY "," expX ... exp1 "(" typeChar oNam]] ==>
;	  [[")" expZ TO expY "," expX ... exp1 "(" typeChar oNam]]
;	opDimOptionBase gets listed as "?" internally, so this function
;	eliminates those nodes when found.
;
;***************************************************************************
ListRule LrStReDimTo
	mov	ax,ORW_REDIM		;ax = reserved word of stmt 
	call	PushTempRwOrComma	;push "," or RW of stmt
	call	PushTempShared		;list SHARED if opShared was seen
	call	PopRoot			;ax = offset to node to be DIMed
; handle the special case where an StReDimTo is being used in place
; of what used to be a StReDimScalar in QB4.0
	test	BYTE PTR[opList+1],HIGH(OPCODE_MASK+1) 
	jnz	GotRedimScalar		;brif if normal ReDimTo
	call	StripOptBase		;walk through list ax, eliminating
					; nodes created by opDimOptionBase
					; and converting ',' to TO.
					; ax still = offset to node to DIM
GotRedimScalar:
	call	PushTemp		;push array node to temp stack
	jmp	PushListStg1		;push temp list to root as 1 node
					; and return to outer loop

;***************************************************************************
; StripOptBase
; Purpose:
;	ax = offset to [[")" exp ", " exp ", " exp "(" typeChar oNam]]
;	walk through this list, eliminating "?" "," nodes (which
;	were created by opDimOptionBase), and converting every other
;	comma to " TO "
; Preserves: ax
;
;***************************************************************************
StripOptBase PROC NEAR
	push	ax
	push	si
	call	ListOffToPtr		;bx = ptr to node ax
	mov	ax,WORD PTR [bx.LN_val_list]
					;bx = offset to 1st node in list
					; This could be ")" or [AS <type>]
	call	ListOffToPtr		;bx = ptr to node ax
	cmp	BYTE PTR [bx.LN_type],LNT_CHAR
	je	StripLoop		;brif it is ")"
	call	ListSibPtr		;bx = sibbling(bx) ")"
StripLoop:
	call	ListSibPtr		;bx = sibbling(bx) (dim's UPPER index)
	call	ListSibPtr		;bx = sibbling(bx) (comma)
	mov	si,bx			;save ptr to it
	mov	WORD PTR [si.LN_val_char],'OT'	;convert comma to TO
	call	ListSibPtr		;bx = sibbling(bx) (dim's LOWER index
					; or "?" put there by opDimOptionBase)
	cmp	BYTE PTR [bx.LN_type],LNT_CHAR
	jne	NotDefault		;brif couldn't be "?"
	cmp	BYTE PTR [bx.LN_val_char],'?'
	jne	NotDefault		;brif not "?"
	sub	ax,ax			;ax = 0
	mov	WORD PTR [si.LN_val_char],ax	;convert comma node to NULL
	mov	WORD PTR [bx.LN_val_char],ax	; and LOWER index node to NULL
NotDefault:
	call	ListSibPtr		;bx = sibbling(bx) (comma or '(')
	cmp	BYTE PTR [bx.LN_val_char],','
	je	StripLoop		;brif ","
StripExit:				
	pop	si
	pop	ax			;ax = offset to node to be DIMed
	ret
StripOptBase ENDP

ListRule LrStErase
	call	PushTempOpRwSpc		;emit opcode's resword (ERASE)
	lods	WORD PTR es:[si]	;ax = opStErase's cnt operand
	mov	[cLsArgs],al
	call	PushCommaArgs		;copy cLsArgs from root to temp
					; and separate them by commas
	jmp	PushListStg1		;push temp list to root as 1 node
					; and return to outer loop

; The statement "COMMON SHARED /foo/ a,b" produces the pcode:
; opShared, opCommon(oNam(foo)), opIdVTRef(a), opIdVTRef(b)
;
ListRule LrStCommon
	call	PushRootOpRwSpc		;emit opcode's resword (COMMON)
	call	PushRootShared		;list SHARED if opShared seen
	inc	si			;skip cntEos operand
	inc	si
	lods	WORD PTR es:[si]	;ax = common block's oNam (FFFF if none)
	cmp	ax,UNDEFINED
	je	NotBlockCommon
	push	ax
	mov	al,'/'			;list "/"
	call	PushRootChar
	pop	ax
	call	PushRootONam		;list block's name
	mov	ax,' /'
	call	PushRootChars		;list "/ "
NotBlockCommon:
	or	[lsBosFlags],FBOS_DoIdCommas
					;every opIdVTRef in this statement
					; is to be preceeded by "," except
					; the first.
	jmp	Stg1Loop		;return to outer loop

; The statement "SHARED a,b" produces the pcode:
; opStShared opIdVTRef(a), opIdVTRef(b)
;
ListRule LrStStatic
ListRule LrStShared
	inc	si			;skip oText operand
	inc	si
	call	PushRootOpRwSpc		;emit opcode's resword (SHARED/STATIC)
	or	[lsBosFlags],FBOS_DoIdCommas
					;every opIdVTRef in this statement
					; is to be preceeded by "," except
					; the first.
	jmp	Stg1Loop		;return to outer loop

;	DIM, AUTO, PUBLIC, and EB STATIC
ListRule LrStoClassDecl
	inc	si			; skip oText operand
	inc	si			
	or	[lsBosFlags2],FBOS2_DIM 
	call	PushRootOpRwSpc		
	call	PushRootShared		;list SHARED if opShared seen
	jmp	Stg1Loop		;return to outer loop


;***************************************************************************
; LrStDefType
; Purpose:
;	List opStDefType opcode.  Opcode's operand is link field followed
;	by 32 bit mask/type as follows:
;	   high 26 bits, 1 bit for each letter A..Z
;	   low 6 bits = ET_xx
; Algorithm:
;	letterCur = 'A'-1
;	cLetters = 0
;	fNotFirst = FALSE
;	while mask != 0
;	   letterCur = letterCur + 1
;	   shift mask left 1 bit
;	   if carry is set
;	      if cLetters == 0
;	         if fNotFirst
;	            list ", "
;	         list letterCur
;	         fNotFirst = TRUE
;	      cLetters++
;	   else
;	      if cLetters > 0
;	         if cLetters > 1
;	            list "-"
;	            list letterCur-1
;		 cLetters = 0
;
; Register allocation:
;	letterCur = si
;	cLetters = cl
;	mask = dx:ax
;	fNotFirst = ch
;
;***************************************************************************
ListRule LrStDefType
	inc	si			;skip link field
	inc	si
	lods	WORD PTR es:[si]	;ax = low 16 bits of operand
	xchg	ax,dx			;dx = low 16 bits of operand
	lods	WORD PTR es:[si]	;ax = high 16 bits of operand
	xchg	ax,dx			;[dx:ax] = 32 bit operand
	mov	bl,al			;bl = type
	and	ax,0FFC0H		;ax = bit mask for last 10 letters
	push	si			;save si for duration of routine

	push	ax			;save low 16 bits of operand
	push	dx			;save high 16 bits of operand
	and	bx,03FH			;bx = type
	shl	bx,1			;bx = type * 2
	mov	ax,tRwDefType - 2[bx]	;ax = ORW_DEFINT .. ORW_DEFSTR
	call	PushRootRwSpc		;list DEFINT..DEFSTR

	mov	si,'A'-1		;si = letterCur = 'A'-1
	sub	cx,cx			;cLetters = 0, fNotFirst = 0
DefTypeLoop:
	pop	dx			;dx = bit mask for first 16 letters
	pop	ax			;ax = bit mask for last 10 letters
	mov	bx,ax			;test mask
	or	bl,cl			;don't exit if we're within a range
					; (like A-Z), so we can terminate it
	or	bx,dx			;test high word of mask as well
	je	EndDefType		;brif mask is 0
	inc	si			;letterCur = letterCur + 1
	shl	ax,1			;shift mask left 1 bit
	rcl	dx,1
	push	ax			;save mask on stack
	push	dx			; (gets popped by DefTypeLoop)
	push	cx			;save cLetters, fNotFirst
	jnc	NotThisLetter		;brif this letter is not set
	or	cl,cl
	jne	BumpCLetters		;brif we're already in a range
					; (ie we're at B in an A-Z range)
	or	ch,ch
	je	Not1stLetter		;brif this is the 1st letter output
	call	PushRootCommaSpc	;list ", "
Not1stLetter:
	mov	ax,si			;al = letterCur
	call	PushRootChar		;list letterCur
BumpCLetters:
	pop	cx			;restore cLetters, fNotFirst
	inc	cx			;cLetters++ (inc cl is bigger opcode)
	mov	ch,1			;fNotFirst = FALSE
	jmp	SHORT DefTypeLoop

NotThisLetter:
	or	cl,cl
	je	NotWithinRange		;brif cLetters = 0
	dec	cl
	je	NotWithinRange		;brif cLetters was 1
	mov	al,'-'
	call	PushRootChar		;list "-"
	mov	ax,si			;al = letterCur
	dec	al			;al = letterCur - 1
	call	PushRootChar
NotWithinRange:
	pop	cx
	sub	cl,cl			;cLetters = 0
	jmp	SHORT DefTypeLoop	;set fNotFirst = TRUE

EndDefType:
	pop	si			;restore si=text pointer
	jmp	Stg1Loop		;return to outer loop

subttl	Procedure related opcodes

;------------------------------------------------------------------
;		Procedure related opcodes
;------------------------------------------------------------------

tcEt LABEL BYTE
	.erre	ET_I2 EQ 1		
	DB	'%'			;ET_I2
	.erre	ET_I4 EQ 2		
	DB	'&'			;ET_I4
	.erre	ET_R4 EQ 3		
	DB	'!'			;ET_R4
	DB	'#'			;ET_R8
	db	'$'			;ET_SD

;***************************************************************************
; ListProc
; Purpose:
;	List a [DECLARE] SUB/FUNCTION/DEF [QB4] statement
; Entry:
;	ax = cnt operand
;	es:si points to oPrs operand
;	lsBosFlags2 & FBOS2_DECLARE is non-zero for DECLARE
;
;***************************************************************************
DbPub	ListProc
cProc	ListProc,<NEAR>
	localW	parmFlags
	localW	cbAlias
	localB	procType
	localW	procAtr				
	procAtr_LO EQU BYTE PTR (procAtr)
	procAtr_HI EQU BYTE PTR (procAtr+1)
cBegin
	add	ax,si			;ax -> beyond end of opcode's operands
	push	ax			;save till function exit
	lods	WORD PTR es:[si]	;ax = oPrs operand (oNam if opStDefFn
					; and we're in SS_RUDE scan state)

	;--------------------------------------------------------\
	;NOTE: Temp data is on stack until end of this block
	;      Don't branch into or out of this block
	push	es			;save es (restored after FieldsOfPrsFar)
	push	ax			;pass oPrs to FieldsOfPrsFar

	lods	WORD PTR es:[si]	;ax = procAtr operand
	mov	[procAtr],ax		;save procAtr
	.errnz	DCLA_procType - 0300h
	and	ah,3			;ah = procType
	mov	[procType],ah		;save for later
	xchg	dx,ax			;dh = procType

	mov	ax,ORW_SUB
	cmp	dh,PT_SUB
	je	LpGotOPrs		;brif we're in a SUB
	mov	ax,ORW_FUNCTION
	cmp	dh,PT_FUNCTION
	je	LpGotOPrs		;brif we're in a FUNCTION
	DbAssertRelB dh,e,PT_DEFFN,LIST,<LrStDeclare has invalid proc type>
	mov	ax,ORW_DEF
	cmp	[txdCur.TXD_scanState],SS_RUDE
	jb	LpGotOPrs		;brif not in SS_RUDE
	;opStDefFn has oNam as operand, not oPrs in RUDE state
	call	PushRootRwSpc		;push DEF to root stack
	pop	ax			;ax = oNam
	jmp	SHORT LpDefFn

LpGotOPrs:
	call	PushRootRwSpc		;push SUB/FUNCTION/DEF to root stack

					;parm to FieldsOfPrsFar pushed shortly
					; after entry to ListProc
	call	FieldsOfPrsFar		;ax = oNam of prs
;ax = oNam for procedure
LpDefFn:
	pop	es			;restore es = seg adr of txt table
	;NOTE: Temp data is now off stack
	;--------------------------------------------------------/

	call	PushRootONam		;list sub/func/def's name
					;high byte contains proc type
	mov	al,[procAtr_LO]
	.errnz	DCLA_Explicit - 0080h
	or	al,al
	jns	ImplicitTyp
	and	al,DCLA_oTyp		;al = explicit type
	DbAssertRelB al,ne,0,LIST,<ListProc: invalid explicit oTyp1>
	DbAssertRelB al,be,ET_MAX,LIST,<ListProc: invalid explicit oTyp2>
	mov	bx,LISTOFFSET tcEt - 1	;bx points to tcEt mapping table
	xlat	cs:[bx]			;al = explicit type char (%,&,etc.)
	call	PushRootChar		;list it
	call	CharToCharTok		;convert it to a LNT_CHARS_TOK node
ImplicitTyp:
	call	PushRootSpc
	test	[procAtr_HI],DCLA_cdecl / 100h
	.errnz	DCLA_cdecl - 8000h
	je	NotCDECL		;brif not declared as CDECL
	mov	ax,ORW_CDECL
	call	PushRootRwSpc		;list "CDECL "
NotCDECL:
	lods	WORD PTR es:[si]	;ax = parm cnt operand
	mov	[cLsArgs],al		;save parm count

	;List ALIAS "string" or LIB "string"
	mov	cl,[procAtr_HI]
	.errnz	DCLA_cbAlias - 7C00h
	and	cx,DCLA_cbAlias / 100h	;cx = cbAlias * 4
	shr	cl,1
	shr	cl,1			;cx = cbAlias
	jcxz	NoAliasOrLib		;brif alias clause not specified
	push	si			;save si points to 1st arg
	push	cx			;save byte count of ALIAS/LIB id

	; set si to point to ALIAS/LIB name
	mov	al,[cLsArgs]
	inc	al			;map 0 and UNDEFINED to 0
	je	NoParms			; brif cLsArgs was UNDEFINED
	dec	al			; restore dx = cLsArgs
NoParms:
	sub	ah,ah			;ax = cLsArgs
	mov	dx,ax			;dx = cLsArgs
	shl	ax,1			;ax = cLsArgs * 2
	add	ax,dx			;ax = cLsArgs * 3
	shl	ax,1			;ax = cLsArgs * 6
	add	si,ax			;si points to ALIAS or LIB name

	mov	ax,ORW_ALIAS
	call	PushRootRwSpc		;list ALIAS
	pop	ax			;restore ax = cbAlias
PushQStr:				
	call	PushRootQstr		
	call	PushRootSpc		;list " "
NoAlias:
	pop	si			;restore si points to 1st arg

NoAliasOrLib:
	cmp	[cLsArgs],0
	jg	GotProcParms		;brif cParms != UNDEFINED and != 0
					; (for DECLARE, UNDEFINED means 0
					;  parms, and no type checking - see
					;  pcode document)
	jl	NoTypeChk		;brif cParms == UNDEFINED
	test	[lsBosFlags2],FBOS2_DECLARE
	je	NoTypeChk		;brif not listing DECLARE
EnforceNoParms:
	mov	ax,')('
	call	PushRootChars		;list "() "
	call	PushRootSpc
NoTypeChk:
	jmp	NoProcParms

GotProcParms:
	call	PushRootLParen		;push '('
ProcParmLoop:
	cmp	si,[otxLsCursorTmp]
	jb	NotNdLsCursor		;brif not token of interest
	mov	[ndLsCursor],di
	mov	[otxLsCursorTmp],UNDEFINED ;make sure we don't set it again
NotNdLsCursor:
	test	[lsBosFlags2],FBOS2_DECLARE
	je	NotDeclParm		;brif not listing DECLARE
	lods	WORD PTR es:[si]	;ax = parm's oNam
	call	NewONam			;ax = offset to oNam node
					;bx = oNam
	jmp	SHORT ChkPrmFlgs
NotDeclParm:
	call	NewId			;consume id's operand, ax = node
					;bx = oNam
ChkPrmFlgs:
	push	bx			;saveüÿNam
	push	ax			;save id's node
	lods	WORD PTR es:[si]	;ax = flags operand (PATR_xxx)
	mov	[parmFlags],ax

	TestX	ax,PATR_SEG		;Is Seg attribute present?
	mov	ax,ORW_SEG		;Assume yes
	jne	@F			;Brif yes
	TestM	[parmFlags],PATR_BYVAL	;Is ByVal attribute present?
	jz	NotSegParm		;Brif not
	mov	ax,ORW_BYVAL
@@:
	call	PushRootRwSpc		;list ByVal or Seg
NotSegParm:
	pop	ax			;ax = offset to id node
	call	PushRoot		;list it
	pop	bx			;bx = parm's oNam
					;NOTE: we need to preserve this in
					; bx until OTypOfONamDefault is called
	lods	WORD PTR es:[si]	;ax = formal parm's oTyp operand
	test	[parmFlags],PATR_asClause
	jne	AsParm			;brif parm was AS <type>
	DbAssertRel ax,be,ET_MAX,LIST,<LrStDeclare has invalid oTyp>
	dec	ax			;map ET_I2 to 0
	test	[parmFlags],PATR_explicit
	jne	NotImpl			;brif explicit type char (%&!#$)
	jmp	SHORT NoExpType

NotImpl:
	mov	bx,LISTOFFSET tcEt	;map (0,1,2,...) to (%,&,!,...)
	xlat	cs:[bx]			;al = explicit type char (%,&,etc.)
	call	PushRootChar		;list it
	call	CharToCharTok		;convert it to a LNT_CHARS_TOK node
	jmp	SHORT NoExpType

AsParm:
	test	[parmFlags],PATR_array
	je	NotAryParm
	push	ax			;save oTyp
	mov	ax,')('
	call	PushRootChars		;list "()"
	call	CharToCharTok		;convert it to a LNT_CHARS_TOK node
	pop	ax			;restore ax=oTyp
;ax = oTyp for AS <type> clause
NotAryParm:
	cmp	ax,ET_MAX
	jbe	NotUserType
	cmp	[txdCur.TXD_scanState],SS_RUDE
	jae	NotUserType		;brif ax=oNam of user type
	push	es			;preserve seg addr of text table
	cCall	ONamOTyp,<ax>		; ax = oNam for oTyp ax
	pop	es			;restore seg addr of text table
NotUserType:
	xchg	bx,ax			;bx = oNam
	sub	ax,ax
DoAsClause:
	call	PushTempAsClause	;Push [<type> AS] to temp stack
CallPushList:
	call	PushList		;Push AsClause to root as 1 node
	jmp	SHORT DecArgs

NoExpType:
	test	[parmFlags],PATR_array
	je	DecArgs
	mov	ax,')('
	call	PushRootChars		;list "()"
	call	CharToCharTok		;convert it to a LNT_CHARS_TOK node
DecArgs:
	dec	[cLsArgs]
	je	EndProcParms
	call	PushRootCommaSpc	;separate parms with ", "
	call	GrowBdNodes		;grow list buffer if necessary
	je	ListProcExit		;brif out-of-memory - We'll abort
					; ListLine next time through Stg1Loop
	jmp	ProcParmLoop

EndProcParms:
	call	PushRootRParen		;push ')'
	call	PushRootSpc		;list " "
NoProcParms:

NoAsClause:
	;list STATIC if not declare and prsCur.fStatic is TRUE
	test	[lsBosFlags2],FBOS2_DECLARE
	jne	ListProcExit		;brif listing DECLARE
	cmp	[procType],PT_DEFFN
	je	ListProcExit		;brif listing DEF FN
	test	[prsCur.PRS_flags],FP_STATIC
	je	ListProcExit		;brif not a STATIC proc
	mov	ax,ORW_STATIC
ListAuto:
	call	PushRootRw		
ListProcExit:
	pop	si			;si points beyond end of opcode
cEnd	;ListProc

; pcode = opStSub/opStFunction(cntEos, oPrs, cnt, cnt*(id,flags,oTyp))
;	[...] ==> [) id , id ... ( id SUB/FUNCTION]
;
ListRule LrStDefFn
	lods	WORD PTR es:[si]	;ax = cnt operand
	inc	si			;skip link field
	inc	si
	dec	ax			;don't include link field in count
	dec	ax
	jmp	SHORT DoListProc

ListRule LrStDeclare
	or	[lsBosFlags2],FBOS2_DECLARE
					;tell ListProc its doing a DECLARE
	jmp	SHORT ListDeclare

ListDeclare:
	mov	ax,ORW_DECLARE
	call	PushRootRwSpc		;push DECLARE to temp stack
ListRule LrStSub
ListRule LrStFunction
	lods	WORD PTR es:[si]	;ax = cnt operand
DoListProc:
	call	ListProc
	jmp	Stg1Loop		;return to outer loop

ListRule LrStCallLess
	or	[lsBosFlags],FBOS_CallLess
	jmp	SHORT CallId

ListRule LrStCallS
ListRule LrStCall
	call	PushTempOpRwSpc		;list "CALL[S] "
CallId:
	lods	WORD PTR es:[si]	;ax = count of args
	mov	[cLsArgs],al		;save count of args
	lods	WORD PTR es:[si]	;ax = oPrs to call
	push	es			;save es = seg adr of txt table
	push	ax			;pass oPrs
	call	FieldsOfPrsFar		;ax = proc's oNam
	pop	es			;restore es = seg adr of txt table
	call	PushTempONam		;list proc's name
	cmp	[cLsArgs],0
	je	NoCallArgs
ListArgsNoParen:
	mov	al,' '
	test	[lsBosFlags],FBOS_CallLess
	jne	CallLess1		;brif no CALL keyword
					;don't list ( for CallLess
ListArgsParen:
	mov	al,'('
CallLess1:
	call	PushTempChar		;push char al onto temp stack
	call	PushCommaArgs		;copy cLsArgs from root to temp
					; and separate them by commas
	test	[lsBosFlags],FBOS_CallLess
	jnz	CallLess2		;brif no CALL keyword
					;don't list ( for CallLess
					;don't list ) for CallLess
	call	PushTempRParen		;push ')' onto temp stack
CallLess2:
NoCallArgs:
	call	PushList		;push temp list to root as 1 node
	jmp	Stg1Loop		;return to outer loop


;	[exp defargs]  ==>  [exp = defargs]
;
ListRule LrEndSingleDef
	add	si,4			;skip filler field operand
					; and link field operand
	mov	ax,' ='			;list "= "
	call	PushTempChars
	call	PopRootPushTemp		;move exp from root to temp stack
	jmp	PushListStg1		;push temp list to root as 1 node
					; and return to outer loop

ListRule LrStExitProc
	inc	si			;consume opcode's operand
	inc	si
ListRule LrStEndProc
	call	PushRootOpRwSpc		;emit opcode's resword (EXIT/END)
	mov	ax,ORW_DEF

	cmp	grs.GRS_oPrsCur,UNDEFINED
	je	GotEndSub		;brif we're in a DEF FN
	mov	ax,ORW_SUB
	cmp	[prsCur.PRS_procType],PT_SUB
	je	GotEndSub		;brif we're in a SUB
	mov	ax,ORW_FUNCTION		;else it has to be a FUNCTION
GotEndSub:
	call	PushRootRw		;list SUB, FUNCTION or DEF
	jmp	Stg1Loop		;return to outer loop

ListRule LrStConst
	or	[lsBosFlags2],FBOS2_CONST	;so LrIdSt outputs commas
	jmp	LrRwSpc




sEnd	LIST

end
