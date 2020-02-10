	TITLE	LSRULES - functions which map opcodes to 'list-node' structs

;======================================================================
; Module: LsRules.asm
;
; Purpose:
;	Contains functions which map opcodes to their equivalent
;	'list-node' structures.  See lsmain.asm for general comments.
;
;
;=======================================================================*/

	include version.inc
	LSRULES_ASM = ON
	includeOnce architec
	includeOnce context
	includeOnce heap		
	includeOnce lister
	includeOnce lsint
	includeOnce opmin
	includeOnce pcode
	includeOnce prsorw
	includeOnce qblist
	includeOnce rtps
	includeOnce scanner

	assumes	CS,LIST
	assumes	DS,DGROUP
	assumes	SS,DGROUP
	assumes	ES,NOTHING

sBegin	DATA
PUBLIC		psdLsIncl
psdLsIncl	dw 0		;pointer to buffer filled by Lr_Include
sEnd	DATA

sBegin	LIST
assumes	CS,LIST


subttl	Literal opcode listers

;------------------------------------------------------------------
;		Literal opcode listers
;------------------------------------------------------------------

; List rule for opcode which encodes I2 literal in high bits of opcode
ListRule LrLitI2
	mov	ax,[opList]		;ax = opcode + high bit operand
	.erre	OPCODE_MASK EQ 03ffh	; Assure SHR/SHR is correct
	mov	al,ah			;al = high-bit operand * 4
	shr	al,1			
	shr	al,1			;al = high-bit operand
	cbw				;ax = high-bit operand
	push	si			;preserve si
	xchg	si,ax			;si = literal number
	mov	ax,LIT_LINENUM*256+2	;ah = LIT_LINENUM, al = 2 (bytes)
	call	NewNum			;ax points to new number node(ax)
	pop	si			;restore si
	jmp	SHORT PushRootStg1	

ListRule LrLitNum
	mov	ax,[mpOpLsArg + bx]	;opcode's argument
	xchg	ah,al
	;al = constant value size (2, 4, or 8)
	;ah = constant type 
	;     (LIT_D2, LIT_O2, LIT_H2,
	;      LIT_D4, LIT_O4, LIT_H4,
	;      LIT_R4, LIT_R8)
	
	call	NewNum			;ax points to new number node(ax)
PushRootStg1:
	call	PushRoot
J0_Stg1Loop:
	jmp	Stg1Loop		;return to outer loop



;	[...] ==> [[" string "] ...]
;	Note:	can't be [" string "] because each expression term must
;		be 1 root node.
;
ListRule LrLitSD
	lods	WORD PTR es:[si]	;ax = cbText
	call	PushRootQStr		;push '"' str_node '"' to root stack
	jmp	Stg1Loop		;push temp list to root as 1 node
					;  and return to outer loop


subttl Remark related list rules

;------------------------------------------------------------------
;		Remark related list rules
;------------------------------------------------------------------

;***************************************************************************
; LrStRem
; Purpose:
;	List the opcode opStRem(cbText, text)
;	[] ==> [text REM]
;
;***************************************************************************
ListRule LrStRem
	call	PushRootOpRw		;push "REM" node to root's stack
LrStRem1:
	lods	WORD PTR es:[si]	;ax = cbText
	or	ax,ax
	je	AtLeast1Spc		;brif cbText = 0
	call	NewEnStr		;ax = offset to new node
	jmp	PushRootStg1		;push node ax to root stack
					; and return to outer loop

;so opStRem(0)op_Static will list as REM $STATIC and not REM$STATIC
;
AtLeast1Spc:
PushRootSpcStg1:
	call	PushRootSpc
	jmp	SHORT J0_Stg1Loop	;return to outer loop

str255Include	DB 11,'$INCLUDE',58d,32d,39d	; $INCLUDE: '
str255Static	DB 7,'$STATIC'
str255Dynamic	DB 8,'$DYNAMIC'

ListRule Lr_Static
	mov	[fLsDynArrays],0	;set static flag for AsciiSave
	mov	ax,LISTOFFSET str255Static
Lr_Static1:
	call	NewCsStr
	call	PushRoot
	jmp	SHORT LrStRem1

ListRule Lr_Dynamic
	mov	[fLsDynArrays],1		;set static flag for AsciiSave
	mov	ax,LISTOFFSET str255Dynamic
	jmp	SHORT Lr_Static1


; List opcode op_Include, which is generated for syntax: $INCLUDE: 'filename'
; If the global variable psdLsIncl is non-zero, copy include filename
; to psdLsIncl->pb and set psdLsIncl->cb.
;
ListRule Lr_Include
	mov	ax,LISTOFFSET str255Include
	call	NewCsStr		;ax = node for ($INCLUDE ')
	call	PushRoot
	lods	WORD PTR es:[si]	;ax = cbText
	cmp	[psdLsIncl],NULL
	je	NoSdLsIncl

;es = segment of text table
;si = offset into text table to string
;ax = length of string (including terminating 0)
	push	si
	push	di

	push	ax
	mov	di,si			;es:di points to string
	mov	cx,-1
	mov	al,27H			;look for terminating '
	repne	scasb
	not	cx			;cx = length including '
	dec	cx			;cx = filename length
	pop	ax

	mov	di,[psdLsIncl]		;di points to destination sd
	mov	[di.SD_cb],cx		;save length of string
	mov	di,[di.SD_pb]		;di points to destination buffer

	push	es
	push	ds
	pop	es			;es = DGROUP
	pop	ds			;ds = text table's segment
	assumes	DS,NOTHING
	rep movsb			;copy string to psdLsIncl's buffer
	push	es
	push	ds
	pop	es			;es = text table's segment
	pop	ds			;ds = DGROUP
	assumes	DS,DGROUP

	pop	di
	pop	si
;si = offset into text table to string
;ax = length of string to push
NoSdLsIncl:
	call	PushString		;ax = node for consumed string operand
	jmp	SHORT J1_Stg1Loop	;return to outer loop

ListRule LrQuoteRem
	lods	WORD PTR es:[si]	;ax = cbText (including column field)
	dec	ax			;don't count column field
	dec	ax
	push	ax			;save it
	lods	WORD PTR es:[si]	;ax = column field
	call	NewCol			;ax = "advance to column(ax)" node
	call	PushRoot		;list it
	mov	al,39			;al = ASCII code for single quote '
	call	PushRootChar		;list '
	pop	ax			;restore ax = size of string
	call	NewEnStr		;ax = offset to new string node
	jmp	PushRootStg1		;push node ax to root stack
					; and return to outer loop

PushString2 PROC NEAR
	dec	ax			;don't count link field
	dec	ax
	inc	si			;skip link field
	inc	si
PushString2 ENDP
	;fall into PushString
PushString PROC NEAR
	call	NewStr			;ax = offset to new node
	jmp	PushRoot		;make it new root of tree
					;return to caller
PushString ENDP

ListRule LrStData
	call	PushRootOpRw		;list DATA
	lods	WORD PTR es:[si]	;ax = cbText (including link field)
	push	ax			;save length
	dec	ax			;don't count 0-terminator
	call	PushString2		;ax = node for consumed string operand
	pop	ax
	and	ax,1			;ax = 1 if string was odd length
	shl	ax,1			;ax = 2 if string was odd length
	add	si,ax			;si points beyond 0-terminator
	jmp	SHORT J1_Stg1Loop	

ListRule LrReParse
	lods	WORD PTR es:[si]	;ax = cbText (including link field)
PushString2Stg1:
	call	PushString2		;ax = node for consumed string operand
J1_Stg1Loop:				
	jmp	Stg1Loop		;return to outer loop

;List rule for SQL source lines. Special processing is needed for
;setting colLsCursor in case of error occuring within the SQL statement.


subttl	Control Flow Opcodes

;------------------------------------------------------------------
;			Control Flow Opcodes
;------------------------------------------------------------------

;	[...] ==> [space ELSE space ...] if single line ELSE
;	[...] ==> [ELSE ...] if block ELSE
;
ListRule LrStElse
	inc	si			;skip link field
	inc	si
ListRule LrStElseNop
	mov	[lsBosFlagsWord],0	;reset beginning of stmt flags
	test	[lsBolFlags],FBOL_GotIf
	jne	GotSingleElse		;brif we've seen an IF opcode
	jmp	LrRwSpc			;just list the ELSE
GotSingleElse:
	; If listing ELSE after :<spaces>, we don't have to emit a space
	; before listing the ELSE reserved word, opBosSp already did.
	
	mov	bx,di			
	add	bx,[bdNodes.BD_pb]	; convert offset to ptr 
	cmp	[bx + LN_type - CBLNT_CHAR],LNT_COL 
	je	NoSpc			; brif opBosSp was just listed
	call	PushRootSpc		;emit blank before reserved word
NoSpc:					
	call	PushRootOpRwSpc		;push opcode's reserved word
	jmp	SHORT J1_Stg1Loop	;return to outer loop

;	[...] ==> [END space <opcode's resword> ...]
;
ListRule LrStEndDef
	inc	si			;skip filler field operand
	inc	si
ListRule LrStEndType
	inc	si			;skip link field operand
	inc	si
ListRule LrStEndIfBlock
ListRule LrStEndSelect
	mov	ax,ORW_END
	call	PushRootRwSpc
	jmp	LrRw			;list TYPE, IF, SELECT
					; and return to outer loop

;	[...] ==> [EXIT space <opcode's resword> ...]
;
ListRule LrStExitDo
ListRule LrStExitFor
	inc	si			;consume oText operand
	inc	si
	mov	ax,ORW_EXIT
	call	PushRootRwSpc
	jmp	LrRw			;list DO or FOR
					; and return to outer loop

;	[exp ...] ==> [[THEN space exp space IF/ELSEIF] ...]
;
IfThen	PROC NEAR
	or	[lsBolFlags],FBOL_GotIf	;set static flag for LrStElse
	call	PushTempOpRwSpc		;push IF/ELSEIF onto temp stack
	call	PopRootPushTemp		;move expNode from root to temp stk
	call	PushTempSpc		;emit blank before THEN
	mov	ax,ORW_THEN
	call	PushTempRwSpc		;push THEN
	call	PushList		;move temp stk to root as 1 node
	ret
IfThen	ENDP

ListRule LrNoList3
	inc	si			;skip operand
	inc	si
ListRule LrNoList2
	inc	si			;skip operand
	inc	si
ListRule LrNoList1
Skip1Stg1:
	inc	si			;skip link field
	inc	si
ListRule LrNoType
ListRule LrNoList
	jmp	SHORT J2_Stg1Loop		;return to outer loop


;	[exp ...] ==> [space [THEN space exp space IF] ...]
;
ListRule LrStIfBlock
ListRule LrStElseIf
ListRule LrStIf
	call	IfThen			;push [[THEN space exp space IF]]
	jmp	SHORT Skip1Stg1		;skip operand
					;return to outer loop


;	[exp ...] ==> [label space [THEN space exp space IF] ...]
;
;	[...] ==> [oNamLabel ...]
;
ListRule LrStIfLab
ListRule LrStIfLabDirect
	call	IfThen			;push [[THEN space exp space IF]]
ListRule LrStElseLab
ListRule LrStElseLabDirect
PushRootLabelStg1:
	call	PushRootLabel		;consume and push <label>
	jmp	SHORT J2_Stg1Loop	;return to outer loop

;	[exp ...] ==> [label space [GOTO space exp space IF] ...]
;
ListRule LrStIfGotoLab
	or	[lsBolFlags],FBOL_GotIf	;set static flag for LrStElse
	call	PushTempOpRwSpc		;push IF onto temp stack
	call	PopRootPushTemp		;move expNode from root to temp stk
	call	PushTempSpc		;emit blank before THEN
	mov	ax,ORW_GOTO
	call	PushTempRw
	call	PushList		;move temp stk to root as 1 node
	call	PushRootSpc
	jmp	SHORT LrStElseLab	;consume and push <label> and
					; return to outer loop

;	[exp ...] ==> [<opcode's resword> space exp ...]
;
ListRule LrEvStop
ListRule LrEvOn
ListRule LrEvOff
	call	PushRootSpc		;emit blank before opcode's res word
	jmp	LrRw			;list opcode's reserved word
					; and return to outer loop

;***************************************************************************
; LrEvGosub
; Purpose:
;	List the opcode opEvGosub(label), for example:
;	opLit1 opEvSignal1 opEvGosub(label)  ==> ON SIGNAL(1) GOSUB label
;	[exp ...]  ==>  [label space [GOSUB space exp space ON] ...]
;
;***************************************************************************
ListRule LrEvGosub
	mov	ax,ORW_ON
	call	PushTempRwSpc		;push ON
	call	PopRootPushTemp		;move expNode from root to temp stk
	call	PushTempSpc		;emit blank before THEN
	call	PushTempOpRw		;push GOSUB onto temp stack
	call	PushList		;move temp stk to root as 1 node
	call	PushRootSpc		;emit blank before label's name
	jmp	short PushRootModLabelStg1	;consume and push <label>
					; and return to outer loop

;	[...]  ==>  [label space GOSUB/GOTO/RESTORE/RESUME/RETURN]
;
ListRule LrRwLabel
ListRule LrStGosub
ListRule LrStGosubDirect
ListRule LrStGoto
ListRule LrStGotoDirect
ListRule LrStReturn1
	call	PushRootOpRwSpc		;push opcode's resword
	jmp	PushRootLabelStg1	;consume and push <label>
					; and return to outer loop

ListRule LrStRunLabel
ListRule LrStRestore1
	call	PushRootOpRwSpc		;push opcode's resword
	jmp	short PushRootModLabelStg1	;consume and push <label>
					; and return to outer loop


; If operand is UNDEFINED, list RESUME 0
; else list RESUME label
;
ListRule LrStResume
	cmp	WORD PTR es:[si],UNDEFINED
	jne	LrRwLabel		;brif not RESUME 0
	call	PushRootOpRwSpc		;list "RESUME "
Goto0:
	inc	si			;skip operand
	inc	si
	mov	al,'0'
	call	PushRootChar
J2_Stg1Loop:
	jmp	Stg1Loop		;return to outer loop

;	[...]  ==>  [label space GOSUB space ERROR space ON]
;
ListRule LrStOnError
	mov	ax,ORW_ON
	call	PushRootRwSpc		;push ON
	call	PushRootOpRwSpc		;push ERROR
	mov	ax,ORW_GOTO
	call	PushRootRwSpc		;push GOTO
PushRootModLabelStg1:
	cmp	WORD PTR es:[si],UNDEFINED
	je	Goto0			;brif ON ERROR GOTO 0
	call	NewModLabel		;ax = module level label node
	call	PushRoot
	jmp	SHORT J2_Stg1Loop	;return to outer loop


;	[exp] ==> [[label, ..., label GOSUB/GOTO exp ON]]
;
ListRule LrStOnGosub
ListRule LrStOnGoto
	mov	ax,ORW_ON
	call	PushTempRwSpc		;push ON
	call	PopRootPushTemp		;move exp from root to temp stack
	call	PushTempSpc		;list space between exp and GOTO/GOSUB
	call	PushTempOpRwSpc		;push GOTO/GOSUB
	lods	WORD PTR es:[si]	;ax = byte count of label args
	shr	ax,1			;ax = count of label args
	mov	[cLsArgs],al		;setup for call to PushCommaArgs
					; (guarenteed by parser to be > 0)
	xchg	cx,ax			;cx = count of label args
EmitLabLoop:
	push	cx			;save count of label args
	call	PushRootLabel		;consume and push <label>
	call	GrowBdNodes		;grow list buffer if necessary
					; preserves cx
	pop	cx			;restore count of label args
	je	OnGosubExit		;brif out-of-memory - We'll abort
					; ListLine next time through Stg1Loop
	loop	EmitLabLoop		;repeat for each label in list
	call	PushCommaArgs		;move labels from root to temp
					; stack, inserting commas
OnGosubExit:
	jmp	PushListStg1		;push temp list to root as 1 node
					; and return to outer loop

;	[exp ...]  ==>  [[exp space CASE space SELECT] ...]
;
ListRule LrStSelectCase
	call	PushTempOpRwSpc		;push SELECT onto temp stack
	mov	ax,ORW_CASE
	call	PushTempRwSpc		;push ON
	call	PopRootPushTemp		;move expNode from root to temp stk
	inc	si			;skip operand
	inc	si
	jmp	PushListStg1		;push temp list to root as 1 node
					; and return to outer loop

;***************************************************************************
; PushCaseOrComma
; Purpose:
;	1st opCase in a statement gets mapped to CASE,
;	all subsequent ones get mapped to ','.
;
;***************************************************************************
PushCaseOrComma PROC NEAR
	mov	ax,ORW_CASE
	jmp	PushTempRwOrComma
PushCaseOrComma ENDP

;	[exp2 exp1 ...]  ==>  [[exp2 space TO space exp1 space CASE]]
;
ListRule LrStCaseTo
	call	PushCaseOrComma		;emit CASE space or ',' space
	call	PopRoot			;ax = exp2
	push	ax			;save it for later
	call	PopRootPushTemp		;move exp1 from root to temp stk
	call	PushTempSpc
	call	PushTempOpRwSpc		;list opcode's resword (TO)
	pop	ax			;ax = exp1
	call	PushTemp
	jmp	PushListStg1		;push temp list to root as 1 node
					; and return to outer loop

;	[...]  ==>  [ELSE space CASE ...]
;
ListRule LrStCaseElse
	call	PushRootOpRwSpc		;list opcode's resword (CASE)
	mov	ax,ORW_ELSE
PushRootRwStg1:
	call	PushRootRw		;list ELSE
	jmp	Stg1Loop		;return to outer loop

;	[exp ...]  ==>  [[exp space CASE ...]]
;
ListRule LrStCase
	call	PushCaseOrComma		;emit CASE space or ',' space
	call	PopRootPushTemp		;move expNode from root to temp stk
	jmp	PushListStg1		;push temp list to root as 1 node
					; and return to outer loop

;	[exp ...]  ==>  [[exp space <relation> space IS space CASE]]
;
ListRule LrStCaseRel
	call	PushCaseOrComma		;emit "CASE " space or ", " to temp stk
	mov	ax,ORW_IS
	call	PushTempRwSpc		;list "IS "
	call	PushTempOpChars		;list opcode's char(s)
	call	PushTempSpc		;surround operator with blanks
	call	PopRootPushTemp		;move expNode from root to temp stk
	jmp	PushListStg1		;push temp list to root as 1 node
					; and return to outer loop

	
;	[exp ...]  ==>  [[exp space WHILE/UNTIL space DO]]
;
ListRule LrStDoWhile
ListRule LrStDoUntil
	mov	ax,ORW_DO
	call	PushTempRw		;list DO
LrStLoop1:
	call	PushTempSpc
	call	PushTempOpRwSpc		;list WHILE or UNTIL
	call	PopRootPushTemp		;move expNode from root to temp stk
	inc	si			;skip oText operand
	inc	si
	jmp	PushListStg1		;push temp list to root as 1 node
					; and return to outer loop

;	[exp ...]  ==>  [[exp space WHILE/UNTIL space LOOP]]
;
ListRule LrStLoopUntil
ListRule LrStLoopWhile
	mov	ax,ORW_LOOP
	call	PushTempRw		;list LOOP
	jmp	SHORT LrStLoop1		;share code with DO

;	[exp3 exp2 exp1 id ...]  ==>  [[exp3 STEP exp2 TO exp1 = id FOR]]
;
ListRule LrStForStep
	call	PopRootPushTemp		;move exp3 from root to temp stk
	call	PushTempSpc
	mov	ax,ORW_STEP
	call	PushTempRwSpc		;list STEP
	;fall into LrStFor
;	[exp2 exp1 id ...]  ==>  [[exp2 TO exp1 = id FOR]]
;
ListRule LrStFor
	add	si,4			;skip oBP, oText operands
	call	PopRootPushTemp		;move exp2 from root to temp stk
	call	PushTempSpc
	mov	ax,ORW_TO
	call	PushTempRwSpc		;list TO
	call	PopRootPushTemp		;move exp1 from root to temp stk
	call	PushTempSpc
	mov	ax,'= '
	call	PushTempChars		;list ' ='
	call	PopRootPushTemp		;move id node from root to temp stk
	call	PushTempSpc
	call	PushTempOpRw		;list FOR
	jmp	PushRevListStg1		;move temp stk to root in reverse order
					; and return to outer loop

;  [...]  ==>  [NEXT ...]
;
ListRule LrStNext
	cmp	[txdCur.TXD_scanState],SS_EXECUTE
	jne	NotExecute
	call	PopRoot			;discard offset to id node (it was
					; synthetically generated by scanner,
					; will be removed by SsDescan()
NotExecute:
	add	si,4			;skip oBP and oText operands
	jmp	short LrRw		;list NEXT
					; and return to outer loop

;  [id ...]  ==>  [id NEXT ...]
;
ListRule LrStNextId			;advance past oBP, oText operands
	inc	si			;skip oBP operand
	inc	si
ListRule LrStWhile			;advance past oText operand
	inc	si			;skip oText operand
	inc	si
ListRule LrStRead
	call	PushTempOpRwOrComma	;push opcode's reserved word (or comma)
					;followed by a space
	jmp	short PopPushPushListStg1	;list exp and return to Stg1Loop

;  [...]  ==>  [LOOP/WEND ...]
;
ListRule LrStLoop
ListRule LrStWend
	inc	si			;skip link field
	inc	si
	jmp	short LrRw		;list LOOP or WEND
					; and return to outer loop

;	[...]  ==>  [NEXT space RESUME ...]
;
ListRule LrStResumeNext
	call	PushRootOpRwSpc		;list RESUME
	mov	ax,ORW_NEXT
	jmp	PushRootRwStg1		;list NEXT
					;return to outer loop

subttl	Generic opcode listers

;------------------------------------------------------------------
;			Generic opcode listers
;------------------------------------------------------------------

;***************************************************************************
; LrRw
; Purpose:
;	[...]  ==>  [resWord ...]
;
;***************************************************************************
ListRule LrRw
	call	PushRootOpRw		;push opcode's reserved word
	jmp	Stg1Loop		;return to outer loop

;	[...]  ==>  [space <opcode's resword> ...]
;
ListRule LrRwSpc
	call	PushRootOpRwSpc		;push opcode's reserved word
	jmp	Stg1Loop		;return to outer loop

;***************************************************************************
; LrChar
; Purpose:
;	[...]  ==>  [char ...]
;
;***************************************************************************
ListRule LrChar
	mov	ax,[mpOpLsArg + bx]	;ax = char(s) to be listed
PushRootCharsStg1:
	call	PushRootChars		;push char(s) to be listed to root stack
	jmp	Stg1Loop		;return to outer loop

;***************************************************************************
; LrRwExp1
; Purpose:
;	[exp]  ==>  [[exp space resWord]]
;	Examples include opStChain, opStRandomize1, opNot
;
;***************************************************************************
ListRule LrRwExp1
	call	PushTempOpRwSpc		;push opcode's reserved word
					;followed by a space
PopPushPushListStg1:
	call	PopRootPushTemp		;move expNode from root to temp stk
	jmp	PushListStg1		;push temp list to root as 1 node
					; and return to outer loop

;***************************************************************************
; LrUnaryChar
; Purpose:
;	[exp]  ==>  [[exp char]]
;
;***************************************************************************
ListRule LrUnaryChar
ListRule LrUnaryOp
	call	PushTempOpChars
	jmp	SHORT PopPushPushListStg1 ;list exp and return to outer loop

;	[exp2 exp1 ...]  ==>
;	 [exp2 space comma exp1 space comma <opcode's resword>]
;
ListRule LrStSwap
	inc	si			;consume opStSwap's operand
	inc	si
ListRule LrRwExp2
	mov	[cLsArgs],2
ListStmtArgs:
	call	PushTempOpRwSpc		;push opcode's reserved word
	call	PushCommaArgs		;copy cLsArgs from root to temp
					; and separate them by commas
	jmp	PushListStg1		;push temp list to root as 1 node
					; and return to outer loop

;	[exp3 exp2 exp1 ...]  ==>
;	 [exp3 space comma exp2 space comma exp1 space comma <opcode's resword>]
;
ListRule LrRwExp3
	mov	[cLsArgs],3
	jmp	SHORT ListStmtArgs

;***************************************************************************
; LrFunc1Arg
; Purpose:
;	[exp]  ==>  [[")" exp "(" resWord]]
;
;***************************************************************************
ListRule LrFnLen
ListRule LrFnVarptr_
	inc	si			;skip size operand
	inc	si
ListRule LrFunc1Arg
	mov	[cLsArgs],1
ListFuncArgs:
	call	PushTempOpRw		;push opcode's reserved word
ListFuncArgs2:
	call	PushTempLParen		;push '(' onto temp stack
	call	PushCommaArgs		;copy cLsArgs from root to temp
					; and separate them by commas
	call	PushTempRParen		;push ')' onto temp stack
	jmp	PushListStg1		;push temp list to root as 1 node
					; and return to outer loop

ListRule LrFunc2Args
	mov	[cLsArgs],2
	jmp	SHORT ListFuncArgs

ListRule LrFunc3Args
	mov	[cLsArgs],3
	jmp	SHORT ListFuncArgs



mptypRwCoerce	equ	$-2		
	dw	ORW_CInt		; I2
	dw	ORW_CLng		; I4
	    dw	    ORW_CSng		; R4
	dw	ORW_CDbl		; R8


ListRule LrCoerce			
	mov	[cLsArgs],1
	mov	bl,byte ptr [opList+1]	; BL = ET type * 4 + garbage
	.erre	OPCODE_MASK EQ 03ffh	; Assure SHR is correct
;	and	bx,HIGH (NOT OPCODE_MASK)
	and	bx,0FCh 		
	shr	bx,1			; BX = ET type * 2
	mov	ax,word ptr mptypRwCoerce[bx]	; Translate to reserved word
	call	PushTempRw		; Push opcode's reserved word
	jmp	short ListFuncArgs2	


	;Start of [22]


	;End of [22]



subttl	Misc opcode listers

;------------------------------------------------------------------
;			Misc opcode listers
;------------------------------------------------------------------

ListRule LrScanStop
	DbHalt	LIST,<Illegal opcode found in ListLine>

;Emitted to indicate defaulted parm - lists as nothing
ListRule LrUndef
	mov	ax,100h
	SKIP2_PSW
ListRule LrNull
	sub	ax,ax
	jmp	PushRootCharsStg1	;list null
					;return to outer loop



tOrwKey LABEL WORD
	DW	ORW_OFF
	DW	ORW_ON
	DW	ORW_LIST

ListRule LrStKey
	call	PushRootOpRwSpc		;list KEY
	lods	WORD PTR es:[si]	;ax = 0,1,2 for ON,OFF,LIST
	shl	ax,1
	xchg	bx,ax			;bx = index into tOrwKey
	mov	ax,[tOrwKey+bx]		;ax = ON,OFF,LIST
	jmp	PushRootRwStg1		;list it
					;return to outer loop


PushDefSeg PROC NEAR
	call	PushRootOpRwSpc		;list DEF
	mov	ax,ORW_SEG
	jmp	PushRootRwSpc		;list SEG
					; and return to caller
PushDefSeg ENDP

ListRule LrStDefSeg1
	call	PopRootPushTemp
	call	PushDefSeg		;list "DEF SEG "
	mov	ax,' ='
	call	PushRootChars		;list "= "
	jmp	PushListStg1		;push temp list to root as 1 node
					; and return to outer loop

ListRule LrStDefSeg0
	call	PushDefSeg		;list "DEF SEG "
J3_Stg1Loop:
	jmp	Stg1Loop		;return to outer loop


ListRule LrStOptionBase0
ListRule LrStOptionBase1
	mov	ax,ORW_OPTION
	call	PushRootRwSpc		;list OPTION
	mov	ax,ORW_BASE
	call	PushRootRwSpc		;list BASE
	call	PushRootOpChars		;list opcode's char ('0' or '1')
	jmp	Stg1Loop		;return to outer loop

;***************************************************************************
; LrStWidthLprint
; Purpose:
;	[exp]  ==>  [[exp LPRINT WIDTH]]
;
;***************************************************************************
ListRule LrStWidthLprint
	call	PushTempOpRwSpc		;list WIDTH
	mov	ax,ORW_LPRINT
	jmp	SHORT Palette1		;list LPRINT exp, return to outer loop

ListRule LrStPaletteUsing
	call	PushTempOpRwSpc		;list PALETTE
	mov	ax,ORW_USING
Palette1:
	call	PushTempRwSpc		;list USING
Palette2:
	jmp	PopPushPushListStg1	;list exp and return to outer loop


;	[exp]  ==>  [[exp = DATE$/TIME$]]
;
ListRule LrStDate_
ListRule LrStTime_
	call	PushTempOpRwSpc		;list DATE$/TIME$
	mov	ax,' ='
	call	PushTempChars		;list '= '
	jmp	PopPushPushListStg1	;list exp and return to outer loop


;***************************************************************************
; LrStLocate
; Purpose:
;	List a statement like CLEAR or LOCATE which takes a variable number
;	of optional arguments.
;	[exp, ..., exp]  ==>  [[exp, ... LOCATE]]
;	For example, if the stmt was SCREEN ,,x,y
;	    the pcode would be:
;	       opLit0 opLit0 opIdLd(x) opLit1 opIdLd(y) opLit1 opStScreen
;	    and the root stack on entry would have:
;	       [1 y 1 x 0 0]
;	    On exit, the root stack would have:
;	       [y "," x "," "," LOCATE]
;
;***************************************************************************
ListRule LrStLocate
ListRule LrStScreen
ListRule LrStColor
ListRule LrStClear
	call	PushTempOpRwSpc		;push opcode's resword
	lods	WORD PTR es:[si]	;ax = number of arguments
	mov	[cLsArgs],al
	call	PushCommaArgs		;copy cLsArgs from root to temp
					; and separate them by commas
	jmp	PushListStg1		;move temp stk to root
					; and return to outer loop
ListRule LrStClose
	lods	WORD PTR es:[si]	;ax = number of arguments
	mov	[cLsArgs],al
	jmp	ListStmtArgs		;pop statement args and list with ", "

;	[exp var]  ==>  [[exp = var LSET/RSET]]
;
ListRule LrStRset
ListRule LrStLset
	call	PushTempOpRwSpc		;push opcode's resword (LSET/RSET)
	call	PopRootPushTemp		;move var node from root to temp stk
	mov	ax,'= '
	call	PushTempChars		;list ' ='
	call	PushTempSpc
	call	PopRootPushTemp		;move expNode from root to temp stk
	jmp	PushListStg1		;move temp stk to root as 1 node
					; and return to outer loop

;	[exp4 exp3 exp2 lval]  ==>  [exp4 = (exp3, exp2, lval) MID$]
;	[exp3 exp2 lval]  ==>  [exp3 = (exp2, lval) MID$]
;
ListRule LrStMid_2
	mov	[cLsArgs],1
	jmp	SHORT MidCont
ListRule LrStMid_3
	mov	[cLsArgs],2
MidCont:
	call	PopRoot			;ax = lval node
	xchg	cx,ax			;cx = lval node
	call	PopRoot			;ax = exp4 node
	push	ax			;save exp4 node
	push	cx			;save lval node
	call	PushTempOpRw		;list "MID$"
	call	PushTempLParen		;push '(' onto temp stack
	pop	ax			;ax = lval node
	call	PushTemp		;list lval node
	call	PushTempCommaSpc	;list ", "
	call	PushCommaArgs		;copy cLsArgs from root to temp
					; and separate them by commas
	call	PushTempRParen		;push ')' onto temp stack
	call	PushTempSpc
	mov	ax,' ='
	call	PushTempChars		;list '= '
	pop	ax			;ax = exp4 node
	call	PushTemp		;push it to temp stack	
	jmp	PushListStg1		;move temp stk to root as 1 node
					; and return to outer loop

;	[exp2 exp1]  ==>  [[exp2 AS exp1 NAME]]
;
ListRule LrStName
	call	PopRootPushTemp		;move exp2 node from root to temp stk
	call	PushTempSpc
	mov	ax,ORW_AS
	call	PushTempRwSpc		;list AS
	call	PopRootPushTemp		;move var node from root to temp stk
	call	PushTempSpc
	call	PushTempOpRw		;push opcode's resword
	jmp	PushRevListStg1		;move temp stk to root in reverse order
					; and return to outer loop



;These constants reside in rtps.inc
;FINP_QSupress	EQU 1	;set if "prompt" was followed by a comma,
;			;not a semicolon, indicating "? " is not to be
;			;output after prompt.
;FINP_CrLf	EQU 2	;set if INPUT was followed by optional ";",
;			;meaning CrLf is not to be output when user
;			;presses return.
;FINP_Prompt	EQU 4	;set if the optional SDPrompt argument is included.


;   [ id optionalPromptExp chan]  ==>  [ id optionalPromptExp chan INPUT LINE]
;
;   [ id optionalPromptExp]  ==>  [ id optionalPromptExp INPUT LINE]
;
;   [ optionalPromptExp ]  ==>  [ optionalPromptExp INPUT ]
;
ListRule LrStLineInput
	call	PopRoot			;ax = offset to id node
	push	ax			;save it
	mov	ax,ORW_LINE
	call	PushTempRwSpc		;list "LINE "
	lods	WORD PTR es:[si]	;ax = FINP_xxx mask
	mov	cl,al			;al = FINP_xxx mask
	call	DoInputPrompt		;list "INPUT ..."
	test	[lsBosFlags],FBOS_Channel
	je	NoChan1			;brif not LINE INPUT #n,
	call	PopPushCommaSpc		;move #chan to temp stack
					;list ", "
NoChan1:
	pop	ax			;ax = offset to id node
	call	PushTemp		;list id node
	jmp	PushListStg1		;push temp list to root as 1 node
					; and return to outer loop

ListRule LrInputPrompt
	lods	WORD PTR es:[si]	;ax = byte count of opcode's operand
	inc	ax			;round up to even byte count
	and	al,0FEH
	mov	cl,BYTE PTR es:[si]	;cl = FINP_xxx mask
	add	si,ax			;advance si beyond opInputPrompt
	call	DoInputPrompt
	jmp	PushListStg1		;push temp list to root as 1 node
					; and return to outer loop

;cl = FINP_xxx mask
DoInputPrompt PROC NEAR
	call	PushTempOpRwOrComma	;list "INPUT ", and set flag so
					; opStInput lists as ", "
	or	[lsBosFlags],FBOS_InputPrompt
					;tell LrStInput not to list INPUT
	test	cl,FINP_CrLf
	je	No_CrLf			;brif INPUT not followed by 
	mov	al,';'
	call	PushTempCharSpc		;list "; "
No_CrLf:				;brif INPUT not followed by 
	test	cl,FINP_Prompt
	je	No_Prompt		;brif INPUT not followed by "prompt"
	call	PopRootPushTemp		;move "prompt" from temp to root stack
	mov	al,';'
	test	cl,FINP_QSupress
	je	No_QSupress		;brif prompt not followed by ,
	mov	al,','
No_QSupress:
	call	PushTempCharSpc		;list "; " or ", "
No_Prompt:
	ret
DoInputPrompt ENDP

;	[exp chan]  ==>  [exp ,chan# INPUT]
;	[exp]  ==>  [exp INPUT]
;	[exp]  ==>  [exp ,]
;
ListRule LrStInput
	test	[lsBosFlags],FBOS_InputPrompt
	jne	NoInpPrompt		;brif opInputPrompt has already
					; been scanned in this stmt
	call	PopRoot			;ax = offset to exp node
	push	ax
	call	PushTempOpRwOrComma	;list "INPUT " or ", "
	test	[lsBosFlags],FBOS_Channel
	je	NoChan			;brif not INPUT #n,
	and	[lsBosFlags],0FFH - FBOS_Channel
	call	PopPushCommaSpc		;move #chan to temp stack
					;list ", "
NoChan:
	pop	ax			;ax = offset to exp node
	call	PushTemp		;list it
	jmp	PushListStg1		;push temp list to root
					; and return to outer loop

NoInpPrompt:
	and	[lsBosFlags],0FFH - FBOS_InputPrompt
	jmp	Stg1Loop


;	[expLast exp1st expFileNum] ==>
;	 [expLast TO expFirst , expFileNum LOCK/UNLOCK]
;	or if LOCK_1stToLast bit is not set in operand:
;	[expFileNum] ==> [expFileNum LOCK/UNLOCK]
;
ListRule LrStLock
ListRule LrStUnLock
	lods	WORD PTR es:[si]	;ax = operand for opStLock/opStUnlock
.errnz	LOCK_1stToLast AND 0FF00H	;if error, test ah
	test	al,LOCK_1stToLast	
	je	LockAll			;brif no 1st/last arg
	xchg	cx,ax			;cx = operand
	test	ch,LOCK_DefLastArg/256
	jne	DefLastArg		;brif last arg was defaulted
	call	PopRoot			;ax = offset to expLast node
.errnz	LOCK_DefLastArg AND 0FFH	;if error, test cl
	call	PushTemp		;push expLast to temp stack
	call	PushTempSpc
	mov	ax,ORW_TO
	call	PushTempRw		;list TO
DefLastArg:
	call	PopRoot			;ax = offset to exp1st node
.errnz	LOCK_Def1stArg AND 0FFH		;if error, test cl
	test	ch,LOCK_Def1stArg/256
	jne	Def1stArg		;brif 1st arg was defaulted
	push	ax			;save exp1st
	call	PushTempSpc		;list a SPACE between TO and exp1st
	pop	ax			;ax = exp1st
	call	PushTemp		;push exp1st to temp stack
Def1stArg:
	call	PushTempCommaSpc	;list ", "
LockAll:
	call	PopRootPushTemp		;move filenum from root to temp stk
	call	PushTempSpc
	call	PushTempOpRw		;list LOCK/UNLOCK
	jmp	PushRevListStg1		;move temp stk to root in reverse order
					; and return to outer loop

;	[exp3 exp2 exp1] ==> [exp3 , exp2 , exp1 OPEN]
;
ListRule LrStOpenOld3
	mov	al,3			;list 3 expressions separated by ", "
	jmp	SHORT ListExps

;	[exp4 exp3 exp2 exp1] ==> [exp4, exp3, exp2, exp1 OPEN]
;
ListRule LrStOpenOld4
	mov	al,4
ListExps:
	mov	[cLsArgs],al		;setup for call to PushCommaArgs
	call	PushTempOpRwSpc		;list OPEN
	call	PushCommaArgs		;list open arguments
	jmp	PushListStg1		;push temp list to root as 1 node
					; and return to outer loop

MASK_INPUT	EQU 01H
MASK_OUTPUT	EQU 02H
MASK_APPEND	EQU 04H
MASK_RANDOM	EQU 08H
MASK_BINARY	EQU 10H
MASK_FOR	EQU 20H
tRwMode:
	DW  ORW_INPUT, ORW_OUTPUT, ORW_APPEND, ORW_RANDOM, ORW_BINARY, ORW_FOR

MASK_WRITE	EQU 01H
MASK_READ	EQU 02H
MASK_SHARED	EQU 04H
MASK_LOCK	EQU 08H
MASK_ACCESS	EQU 10H
tRwAccess:
	DW	ORW_WRITE, ORW_READ, ORW_SHARED, ORW_LOCK, ORW_ACCESS

;This table says, if <mode> == MD_SQI, list FOR INPUT,
;                 if <mode> == MD_SQO, list FOR OUTPUT, etc.
tMaskMode:
	DB	MD_SQI, MASK_INPUT  + MASK_FOR
	DB	MD_SQO, MASK_OUTPUT + MASK_FOR
	DB	MD_APP, MASK_APPEND + MASK_FOR
	DB	MD_RND, MASK_RANDOM + MASK_FOR
	DB	MD_BIN, MASK_BINARY + MASK_FOR
	DB	0	;end of list

;This table says, if <access> == ACCESS_BOTH, list READ WRITE, etc.
tMaskAccess:
	DB	ACCESS_READ, MASK_ACCESS + MASK_READ
	DB	ACCESS_WRITE, MASK_ACCESS + MASK_WRITE
	DB	ACCESS_BOTH, MASK_ACCESS + MASK_WRITE + MASK_READ
	DB	0	;end of list

;This table says, if <lock> == LOCK_BOTH, list LOCK READ WRITE, etc.
tMaskLock:
	DB	LOCK_READ, MASK_READ + MASK_LOCK
	DB	LOCK_WRITE, MASK_WRITE + MASK_LOCK
	DB	LOCK_BOTH, MASK_WRITE + MASK_READ + MASK_LOCK
	DB	LOCK_SHARED, MASK_SHARED
	DB	0	;end of list

;*************************************************************************
; OutRwMask
; Purpose:
;	Output 1 or more reserved words based on a mask value and tables
; Entry:
;	al = value to search for
;	bx = offset to ORW_xxx table (in cs:)
;	dx = offset to mask table (in cs:)
;
;*************************************************************************
OutRwMask PROC NEAR
	push	si			;save caller's si
	push	cx			;save caller's cx
	mov	cl,al			;cl = mode we're looking for
	mov	si,dx			;si points to mask table (in cs)
OpnMdLoop:
	lods	BYTE PTR cs:[si]	;al = comparison value
	or	al,al
	je	OpnMdExit		;brif end of table
	cmp	al,cl
	lods	BYTE PTR cs:[si]	;al = mask of res words to output
	jne	OpnMdLoop		;brif al is not value of interest (cl)
	mov	cl,al			;cl = mask of res words to output
	mov	si,bx			;si points to ORW_xxx table (in cs)
OpnRwLoop:
	or	cl,cl
	je	OpnMdExit		;brif end of reserved word mask
	lods	WORD PTR cs:[si]	;ax = ORW_xxx to be listed
	shr	cl,1
	jnc	OpnRwLoop		;brif this res word not to be listed
	call	PushTempRwSpc		;list it
	jmp	SHORT OpnRwLoop

OpnMdExit:
	pop	cx
	pop	si
	ret
OutRwMask ENDP

;	[exp3 exp2 exp1] ==> [exp3 = LEN, exp2 AS <mode> FOR exp1 OPEN]
;
ListRule LrStOpen3
	call	PopRootPushTemp		;move exp3 node from root to temp stk
	mov	ax,' ='
	call	PushTempChars		;list "= "
	call	PushTempSpc
	mov	ax,ORW_LEN
	call	PushTempRwSpc		;list "LEN "
	;fall into LrStOpen2
;	[exp2 exp1] ==> [exp2 AS <mode> FOR exp1 OPEN]
;
ListRule LrStOpen2
	call	PopRootPushTemp		;move exp2 node from root to temp stk
	call	PushTempSpc		;list " "
	mov	ax,ORW_AS
	call	PushTempRwSpc		;list "AS "
	lods	WORD PTR es:[si]	;ax = opStOpen's operand
	xchg	cx,ax			;save copy in cx

	mov	al,ch			;al = access/locking bits
	and	al,0F0H			;al = locking bits
	mov	bx,LISTOFFSET tRwAccess
	mov	dx,LISTOFFSET tMaskLock
	call	OutRwMask		;list LOCK READ/WRITE/READ WRITE  or
					;     SHARED

	mov	al,ch			;al = access/locking bits
	and	al,0FH			;al = access bits
	mov	bx,LISTOFFSET tRwAccess
	mov	dx,LISTOFFSET tMaskAccess
	call	OutRwMask		;list ACCESS READ/WRITE/READ WRITE

	mov	al,cl			;al = mode bits
	mov	bx,LISTOFFSET tRwMode
	mov	dx,LISTOFFSET tMaskMode
	call	OutRwMask		;list FOR INPUT/OUTPUT/APPEND/
					;         RANDOM/BINARY

	call	PopRootPushTemp		;move exp1 node from root to temp stk
	call	PushTempSpc
	call	PushTempOpRw		;list "OPEN"
	jmp	PushRevListStg1		;move temp stk to root in reverse order
					; and return to outer loop

; source: FIELD #1 ...
; pcode: opLit1 opLbs opFieldInit ...
;	[ expChan ...] ==> expChan FIELD]
;
ListRule LrFieldInit
	call	PopRootPushTemp		;copy #n to temp stack
	call	PushRootOpRwSpc		;list "FIELD "
	jmp	PushRevListStg1 	;return to outer loop
	

; source: , a as b, c as d
; pcode: opId(a) opId(b) opFieldItem opId(c) opId(d) opFieldItem
;	[ idAs2 expCnt2 ] ==> [idAs2 AS expCnt2 ,]
;
ListRule LrFieldItem
	call	PopRootPushTemp		;copy idAsN to temp stack
	call	PushTempSpc		;list " "
	call	PushTempOpRwSpc		;list "AS "
	call	PopPushCommaSpc		;copy expCntN to temp stack
					;list ", "
	jmp	PushRevListStg1		;return to outer loop


;	opStGetRec2: [exp2 exp1]  ==>  [exp2,,exp1 GET]
;	opStGetRec3: [exp3 exp2 exp1]  ==>  [exp3,exp2,exp1 GET]
;	opStPutRec2: [exp2 exp1]  ==>  [exp2,,exp1 PUT]
;	opStPutRec3: [exp3 exp2 exp1]  ==>  [exp3,exp2,exp1 PUT]
;
ListRule LrStGetRec2
ListRule LrStPutRec2
	call	PopPushCommaSpc		;move exp2 node from root to temp stk
					;list ", "
	jmp	SHORT GetPut1

ListRule LrStGetRec3
ListRule LrStPutRec3
	call	PopPushCommaSpc		;move exp3 node from root to temp stk
					;list ", "
	call	PopRootPushTemp		;move exp2 node from root to temp stk
GetPut1:
	call	PushTempCommaSpc	;list ", "
	call	PopRootPushTemp		;move exp1 node from root to temp stk
	call	PushRootOpRwSpc		;list "GET/PUT "
	inc	si			;skip size operand
	inc	si
	jmp	PushRevListStg1 	;move temp stk to root in reverse order
					; and return to outer loop


;	[exp2 exp1] ==> [exp2, exp1 WIDTH]
;
ListRule LrStWidth2
ListRule LrStWidthfile
	call	PopNilExp		;copy exp2 and "," to temp stack
	call	PopRootPushTemp		;copy exp1 from root to temp stk
	call	PushRootOpRwSpc		;list "WIDTH "
	jmp	PushRevListStg1		;move temp stk to root in reverse order
					; and return to outer loop

;copy exp from root to temp stack
; if exp is opUndef or opNull, don't copy anything to temp stack.
; If an expression is seen, all subsequent calls for this
; statement list at least ", "
;
PopNilExp PROC NEAR
	call	PopRoot			;ax = offset to exp node
	call	ListOffToPtr		;bx = ptr to node ax
	cmp	BYTE PTR [bx.LN_type],LNT_CHAR
	jne	NotNilExp		;brif not nil expression
	cmp	BYTE PTR [bx.LN_val_char],0
	jne	NotNilExp		;brif this arg's value is defaulted
					;i.e. if node produced by opNull or
					;opUndef
	test	[lsBosFlags2],FBOS2_NonNilExp
	je	GotNilExp
	jmp	SHORT NonNilExp

NotNilExp:
	call	PushTemp		;push node ax onto temp stack
	or	[lsBosFlags2],FBOS2_NonNilExp
NonNilExp:
	call	PushTempCommaSpc	;list ", "
GotNilExp:
	ret
PopNilExp ENDP



subttl PRINT related opcodes

;------------------------------------------------------------------
;			PRINT related opcodes
;------------------------------------------------------------------

;-----------------------------------------------------------------------
; Print related opcodes
;
; The statement:
;	PRINT USING a$; x, TAB(5); y
; produces the pcode:
;	(a$)opUsing
;       (x)opPrintItemComma
;       (5)opPrintTab
;       (y)opPrintItemEos
;
;-----------------------------------------------------------------------
	
;push PRINT [#n] to root stack if 1st time this has been called for this stmt
;if FBOS_PrintSemi is set, push "; " to the root stack
;
PushPrintIfBos PROC NEAR
	sub	ax,ax
	test	[lsBosFlags],FBOS_Channel
	je	NoPrintChan		;brif not PRINT #n
	and	[lsBosFlags],0FFH - FBOS_Channel
	call	PopRoot			;ax = channel node
NoPrintChan:
	push	ax			;save 0/channel
	mov	ax,ORW_PRINT
	call	PushStmtRwIfBos
	pop	ax			;ax = 0/channel
	or	ax,ax
	je	NoPrintChan1		;brif no channel
	call	PushRoot		;list channel again (after PRINT)
	call	PushRootCommaSpc	;list ", "
NoPrintChan1:
	test	[lsBosFlags],FBOS_PrintSemi
	je	NoSemi
	and	[lsBosFlags],0FFH - FBOS_PrintSemi
					;tell next PushPrintIfBos not to list 
	mov	al,';'
	call	PushRootCharSpc		;list "; "
NoSemi:
	ret
PushPrintIfBos ENDP

;	[exp]  ==>  [exp [[n #] PRINT]]
;
ListRule LrPrintItemEos
	call	PopRootPushTemp		;move exp from root to temp stack
	call	PushPrintIfBos		;list PRINT [#n] if appropriate
	jmp	PushListStg1		;push temp list to root as 1 node
					; and return to outer loop

ListRule LrPrintEos
	call	PushPrintIfBos		;list PRINT [#n] if appropriate
	jmp	SHORT J4_Stg1Loop	;return to outer loop

;	[exp]  ==>  [; ( exp ) TAB/SPC ]
;
ListRule LrPrintSpc
ListRule LrPrintTab
	call	PushTempOpRw		;list "TAB" or "SPC"
	call	PushTempLParen		;list "("
	call	PopRootPushTemp		;list exp
	call	PushTempRParen		;list ")"
	call	PushPrintIfBos		;list PRINT [#n] if appropriate
	or	[lsBosFlags],FBOS_PrintSemi
					;tell next print item's call to
					; PushPrintIfBos to list ";"
					; if no interviening ","
	jmp	PushListStg1		;push temp list to root as 1 node
					; and return to outer loop

ListRule LrPrintSemi
	or	[lsBosFlags],FBOS_PrintSemi
					;tell next print item's call to
					; PushPrintIfBos to list ";"
					; if no interviening ","
J4_Stg1Loop:
	jmp	Stg1Loop		;return to outer loop

;	[...]  ==>  [,]
;
ListRule LrPrintComma
	call	PushPrintIfBos		;list PRINT [#n] if appropriate
	call	PushRootOpChars		;list ","
	call	PushRootSpc		;list " "
	and	[lsBosFlags],0FFH - FBOS_PrintSemi
					;tell next PushPrintIfBos not to list 
	jmp	SHORT J4_Stg1Loop	;return to outer loop

;	[exp]  ==>  [,/; exp]
;
ListRule LrPrintItemComma
ListRule LrPrintItemSemi
	call	PopRootPushTemp		;move exp from root to temp stack
	call	PushTempOpChars		;list opcode's char(s) (,/:)
	call	PushTempSpc		;list " "
	call	PushPrintIfBos		;list PRINT [#n] if appropriate
	jmp	PushListStg1		;push temp list to root as 1 node
					; and return to outer loop

;	[exp]  ==>  [; exp USING]
;
ListRule LrUsing
	call	PushTempOpRwSpc		;list "USING "
	call	PopRootPushTemp		;list exp
	mov	al,';'
	call	PushTempCharSpc		;list "; "
	call	PushPrintIfBos		;list PRINT [#n] if appropriate
	jmp	PushListStg1		;push temp list to root as 1 node
					; and return to outer loop
ListRule LrStLprint
ListRule LrStWrite
	or	[lsBosFlags],FBOS_StmtRw	;remember not to list "PRINT"
	jmp	LrRwSpc			;list "LPRINT " and return to outer loop

ListRule LrInputChan
ListRule LrChanOut
	or	[lsBosFlags],FBOS_Channel ;remember to list [#]n,
	jmp	SHORT J4_Stg1Loop	;return to outer loop


subttl Graphics related pcodes

;------------------------------------------------------------------
;		Graphics related pcodes
;------------------------------------------------------------------

;	[exp2 exp1]  ==>  [( exp1 , exp2) ]
;
ListCoords PROC NEAR
	call	PushTempRParen		;list ")"
	call	PopPushCommaSpc		;move exp2 node from root to temp stack
					;list ", "
	call	PopRootPushTemp		;move exp1 node from root to temp stack
	jmp	PushTempLParen		;list "("
					;and return to caller
ListCoords ENDP

;copy 1st coord to temp stack if any
;
PushPop1stCoord PROC NEAR
	mov	al,'-'
	call	PushTempChar		;list "-" for 2nd coord pair
	test	[lsBosFlags2],FBOS2_1stCoord
	je	No1stCoord		;brif stmt like LINE -(x,y) instead
					; of LINE (x,y)-(x,y)
	call	PopRootPushTemp		;copy [STEP (x,y)] to temp stack
No1stCoord:
	ret
PushPop1stCoord ENDP


;	[exp2 exp1]  ==>  [[) exp2 , exp1 ( -]]
;
ListRule LrCoordSecond
	call	ListCoords		;list (exp1, exp2) to temp stack
	call	PushPop1stCoord		;copy 1st coord to temp stack if any
	jmp	SHORT J2_PushRevListStg1 ;push temp list to root in rev order
					; and return to outer loop

;	[exp2 exp1]  ==>  [[) exp2 , exp1 ( STEP -]]
;
ListRule LrCoordStepSecond
	call	ListCoords		;list (exp1, exp2) to temp stack
	mov	ax,ORW_STEP
	call	PushTempRw		;list "STEP"
	call	PushPop1stCoord		;copy 1st coord to temp stack if any
J2_PushRevListStg1:
	jmp	PushRevListStg1		;push temp list to root in rev order
					; and return to outer loop

;	[exp2 exp1]  ==>  [[) exp2 , exp1 ( STEP]]
;
ListRule LrCoordStep
	call	ListCoords		;list (exp1, exp2) to temp stack
	mov	ax,ORW_STEP
	call	PushTempRw		;list "STEP"
	jmp	SHORT LrCoord1

;	[exp2 exp1]  ==>  [[) exp2 , exp1 (]]
;
ListRule LrCoord
	call	ListCoords		;list (exp1, exp2) to temp stack
LrCoord1:
	or	[lsBosFlags2],FBOS2_1stCoord
	jmp	SHORT J2_PushRevListStg1 ;push temp list to root in rev order
					; and return to outer loop

;*************************************************************************
;LineBfArg
;Purpose:
;	consume [B[F]] arg and list it
;Exit:
;	condition codes: ZERO if no arg was present
;
;*************************************************************************
LineBfArg PROC NEAR
	lods	WORD PTR es:[si]	;ax = [B[F]] arg
	or	ax,ax
	je	LineBfExit		;brif no BF arg
	dec	al
	mov	al,'B'
	je	GotLineB		;brif got B arg
	mov	ah,'F'			;else it must have been BF
GotLineB:
	call	PushTempChars
	or	sp,sp			;set nz exit condition code
LineBfExit:
	ret
LineBfArg ENDP

;	[coord]  ==> [[BF ,,] coord LINE]
;
ListRule LrStLine
	call	LineBfArg		;consume and list [B[F]] arg
	je	LineNoArg		;brif no BF arg
	call	PushTempCommaSpc	;list ", "
	jmp	SHORT LineComma		;list comma and coord arg

;	[color coord]  ==> [[BF,] color , coord LINE]
;
ListRule LrStLineColor
	call	LineBfArg		;consume and list [B[F]] arg
	je	LineArg			;brif no BF arg
	jmp	SHORT LineCommaArg	;list comma, color and coord arg

;	[style coord]  ==> [style , [BF] ,, coord LINE]
;
ListRule LrStLineStyle
	call	PopPushCommaSpc		;move style from root to temp stk
					;list ", " between BF and style
	call	LineBfArg		;consume and list [B[F]] arg
	call	PushTempCommaSpc	;list ", " between color and BF
	jmp	SHORT LineComma		;list comma and coord arg

;	[style color coord]  ==> [style ,[BF], color, coord LINE]
;
ListRule LrStLineStyleColor
	call	PopPushCommaSpc		;move style from root to temp stk
					;list ", "
	call	LineBfArg		;consume and list [B[F]] arg
	jmp	SHORT LineCommaArg	;list color and coord arg

;Table for mapping PUT function to reserved word
tRwPutFunc LABEL WORD
	DW	ORW_OR
	DW	ORW_AND
	DW	ORW_PRESET
	DW	ORW_PSET
	DW	ORW_XOR

;	[array coord]  ==>  [function array coord PUT]
;
;	[color coord]  ==>  [color coord PSET/PRESET]
;
;	[array coord]  ==>  [array coord GET]
;	
;	[coord]  ==>  [coord PSET/PRESET]
;
ListRule LrStGraphicsPut
	lods	WORD PTR es:[si]	;ax = function mask
	shl	ax,1			;ax = function * 2
	js	NoPutFunc		;brif no function specified
					; i.e. if ax was UNDEFINED
	xchg	bx,ax
	mov	ax,tRwPutFunc[bx]	;ax = ORW_OR .. ORW_XOR
	call	PushTempRw		;list OR .. XOR
LineCommaArg:
	call	PushTempCommaSpc	;list ", "
NoPutFunc:
ListRule LrStPresetColor
ListRule LrStPsetColor
ListRule LrStGraphicsGet
LineArg:
	call	PopRootPushTemp		;move color from root to temp stk
LineComma:
	call	PushTempCommaSpc	;list ", "
ListRule LrStPreset
ListRule LrStPset
LineNoArg:
	call	PopRootPushTemp		;move coord from root to temp stk
	call	PushRootOpRwSpc		;list "PSET " or "PRESET "
	jmp	PushRevListStg1		;move temp stk to root in reverse order
					; and return to outer loop


;---------------------------------------------------------------------
; The statement:
;	CIRCLE (x,y),r,c,astart,astop,aspect 
; generates pcode:
;	(x)(y)opCoord
;	(r)(c)
;	(astart)opCircleStart
;	(astop)opCircleEnd
;	(aspect)opCircleAspect
;	opStCircleColor
; where (r)(c) are operands for opStCircleColor
;
;	CIRCLE (x,y),r,,,,aspect
; generates pcode:
;	(x)(y)opCoord
;	(r)
;	opNull opCircleStart
;	(aspect)opCircleAspect
;	opStCircle
;	  NOTE: opCircleEnd not generated because runtime can't tell
;	        opNull from whether user supplied value
;
;---------------------------------------------------------------------

;------------------------------------
;	[expStart]  ==>  [expStart ,]
;
ListRule LrCircleStart
	call	PopRootPushTemp		;copy expStart to temp stack
	call	PushTempCommaSpc	;emit ", "
	or	[lsBosFlags2],FBOS2_Circle1
					;tell LrCircle[Color] that it
					; has a startEndAspect node
	jmp	SHORT J3_PushRevListStg1 ;copy temp stk to root in rev order

;------------------------------------
;	[expEnd [expStart ,]]  ==>  [expEnd ,[expStart ,]]
;
ListRule LrCircleEnd
	or	[lsBosFlags2],FBOS2_Circle2
					;remember we've gotten an opCircleEnd
	call	PopRootPushTemp		;copy expEnd to temp stack
	call	PushTempCommaSpc	;emit ", "
CE_CopyStart:
	call	PopRootPushTemp		;copy [expStart ,] from root to temp
J3_PushRevListStg1:
	jmp	PushRevListStg1		;copy temp stk to root in rev order

;-------------------------------------------
;	[expAspect ]  ==>  [expAspect , , ,]
;		   or
;	[expAspect [expStart ,]]  ==>  [expAspect , ,[expStart ,]]
;		   or
;	[expAspect [expEnd ,[expStart ,]]]  ==>
;	   [expAspect ,[expEnd ,[expStart ,]]]
;
ListRule LrCircleAspect
	call	PopRootPushTemp		;copy expAspect to temp stack
	call	PushTempCommaSpc	;emit ", " between aspect and start
	test	[lsBosFlags2],FBOS2_Circle2
	jne	CE_CopyStart		;brif stmt has opCircleEnd
	call	PushTempCommaSpc	;emit ", " between aspect and start
	jmp	SHORT CE_CopyStart

;----------------------------------------------------
;	[[startEndAspect] expRadius coord]  ==>
;          [[startEndAspect] expRadius ,coord CIRCLE]
;
ListRule LrStCircle
	test	[lsBosFlags2],FBOS2_Circle1
	je	CirCont2		;brif no start/end/aspect args
	jmp	SHORT CirCont1		;copy expAspect from root to temp

;--------------------------------------------------------------
;	[[startEndAspect] expColor expRadius coord]  ==>
;          [[startEndAspect] expColor ,expRadius ,coord CIRCLE]
;
ListRule LrStCircleColor
	test	[lsBosFlags2],FBOS2_Circle1
	je	CirCont1		;brif no start/end/aspect args
	call	PopRootPushTemp		;copy startEndAspect from root to temp
CirCont1:
	call	PopPushCommaSpc		;copy expColor to temp stack
					; or startEndAspect from root to temp
					; if LrStCircle
					;list ", "
CirCont2:
	call	PopPushCommaSpc		;copy expRadius to temp stack
					;list ", "
	call	PopRootPushTemp		;copy coord to temp stack
	call	PushTempSpc
	call	PushTempOpRw		;list CIRCLE
	jmp	SHORT J3_PushRevListStg1 ;move temp stk to root in reverse order


; [exp3 exp2 exp1 coord] ==> [exp3, exp2, exp1, coord PAINT]
;
ListRule LrStPaint
ListRule LrStPaint3
	call	PopNilExp		;copy exp3 and "," to temp stack
ListRule LrStPaint2
	call	PopNilExp		;copy exp2 and "," to temp stack
	call	PopNilExp		;copy exp1 and "," to temp stack
	call	PopRootPushTemp		;copy coord to temp stack
	call	PushTempSpc
	call	PushTempOpRw		;list PAINT
	jmp	SHORT J4_PushRevListStg1 ;move temp stk to root in reverse order

PushCoordPair PROC NEAR
	call	ListCoords		;list (exp4 ,exp3) to temp stack
	mov	al,'-'
	call	PushTempChar		;list "-"
	jmp	ListCoords		;list (exp2 ,exp1) to tmp stack & return
PushCoordPair ENDP

;  [exp6 exp5 exp4 exp3 exp2 exp1]  ==>
;  [exp6 ,exp5 ,(exp4 ,exp3)-(exp2 ,exp1) VIEW]
;
ListRule LrStView
	call	PopNilExp		;copy exp6 and "," to temp stack
	call	PopNilExp		;copy exp5 and "," to temp stack
ListRule LrStWindow
	call	PushCoordPair		;list (exp4 ,exp3)-(exp2 ,exp1)
	call	PushTempSpc
	call	PushTempOpRw		;list VIEW/WINDOW
	jmp	SHORT J4_PushRevListStg1 ;move temp stk to root in reverse order

;  [exp6 exp5 exp4 exp3 exp2 exp1]  ==>
;  [exp6 ,exp5 ,(exp4 ,exp3)-(exp2 ,exp1) SCREEN VIEW]
;
ListRule LrStViewScreen
	call	PopNilExp		;copy exp6 and "," to temp stack
	call	PopNilExp		;copy exp5 and "," to temp stack
ListRule LrStWindowScreen
	call	PushCoordPair		;list (exp4 ,exp3)-(exp2 ,exp1)
	call	PushTempSpc
	mov	ax,ORW_SCREEN
	call	PushTempRwSpc		;list SCREEN
	call	PushTempOpRw		;list VIEW/WINDOW
J4_PushRevListStg1:
	jmp	PushRevListStg1		;move temp stk to root in reverse order

;	[exp2 exp1 ...]  ==>  [exp2 TO exp1 PRINT VIEW ...]
;
;	[...]  ==>  [PRINT VIEW ...]
;
ListRule LrStViewPrint2
	call	PopRootPushTemp		;move exp2 from root to temp stack
	call	PushTempSpc		;list " "
	mov	ax,ORW_TO
	call	PushTempRwSpc		;list "TO "
	call	PopRootPushTemp		;move exp1 from root to temp stack
	call	PushTempSpc		;list " "
ListRule LrStViewPrint0
	mov	ax,ORW_PRINT
	call	PushTempRw		;list "PRINT"
	call	PushRootOpRwSpc		;list "VIEW "
	jmp	SHORT J4_PushRevListStg1 ;move temp stk to root in reverse order
	

sEnd	LIST

end
