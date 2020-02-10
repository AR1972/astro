	TITLE	LSUTIL - utility module of 'lister' component

;======================================================================
; Module: LsUtil.asm - utility module of 'lister' component
; Subsystem:  Lister
; System:  Quick BASIC Interpreter
;
;
;=======================================================================*/

	include 	version.inc
	LSUTIL_ASM = ON
	includeOnce	architec
	includeOnce	context
	includeOnce	heap
	includeOnce	lister
	includeOnce	lsint
	includeOnce	names
	includeOnce	qblist
	includeOnce	scanner
	includeOnce	txtmgr
	includeOnce	variable

	PERIOD_ID EQU ON		;record.element, not record->element

	assumes DS,DGROUP
	assumes ES,DGROUP
	assumes SS,DGROUP

sBegin	LIST
assumes CS,LIST

subttl	List Node creation functions

;***************************************************************************
; NewChar, NewChars
; Purpose:
;	Create a new LNT_CHAR node
; Entry:
;	[al] = ASCII value for char
;	for NewChars: [ah] = ASCII value for 2nd char to list
;	di = offset to next free byte in bdNodes
; Exit:
;	di is updated
;	ax = offset to newly created node
;
;***************************************************************************
PUBLIC	NewChar
NewChar PROC NEAR
	sub	ah,ah			;only 1 char in this node
NewChar ENDP
	;fall into NewChars
PUBLIC	NewChars
NewChars PROC NEAR
	mov	bx,di			;bx = offset to new node (return value)
	add	di,[bdNodes.BD_pb]	;convert offset to ptr
	stosDsWord 0			;set LN_sib field to 0
	stosDsByte LNT_CHAR		;set LN_type field
	stosDsWord ax			;set LN_val_char field
	sub	di,[bdNodes.BD_pb]	;convert ptr to offset
	mov	ax,bx			;return offset to new node
	ret
NewChars ENDP

;***************************************************************************
; NewId
; Purpose:
;	Fetch the next 2 bytes of pcode, which represent an id
;	reference.  If scanState != SS_RUDE, convert this 16 bit
;	module-value-table offset into a name table offset.
;	Produce the following nodes:
;	->[LNT_oNam(oNam)]
; Entry:
;	es:si points to id's oNam or oVar argument
;	di = offset to next free byte in bdNodes
; Exit:
;	si points beyond id's operand
;	di is updated
;	ax = offset to newly created id node
;	bx = oNam
;
;***************************************************************************
;***************************************************************************
; NewONam
; Purpose:
;	Produce the following nodes:
;	->[LNT_oNam(oNam)]
; Entry:
;	ax = oNam operand
;	di = offset to next free byte in bdNodes
; Exit:
;	di is updated
;	ax = offset to newly created id node
;	bx = oNam
;
;***************************************************************************
PUBLIC	NewId
NewId	PROC NEAR
	lods	WORD PTR es:[si]	;ax = operand
	push	es			;save ptr to text table
	push	ax			;push variable's oNam/oVar
	call	ONamOVarRudeOrParse	;get oNam
	pop	es			;restore es
	
	; fall into NewONam		;produce an oNam node
NewId	ENDP
PUBLIC	NewONam
NewONam PROC NEAR
	DbChk	oNam,ax
	mov	dl,LNT_ONAM		;dl = LN_type field
;ax = node's value, dl = node's type
NewNodeDlAx:
	mov	bx,di			;bx = offset to new node (return value)
	add	di,[bdNodes.BD_pb]	;convert offset to ptr
	stosDsWord 0			;set LN_sib field to 0
	stosDsByte dl			;set LN_type field
	stosDsWord ax			;save oNam in LN_val_oNam field
	sub	di,[bdNodes.BD_pb]	;convert ptr to offset
	xchg	ax,bx			;return offset to new node in ax
					;bx = oNam
	ret
NewONam ENDP

;***************************************************************************
; NewRw
; Purpose:
;	Create a new LNT_RW node
; Entry:
;	[ax] = ORW_xxx (offset into res word table for res word to list)
;	di = offset to next free byte in bdNodes
; Exit:
;	di is updated
;	ax = offset to newly created node
;
;***************************************************************************
PUBLIC	NewRw
NewRw	PROC NEAR
	mov	dl,LNT_RW		;dl = LN_type field
	jmp	SHORT NewNodeDlAx	;create new node and return offset in ax
NewRw	ENDP


;***************************************************************************
; NewCsStr
; Purpose:
;	Create a new LNT_CSSTR node
; Entry:
;	[ax] = offset into LIST segment to string constant to list
;	di = offset to next free byte in bdNodes
; Exit:
;	di is updated
;	ax = offset to newly created node
;
;***************************************************************************
PUBLIC	NewCsStr
NewCsStr PROC NEAR
	mov	dl,LNT_CSSTR		;dl = LN_type field
	jmp	SHORT NewNodeDlAx	;create new node and return offset in ax
NewCsStr ENDP

;***************************************************************************
; NewSpaces
; Purpose:
;	Create a new LNT_SPACES node
; Entry:
;	[ax] = count of spaces
;	di = offset to next free byte in bdNodes
; Exit:
;	di is updated
;	ax = offset to newly created node
;
;***************************************************************************
PUBLIC	NewSpaces
NewSpaces PROC NEAR
	mov	dl,LNT_SPACES		;dl = LN_type field
	jmp	SHORT NewNodeDlAx	;create new node and return offset in ax
NewSpaces ENDP

;***************************************************************************
; NewCol, NewCol1
; Entry:
;	ax = column to advance to
; NewCol1 is the same as NewCol, but it always lists at least 1 column,
; even if we're beyond the specified column.
;
;***************************************************************************
PUBLIC NewCol1
NewCol1 PROC NEAR
	or	ah,80h			;always list at least 1 column, even if
					; we're beyond the specified column.
NewCol1 ENDP
PUBLIC NewCol
NewCol PROC NEAR
	mov	dl,LNT_COL		;dl = LN_type field
	jmp	SHORT NewNodeDlAx	;create new node and return offset in ax
NewCol ENDP

;***************************************************************************
; NewStr/NewEnStr/NewLitStr
; Purpose:
;	Consume a string literal and produce a LNT_STR/LNT_ENSTR/LNT_LITSTR node
; Entry:
;	ax = byte count
;	es:si points to 1st byte
; Exit:
;	si is advanced beyond the string constant
;	ax = offset to newly created node
;
;***************************************************************************
PUBLIC	NewEnStr
NewEnStr PROC NEAR
	mov	dl,LNT_ENSTR		;produce an encoded string node
	SKIP2_PSW
NewEnStr ENDP

PUBLIC	NewStr
NewStr	PROC NEAR
	mov	dl,LNT_STR		;produce an standard string node

NewStrCommon:
	mov	bx,di			;bx = offset to new node (return value)
	add	di,[bdNodes.BD_pb]	;convert offset to ptr
	mov	cx,ax			;cx = length of string
	sub	ax,ax			;ax = 0
	stosDsWord ax			;set LN_sib field to 0
	stosDsByte dl			;set LN_type field
	stosDsWord cx			;save offset in LN_val_cbStr field
	stosDsWord si			;save count in LN_val_oStr field
	inc	cx			;round up to even byte count
	and	cl,0FEH
	add	si,cx			;skip past string literal
	sub	di,[bdNodes.BD_pb]	;convert ptr to offset
	mov	ax,bx			;return offset to new node
	ret
NewStr	ENDP

;***************************************************************************
; NewNum
; Purpose:
;	Consume a numeric literal and produce a LNT_NUM node
; Entry:
;	al = constant value size (2, 4, or 8)
;	ah = constant type
;	     (LIT_D2, LIT_O2, LIT_H2,
;	      LIT_D4, LIT_O4, LIT_H4,
;	      LIT_R4, LIT_R8,
;	      LIT_LINENUM)
;	if ah = LIT_LINENUM,
;	   si = value of line number (16 bit unsigned number)
;	else
;	   es:si points to 1st byte of value
; Exit:
;	if ah = LIT_LINENUM,
;	   si contains garbage
;	else
;	   si is advanced beyond the string constant
;	ax = offset to newly created node
;
;***************************************************************************
PUBLIC	NewNum
NewNum	PROC NEAR
	mov	bx,di			;bx = return value
	add	di,[bdNodes.BD_pb]	;convert offset to ptr
	stosDsWord 0			;set LN_sib field to 0
	stosDsByte LNT_NUM		;set LN_type field
	stosDsWord ax			;set LN_val_cbNum, LN_val_clNum fields
	stosDsWord si			;set LN_val_otxNum
	sub	di,[bdNodes.BD_pb]	;convert ptr to offset
	sub	ah,ah			;ax = size of constant in bytes
	add	si,ax			;advance past constant's value
	mov	ax,bx			;ax = return value (offset to new node)
	ret
NewNum	ENDP

;***************************************************************************
; NewLabel, NewModLabel
; Purpose:
;	Fetch the  next 2 bytes of pcode, which represent a label
;	reference.  If scanState = SS_EXECUTE, convert this 16 bit
;	text offset into a name table offset.  If name mgr says this
;	is a line-number oNam, create an LNT_NUM node, else create an
;	LNT_ONAM node.
; 	NewModLabel is the same, except the referenced label is known
;	to exist in module level code.  This is used to list the statements
;	ON ERROR GOTO <label>,  ON <event> GOSUB <label>,  RESTORE <label>.
; Entry:
;	es:si points to oNam or otx argument
; Exit:
;	si bumped by 2
;	dl = 0 if it was an alphanumeric label, 1 if it was a line number
;	ax = offset to newly created node
;
;***************************************************************************
PUBLIC	NewModLabel
NewModLabel PROC NEAR
	cmp	[txdCur.TXD_scanState],SS_EXECUTE
	jne	NewLabel		;brif table isn't in execute state
					; in this case, operand is an oNam
	test	[txdCur.TXD_flags],FTX_mrs
	jne	NewLabel		;brif not within a procedure txt tbl

;It would be an error for a procedure to be in SS_EXECUTE state, and
;a module to be in SS_PARSE state, because the procedure could contain
;invalid otx operands to module level code.  Assert this is not the case.
;
DbAssertRelB[mrsCur.MRS_txd.TXD_scanState],e,SS_EXECUTE,LIST,<NewModLabel err1>
	;convert oTxt to module level label's oNam
	lods	WORD PTR es:[si]	;ax = operand
	xchg	bx,ax			;bx = offset to opBolLab's opcode
	push	ds
	mov	ds,[mrsCur.MRS_txd.TXD_bdlText_seg]
					;ds points to module level pcode seg
	mov	ax,[bx + 4]		;ax = opBolLab's oNam operand
	pop	ds
	jmp	SHORT NewLabelONam
NewModLabel ENDP

PUBLIC	NewLabel
NewLabel PROC NEAR
	lods	WORD PTR es:[si]	;ax = operand
	cmp	[txdCur.TXD_scanState],SS_EXECUTE
	jne	NewLabelONam		;brif table isn't in execute state
	;convert oTxt to oNam for referenced label
	mov	bx,ax			;bx = offset to opBolLab's opcode
	mov	ax,es:[bx + 4]		;ax = opBolLab's oNam operand
NewLabel ENDP
	;fall into NewLabelONam
;***************************************************************************
; NewLabelONam
; Purpose:
;	Given an oNam which represent a label reference, create
;	an LNT_ONAM node if its an alpha-label, else a LNT_NUM
;	node if its a line-number.
; Entry:
;	ax = oNam for this label
; Exit:
;	dl = 0 if it was an alphanumeric label, 1 if it was a line number
;	ax = offset to newly created node
;
;***************************************************************************
PUBLIC NewLabelONam
NewLabelONam PROC NEAR
	DbChk	oNam,ax
	push	ax			;save oNam
	push	es			;LnOfONam destroys es
	cCall	LnOfONam,<ax>		; ax = linenum or UNDEFINED
					;dl = 1st letter if alpha name
	pop	es			;restore es = text table seg
	inc	ax			;test for UNDEFINED
	je	GotAlphaLabel		;brif not a line number
	pop	dx			;discard stacked oNam
	dec	ax			;ax = linenum
	push	si			;save caller's si
	mov	si,ax			;si = linenum
	mov	ax,LIT_LINENUM * 256 + 2
	call	NewNum			;ax = create LNT_NUM node
	pop	si			;restore caller's si
NlLineNum:
	mov	dl,1			;tell caller its a line number
NlExit:
	ret

;dl = 1st letter of label
GotAlphaLabel:
	pop	ax			;ax = oNam
	push	dx			;preserve 1st letter of label
	call	NewONam 		;ax = new node for this label
	pop	bx
	sub	dx,dx
	cmp	bl,'0'
	jb	NlExit			;brif not line num > 65529
	cmp	bl,'9'
	jbe	NlLineNum		;brif line num > 65529
	jmp	SHORT NlExit
NewLabelONam ENDP

;***************************************************************************
; ListOffToPtr
; Purpose:
;	Convert a node offset to a node ptr.  This means that list rules
;	need not know about bdNodes or include heap.inc
; Entry:
;	ax = offset to node
; Exit:
;	bx = pointer to node
;
;***************************************************************************
;bx = sibbling(bx)
PUBLIC ListSibPtr
ListSibPtr PROC NEAR
	mov	ax,[bx.LN_sib]		;ax = offset to si's sibbling node
ListSibPtr ENDP
	;fall into ListOffToPtr to convert ax from offset to pointer
PUBLIC ListOffToPtr
ListOffToPtr PROC NEAR
	mov	bx,ax			;bx = offset to node
	add	bx,[bdNodes.BD_pb]	;convert offset to ptr
	ret
ListOffToPtr ENDP


subttl	Root and Temp list manipulation functions

;***************************************************************************
; PushRoot
; Purpose:
;	Push a node onto oNodeRoot's list, for example:
;	[x y z]  ==>  [newNode x y z]
; Entry:
;	ax = offset to new root
; Preserves:
;	cx (depended on by some callers)
;
;***************************************************************************
PUBLIC	PushRoot
PushRoot PROC NEAR
	mov	bx,ax			;bx = offset for new root node
	add	bx,[bdNodes.BD_pb]	;convert offset to ptr (new root)
	xchg	ax,[oNodeRoot]		;save offset for new root
					;ax = offset for old root
	mov	LN_sib[bx],ax		;new.sib = old
	ret
PushRoot ENDP

;***************************************************************************
; PopRoot
; Purpose:
;	Pop a node from oNodeRoot's list, for example:
;	[x y z]  ==>  [y z]
; Exit:
;	ax = offset to popped node
;	bx = pointer to the popped node 
;	popped node's LN_sib field is set to NULL
; Preserves:
;	cx (depended on by some callers)
;
;***************************************************************************
PUBLIC	PopRoot
PopRoot PROC NEAR
	mov	ax,[oNodeRoot]		;ax = offset to current root node
					; (return value)
	mov	bx,ax			;bx = offset to current root node
	add	bx,[bdNodes.BD_pb]	;convert offset to ptr
	mov	dx,LN_sib[bx]		;dx points to new root
	mov	[oNodeRoot],dx
	mov	LN_sib[bx],NULL 	;unlink old root
	ret
PopRoot ENDP

;***************************************************************************
; PushTemp
; Purpose:
;	Push a node onto oNodeTemp's list, for example:
;	[x y z]  ==>  [newNode x y z]
; Entry:
;	ax = offset to node to push onto oNodeTemp's stack
;
;***************************************************************************
;***************************************************************************
; PopRootPushTemp
; Purpose:
;	Move 1 node from top of root stack to top of temp stack.
;
;***************************************************************************
PUBLIC PopRootPushTemp
PopRootPushTemp PROC NEAR
	call	PopRoot			;ax = popped node
PopRootPushTemp ENDP
	;fall into PushTemp
PUBLIC	PushTemp
PushTemp PROC NEAR
	mov	bx,ax			;bx = offset for new temp node
	add	bx,[bdNodes.BD_pb]	;convert offset to ptr (new temp)
	xchg	ax,[oNodeTemp]		;save offset for new temp
					;ax = offset for old temp
	mov	LN_sib[bx],ax		;new.sib = old
	ret
PushTemp ENDP

;***************************************************************************
; PushRevList
; Purpose:
;	Push the entire list headed by oNodeTemp as a child-list-node
;	onto oNodeRoot's list, but reverse the order of the temp list first.
;	For example:
;	  before:
;		root: [x y z]  temp: [a b c]
;	  after:
;		root: [[c b a] x y z]  temp: []
; Entry:
;	oNodeTemp = offset to list of nodes to be pushed as a list
;	onto oNodeRoot's stack
;
;***************************************************************************
;***************************************************************************
; PushList
; Purpose:
;	Push the entire list headed by oNodeTemp as a child-list-node
;	onto oNodeRoot's list
;	For example:
;	  before:
;		root: [x y z]  temp: [a b c]
;	  after:
;		root: [[a b c] x y z]  temp: []
; Entry:
;	oNodeTemp = offset to list of nodes to be pushed as a list
;	onto oNodeRoot's stack
;
;***************************************************************************
PUBLIC	PushRevList
PushRevList PROC NEAR
	mov	bx,[oNodeTemp]		;bx = offset to start of temp list
	DbAssertRel bx,ne,0,LIST,<PushRevList: temp stack empty> 
	sub	ax,ax			;prev node = NULL
;bx = offset to current node,
;ax = offset to previous node (if start of list, ax = 0),
;Traverse bx's list to the end, reversing linkage
;
RevListLoop:
	mov	dx,bx			;save cur nodes offset
	add	bx,[bdNodes.BD_pb]	;convert offset to ptr
	mov	cx,LN_sib[bx]		;cx = offset to next node (if any)
	mov	LN_sib[bx],ax		;swap from prev->next to next->prev
	jcxz	RevListDone		;brif we're at the end-of-list
	mov	ax,dx			;ax = offset to prev node
	mov	bx,cx			;bx = offset to current node
	jmp	SHORT RevListLoop

RevListDone:
	mov	[oNodeTemp],dx		;last node is now head of temp list
PushRevList ENDP
	;fall into PushList
PUBLIC	PushList
PushList PROC NEAR
	mov	bx,di			;bx = offset to new node
	add	di,[bdNodes.BD_pb]	;convert offset to ptr
	mov	ax,[oNodeRoot]		;ax = offset for old root
	stosDsWord ax			;store LN_sib field
	stosDsByte LNT_LIST		;set LN_type field
	mov	ax,[oNodeTemp]
	DbAssertRel ax,ne,0,LIST,<PushList: temp stack empty> 
	stosDsWord ax			;store LN_val_list field
	mov	[oNodeTemp],0
	mov	[oNodeRoot],bx		;save offset for new root
	sub	di,[bdNodes.BD_pb]	;convert ptr to offset
	ret
PushList ENDP

;push '(' onto root stack
PUBLIC	PushRootLParen
PushRootLParen PROC NEAR
	mov	al,'('
	jmp	SHORT PushRootChar	;push a char node (al) onto root stack
PushRootLParen ENDP

;push ')' onto root stack
PUBLIC	PushRootRParen
PushRootRParen PROC NEAR
	mov	al,')'
	jmp	SHORT PushRootChar	;push a char node (al) onto root stack
PushRootRParen ENDP

PUBLIC	PushRootSpc
PushRootSpc PROC NEAR
	mov	al,' '
PushRootSpc ENDP
	;fall into PushRootChar
PUBLIC	PushRootChar
PushRootChar PROC NEAR
	sub	ah,ah			;only 1 char in this node
PushRootChar ENDP
	;fall into PushRootChars
;push a char node (ax) onto root stack
PUBLIC	PushRootChars
PushRootChars PROC NEAR
	call	NewChars
	jmp	PushRoot
PushRootChars ENDP

;push '(' onto temp stack
PUBLIC	PushTempLParen
PushTempLParen PROC NEAR
	mov	al,'('
	jmp	SHORT PushTempChar	;push a char node (al) onto temp stack
PushTempLParen ENDP

;push ')' onto temp stack
PUBLIC	PushTempRParen
PushTempRParen PROC NEAR
	mov	al,')'
	jmp	SHORT PushTempChar	;push a char node (al) onto temp stack
PushTempRParen ENDP

PUBLIC	PushTempSpc
PushTempSpc PROC NEAR
	mov	al,' '
PushTempSpc ENDP
	;fall into PushTempChar
;push a char node (al) onto temp stack
PUBLIC	PushTempChar
PushTempChar PROC NEAR
	sub	ah,ah			;only 1 char in this node
PushTempChar ENDP
	;fall into PushTempChars
;push a char node (ax) onto temp stack
PUBLIC	PushTempChars
PushTempChars PROC NEAR
	call	NewChars		;ax = offset to new node
	jmp	PushTemp		;push it onto temp stack
PushTempChars ENDP

;***************************************************************************
;CharToCharTok
;Purpose:
;	Given the last node created by PushRootChar[s], PushTempChar[s]
;	or NewChar[s], convert its node-type from LNT_CHAR to LNT_CHAR_TOK.
;	This is done for nodes which are known to not begin a lexical token.
;	For example, "string" is 3 nodes, char, string, char, but only
;	1 lexical token, so the 2nd char node is converted to LNT_CHAR_TOK.
;	This is so ChkLineWrap in lsmain.asm knows not to split a logical
;	line into 2 physical lines at this node.
;Entry:
;	di = offset to last node created.
;
;***************************************************************************
PUBLIC	CharToCharTok
CharToCharTok PROC NEAR
	mov	bx,di
	add	bx,[bdNodes.BD_pb]	;convert offset to ptr 
DbAssertRelB <[bx + LN_type - CBLNT_CHAR]>,e,LNT_CHAR,LIST,<CharToCharTok err1>
	mov	[bx + LN_type - CBLNT_CHAR],LNT_CHAR_TOK
	ret
CharToCharTok ENDP

;***************************************************************************
; PushRootONam
; Purpose:
;	Create a new oNam list node and push it onto oNodeRoot's stack.
; Entry:
;	ax = oNam
; Exit:
;	none
;
;***************************************************************************
PUBLIC PushRootONam
PushRootONam PROC NEAR
	call	NewONam 		;ax = offset to new ONam node
	jmp	PushRoot		;push node to root stack
					; and return to caller
PushRootONam ENDP

;***************************************************************************
; PushRootLabel
; Purpose:
;	Fetch the next 2 bytes of pcode, which represent a label
;	reference.  If scanState = SS_EXECUTE, convert this 16 bit
;	text offset into a name table offset.  Create a new oNam list
;	node and push it onto oNodeRoot's stack.
; Entry:
;	es:si points to oNam or otx argument
; Exit:
;	si bumped by 2
;	none
;
;***************************************************************************
PUBLIC	PushRootLabel
PushRootLabel PROC NEAR
	call	NewLabel		;ax = new node for this label
	jmp	PushRoot
PushRootLabel ENDP

;push opcode's reserved word node to root's stack
PUBLIC	PushRootOpRw
PushRootOpRw PROC NEAR
	mov	bx,[opList2]		;bx = opcode being listed * 2
	mov	ax,[mpOpLsArg + bx]	;ax = ORW_xxx to be listed
PushRootOpRw ENDP
	;fall into PushRootRw
;Create a reserved-word node for oRw ax and push it to oNodeRoot's stack
PUBLIC	PushRootRw
PushRootRw PROC NEAR
	call	NewRw			;ax = offset to node for "REM"
	jmp	PushRoot
PushRootRw ENDP

;push opcode's reserved word node followed by space node to root's stack
PUBLIC	PushRootOpRwSpc
PushRootOpRwSpc PROC NEAR
	call	PushRootOpRw
	jmp	PushRootSpc
PushRootOpRwSpc ENDP

;Push a reserved word node followed by a space node to root stack
PUBLIC PushRootRwSpc
PushRootRwSpc PROC NEAR
	call	PushRootRw		;list reserved word [ax]
	jmp	PushRootSpc		;list a space and return to caller
PushRootRwSpc ENDP

;Push '"' literal_string_node '"' onto root stack
;	added as part of revision [6]
; ax = length of string; es:si points to text of string
PUBLIC PushRootQStr
PushRootQStr PROC NEAR
	push	ax			;preserve cbText
	mov	al,34			;al = code for double quote "
	call	PushTempChar
	pop	ax			;restore ax = cbText
	call	NewStr			;ax = offset to new node
	call	PushTemp
	mov	al,34			;al = code for double quote "
	call	PushTempChar
	call	CharToCharTok		;convert it to a LNT_CHARS_TOK node
	call	PushList		;convert to single node on Root stack
	ret
PushRootQStr ENDP

;push opcode's reserved word node to temp stack
PUBLIC	PushTempOpRw
PushTempOpRw PROC NEAR
	mov	bx,[opList2]		;bx = opcode being listed * 2
	mov	ax,[mpOpLsArg + bx]	;ax = ORW_xxx to be listed
PushTempOpRw ENDP
	;fall into PushTempRw
;Create a reserved-word node for oRw ax and push it to oNodeTemp's stack
PUBLIC	PushTempRw
PushTempRw PROC NEAR
	call	NewRw			;ax = offset to node for "REM"
	jmp	PushTemp
PushTempRw ENDP

;push opcode's reserved word node followed by space node to temp stack
PUBLIC	PushTempOpRwSpc
PushTempOpRwSpc PROC NEAR
	call	PushTempOpRw
	jmp	PushTempSpc
PushTempOpRwSpc ENDP

;Push a reserved word node followed by a space node to temp stack
PUBLIC PushTempRwSpc
PushTempRwSpc PROC NEAR
	call	PushTempRw		;list reserved word [ax]
	jmp	PushTempSpc		;list a space and return to caller
PushTempRwSpc ENDP

;***************************************************************************
; PushCommaArgs
; Purpose:
;	Copy cLsArgs from root to temp and separate them by commas.
;	Nodes created by opUndef are not listed.
; Entry:
;	cLsArgs = number of args to be transfered from root to temp stack
; Exit:
;	cLsArgs = 0
;
;***************************************************************************
PUBLIC	PushCommaArgs
PushCommaArgs PROC NEAR
	sub	cx,cx			;cx = 0
	mov	cl,[cLsArgs]		;cx = count of args
	jcxz	EndOfArgs		;brif no args
	mov	bx,[oNodeRoot]		;bx = offset to current root node
MoveIndLoop:
	DbAssertRel bx,ne,0,LIST,<PushCommaArgs: root stack underflow> 
	push	bx			;save offset to index node
	add	bx,[bdNodes.BD_pb]	;convert offset to ptr
	cmp	LN_type[bx],LNT_CHAR
	jne	NotUndefNode		;brif couldn't be opUndef node
	cmp	WORD PTR LN_val_char[bx],100h
	jne	NotUndefNode
	pop	ax			;don't list node's created by opUndef
	dec	[cLsArgs]
NotUndefNode:
	mov	bx,LN_sib[bx]		;bx = offset to next index
	loop	MoveIndLoop		;repeat for all args
	mov	[oNodeRoot],bx		;save new root after all args popped
	pop	ax			;ax = offset to next index node
	call	PushTemp		;transfer it to temp stack
XferIndLoop:
	dec	[cLsArgs] 		;countdown # of args left
	je	EndOfArgs		;brif end of args
	call	GrowBdNodes		;grow list buffer if necessary
	je	XferOmErr		;brif out-of-memory - We'll abort
					; ListLine next time through Stg1Loop
	call	PushTempCommaSpc	;push ", " node onto temp stack
	pop	ax			;ax = offset to next index node
	call	PushTemp		;transfer it to temp stack
	jmp	SHORT XferIndLoop

XferOmErr:
	pop	ax			;ax = offset to next index node
	jmp	SHORT XferIndLoop

EndOfArgs:
	ret
PushCommaArgs ENDP

PUBLIC PushTempCharSpc
PushTempCharSpc PROC NEAR
	call	PushTempChar
	jmp	PushTempSpc
PushTempCharSpc ENDP

PUBLIC PushRootCharSpc
PushRootCharSpc PROC NEAR
	call	PushRootChar
	jmp	PushRootSpc
PushRootCharSpc ENDP

PUBLIC PushTempComma
PushTempComma PROC NEAR
	mov	al,','
	jmp	PushTempChar
PushTempComma ENDP

PUBLIC	PopPushCommaSpc
PopPushCommaSpc PROC NEAR
	call	PopRootPushTemp		;move exp from root to temp stk
PopPushCommaSpc ENDP
	;fall into PushTempCommaSpc
PUBLIC PushTempCommaSpc
PushTempCommaSpc PROC NEAR
	mov	ax,' ,'			;list ", "
	jmp	PushTempChars
PushTempCommaSpc ENDP

PUBLIC PushRootCommaSpc
PushRootCommaSpc PROC NEAR
	mov	ax,' ,'			;list ", "
	jmp	PushRootChars
PushRootCommaSpc ENDP

;list ", " if we're in COMMON/SHARED (i.e. if FBOS_DoIdCommas is set)
;
PUBLIC PushTempIdComma
PushTempIdComma PROC NEAR
	test	lsBosFlags,FBOS_DoIdCommas
	je	NoIdComma		;brif not in COMMON/SHARED stmt
	test	lsBosFlags,FBOS_NextIdComma
	je	FirstId			;brif first id in list, no leading comma
	call	PushTempCommaSpc	;output a ', '
FirstId:
	or	lsBosFlags,FBOS_NextIdComma
NoIdComma:
	ret
PushTempIdComma ENDP

;***************************************************************************
; PushTempOpChars
; Entry:
;	mpOpLsArg[opList2] = ASCII codes for 1 or 2 chars (if only 1 char,
;	high byte = 0)
;
;***************************************************************************
PUBLIC PushTempOpChars
PushTempOpChars PROC NEAR
	mov	bx,[opList2]		;bx = opcode being listed
	mov	ax,[mpOpLsArg + bx]	;ax = char(s) to be listed
	jmp	PushTempChars		;push char(s) to be listed
					; and return to caller
PushTempOpChars ENDP

PUBLIC PushRootOpChars
PushRootOpChars PROC NEAR
	mov	bx,[opList2]		;bx = opcode being listed
	mov	ax,[mpOpLsArg + bx]	;ax = char(s) to be listed
	jmp	PushRootChars		;push char(s) to be listed
					; and return to caller
PushRootOpChars ENDP

;***************************************************************************
; PushTempRwOrComma
; Purpose:
;	Used to list opcodes which list as a reserved word the first time
;	they occur in a statement, and as a comma for the 2nd-nth occurence
;	in the statement.
; Entry:
;	lsBosFlags.FBOS_NextStmtComma is 0 if this is the first time this
;	function has been called this statement.
;	ax = ORW_xxx (reserved word table offset) for res word to list
;	   if FBOS_NextStmtComma = 0
; Exit:
;	lsBosFlags.FBOS_NextStmtComma is set to 1
;	the res word or a comma node is pushed to the temp stack
;
;***************************************************************************
PUBLIC PushTempOpRwOrComma
PushTempOpRwOrComma PROC NEAR
	mov	bx,[opList2]		;bx = opcode * 2
	mov	ax,[mpOpLsArg + bx]	;ax = opcode's reserved word
PushTempOpRwOrComma ENDP
	;fall into PushTempRwOrComma
PUBLIC PushTempRwOrComma
PushTempRwOrComma PROC NEAR
	test	lsBosFlags,FBOS_NextStmtComma
	jne	EmitComma		;brif not 1st time called for this stmt
	or	lsBosFlags,FBOS_NextStmtComma
	jmp	PushTempRwSpc		;push reserved word ax
					; and return to caller
EmitComma:
	call	PushTempComma
	jmp	PushTempSpc		;output a space
					; and return to caller
PushTempRwOrComma ENDP

;push res word ax to root stack if 1st time this has been called for this stmt
PUBLIC PushStmtRwIfBos
PushStmtRwIfBos PROC NEAR
	test	lsBosFlags,FBOS_StmtRw
	jne	PushStmtRet		;brif already called for this stmt
	or	lsBosFlags,FBOS_StmtRw
	call	PushRootRwSpc		;push res word ax to root stack
PushStmtRet:
	ret
PushStmtRwIfBos ENDP

sEnd	LIST

end
