
;======================================================================
; Module: LsMain.asm - main module of 'lister' component
;
; Purpose:
;	The Lister is responsible for converting one logical line
;	of pcode to its ASCII source equivalent.  A logical line
;	may consist of several physical lines, with each physical
;	line terminated by an underscore_remark.  The pcode may
;	be in any scan-state from SS_rude to SS_executable.  The
;	pcode resides in a far (non DS) segment.  The lister's
;	main interface to the rest of BASIC is the entry point
;	ListLine().
;
;
;=======================================================================*/

	include 	version.inc
	LSMAIN_ASM = ON
	includeOnce	architec
	includeOnce	context
	includeOnce	heap
	includeOnce	lister
	includeOnce	lsint
	includeOnce	names
	includeOnce	optables
	includeOnce	parser
	includeOnce	pcode
	includeOnce	prstab
	includeOnce	qblist
	includeOnce	rtps
	includeOnce	scanner
	includeOnce	txtmgr
	includeOnce	util

	assumes DS,DGROUP
	assumes SS,DGROUP
	assumes ES,NOTHING


;-----------------------------------------------------------------------
;		NOTE NOTE NOTE NOTE NOTE
;
;  A powerful debugging aid is to set a break point at ListLine
;  just before the line of interest is to be listed.  When in
;  the breakpoint, set fDebugList non-zero.  This will cause
;  the intermediate list-node-tree to be symbolically dumped
;
;-----------------------------------------------------------------------



;-----------------------------------------------------------------------
;   Notation:
;
;   This module deals heavily with lists of nodes.
;
;   The list:
;	(a)->(b)->( )->(f)->(g)
;		   |
;		   +-->(c)->(d)->(e)
;
;   is equivalent to the representation:
;
;	[a b [c d e] f g]
;
;   Where b is a sibling of a, d is a sibling of c, and the entire list [c d e]
;   is a sibling of b.
;
;   Functions which operate on a list of nodes use the notation
;	[oldList] ==> [newList]
;   to indicate how the list is transformed.  For example, a function
;   which inserts 'newNode' into a list would be documented as:
;	[x y z]  ==>  [newNode x y z]
;
;   The way a line of pcode is listed (converted to ASCII text) is by
;   dispatching to a "ListRule" for each opcode.  These ListRules are
;   labeled LrXXX (i.e. LrBos handles opBos and LrBinaryOp handles
;   all binary operators).  These ListRule functions all work together
;   to manipulate to global lists, the "Root" list and the "Temp" list.
;   By the time the end-of-line is reached, the Root list contains
;   a complete description of the text to be listed.  The Temp list is
;   only used for 1 ListRule's temporary needs.  Nothing is passed
;   on the Temp list from one ListRule to another.
;
;   All the nodes which make up these lists physically reside within
;   the buffer bdNodes.  Since this buffer is a heap entry that can move,
;   all nodes are refered to as offsets into the buffer.  The first byte
;   of the buffer is not used, allowing 0 to represent end-of-list.
;   Nodes are of different types and lengths length.  The first field
;   in each node is LN_sib (offset to sibling node), followed by LN_type
;   (type of node).
;
; Algorithm:
;
;   ListLine consists of 2 stages.  Stage1 reads the pcode,
;   building an list-node description of the line.  Stage2
;   traverses this node description and appends ASCII text
;   to the output buffer.  Stage2 dumps the last node in the list
;   first, then the next to last, etc.
;
; Example of how an line of pcode is listed.
;
;   Original ASCII Source: a(x,y)=1+2
;   pcode: opBol opLit(1) opLit(2) opAdd
;	     opId(x) opId(y) opAIdSt(a,2) opBol/opEot
;
;   The following chart represents snapshots in time of the
;   current state of the lister's data structures during Stage1:
;
;	next opcode   oNodeRoot's tree
;	-----------   -----------------
;	opBol	      NULL
;	opLit(1)      NULL
;	opLit(2)      [1]
;	opAdd	      [2 1]
;	opId(x)       [[2 + 1]]
;	opId(y)       [x [2 + 1]]
;	opAIdSt(a,2)  [y x [2 + 1]]
;	opBol/opEot   [[2 + 1] = [) y , x ( a]
;
;  Stage2 then output's 'a', '(', 'x', ',', 'y', ')', '=', '1', '+', '2'
;
;-----------------------------------------------------------------------


sBegin	RT
	EXTRN	B$IFOUT:far		;binary to ASCII numeric conversion
	EXTRN	B$FHEX:far		;binary to ASCII base 16 conversion
	EXTRN	B$FOCT:far		;binary to ASCII base 8 conversion
sEnd	RT

sBegin	DATA
;-----------------------------------+
; Inter-component Global variables: |
;-----------------------------------+
PUBLIC	otxListNext, otxListNextInc, cLeadingSpaces
otxListNext	DW 0	;text offset to opBol for next line to be listed
			; by ListLine().  Set by ListLine at exit.
otxListNextInc	DW 0	;text offset to opBol for next line after current
			; line, even if it is from an included file.
			; Set by ListLine at exit.
cLeadingSpaces	DB 0	;Could of leading spaces in listed line
			; Set by ListLine at exit.

PUBLIC	otxLsCursor			;Public to non-lister components
PUBLIC otxLsCursorTmp, ndLsCursor, colLsCursor ;PUBLIC to modules w/i lister
otxLsCursor	DW 0	;See ListLine header for description
otxLsCursorTmp	DW 0	;See ListLine header for description
ndLsCursor	DW 0	;node equivalent to otxLsCursor
colLsCursor	DW 0	;column equivalent to otxLsCursor
lnTypeFindSave	DB 0

PUBLIC	fLsIncluded
fLsIncluded	DB 0	;set TRUE if this line is from an $INCLUDE file
			; (i.e. opInclude was found in it).  This allows
			; ASCII Save to not send it to the file being saved.

;flags which get reset each beginning of line
PUBLIC	lsBolFlags
lsBolFlags DB 0		;FBOL_xxx masks

;flags which get reset each beginning of stmt
PUBLIC	lsBosFlags, lsBosFlags2, lsBosFlagsWord
lsBosFlagsWord LABEL WORD
lsBosFlags DB 0		;FBOS_xxx masks
lsBosFlags2 DB 0	;FBOS2_xxx masks

;variables used by ChkLineWrap to insert _<CrLf> in lines longer than 255 bytes
;
colLastTok	DW 0
colLastLine	DW 0

PUBLIC	fLsDynArrays
fLsDynArrays DB 0	;Initialized to FALSE by AsciiSave, set TRUE when
			; $DYNAMIC is listed, set FALSE when $STATIC is listed
			; Tested by AsciiSave.

QUOTE	=	022H			;quote char (")

;--------------------------+
; Lister Local variables:  |
;--------------------------+
CB_NODES_MIN	equ	200	;never let node buffer get < 200 free bytes
CB_NODES_GROW	equ	400	;when it does, grow it by 400
				;Note: this isn't because one node can be
				; 200 bytes long, its because one dispatch
				; can produce many nodes, which together
				; can be 200 bytes in length.  For example,
				; CALL x (x1,x2,x3,...,x50)

;bdNodes is a table descriptor for a growable buffer in the table-heap used
; to accumulate the generated listing.	It contains node entries
; which represent current line being listed.

	EVEN
PUBLIC	bdNodes
bdNodes bd	<0,0,0>


PUBLIC	fGotBol
fGotBol DB	0			;non-zero after 1st opBol
					;UNDEFINED if no leading spaces on line
					;else count of leading spaces

;oNodeRoot is an offset (initially NULL) into bdNodes for the node which
; is the root of a tree of nodes used to represent current line being listed.
;
PUBLIC	oNodeRoot
oNodeRoot DW	0

;oNodeTemp is an offset (initially NULL) into bdNodes for the node which
; is the root of a temporary tree of nodes used to construct complex
; nodes which will eventyally be pushed onto oNodeRoot's list.
;
PUBLIC	oNodeTemp
oNodeTemp DW	0

PUBLIC	opList2, opList
opList2 DW	0			;2 * opcode being listed.  Used
					; by individual listers to index
					; tables of opcode related data
opList DW	0			;value of current opcode before 
					;masking with OPCODE_MASK


PUBLIC	cLsArgs
cLsArgs DB	0			;temporary for counting down args


sEnd	DATA

subttl	List Debugging Functions




sBegin	LIST
assumes CS,LIST

;===========================================================================
;		Outer Loop of Lister
;===========================================================================

;***************************************************************************
; ushort FAR ListLine(otx, pbdDst)
;
; Purpose:
;	Converts one line of pcode to its ASCII source equivalent.
;	The pcode may be in any scan-state from SS_rude to SS_executable.
;	The pcode resides in a far (non DS) segment indicated by grsCur.oRsCur.
;	The buffer pointed to by pbdDst is grown if necessary EXCEPT FOR
;	THE SPECIAL CASE when pbdDst->cbLogical < 80, in which case,
;	the output is truncated.  This special case is for listing WATCH
;	expressions, which list to a static buffer that can't be grown.
;	All normal callers call this with pbdDst->cbLogical >= 80.
;
; Entry:
;	parm1: ushort oText - the offset into the current text table for
;		for the 1st opcode of the line to be listed.
;	parm2: char *pbdDst - points to destination buffer to
;		filled with the resulting ASCII source.
;	The global variable otxLsCursor = text offset for stmt to isolate
;	   columns for.  Causes column for otxLsCursor to be returned in dx.
;
; Exit:
;	returns UNDEFINED if out-of-memory-error
;	else returns actual number of bytes listed to pbdDst.
;	Text in pbdDst is 0-byte terminated (0 not included in byte count)
;	The global variable 'otxListNext' = text offset to opBol for next
;	   line to be listed by ListLine.  Set by ListLine at exit to speed
;	   up actions like listing 1 screen full, or ASCII Save.
;	The global variable 'otxListNextInc' is set to text offset to opBol
;	   for next line after current line, even if it is from an included
;	   file.
;	dx is set to UNDEFINED if otxLsCursor is not within this line.
;	   Otherwise, it represents the column offset into the ASCII
;	   output buffer where the opcode for otxLsCursor is.
;	   This is used by the User Interface code so it can
;	   highlight the current statement and position the cursor to errors.
;	The global variable 'fLsIncluded' is set TRUE if this line
;	   is from an $INCLUDE file (i.e. opInclude was found in it).
;	   This allows ASCII Save to not send it to the file being saved.
;	cLeadingSpaces is set to the number of leading spaces 
;	   that were on the line.
;
;***************************************************************************

cProc	ListLine <PUBLIC,NODATA,FAR>,<si,di>
	parmW	otxParm
	parmW	pbdDst

	localV	numBuf,8		;holds number for B$IFOUT
	localW	spSave
	localW	pbDstWarning
	localW  hTxdCurSeg		;handle of current text seg 
cBegin	ListLine
	mov	Word Ptr [lsBosFlagsWord],NULL ; clear any leftover flags
ListLineRestart:
	mov	[spSave],sp		;save for restart-ability
	mov	si,[otxParm]		;si = offset for 1st opcode to list
	sub	di,di			;di = offset to 1st node to output
	inc	di			;1st node must not be 0, because
					; we use NULL to indicate end-of-list.
					; This is much cheaper than using
					; UNDEFINED to indicate end-of-list.

	sub	ax,ax			;ax = 0
	mov	[fGotBol],al		;so we can stop at 2nd opBol
	mov	[fLsIncluded],al	;default to FALSE
	mov	[oNodeRoot],ax		;oNodeRoot = 0
	mov	[oNodeTemp],ax		;temp stack head = 0
	dec	ax			;ax = UNDEFINED
	mov	[ndLsCursor],ax
	mov	[colLsCursor],ax

	mov	dx,[otxLsCursor]
	mov	[otxLsCursorTmp],dx
	cmp	dx,si
	jae	StmtInLine		;brif stmt is in or beyond line to list
					; or if otxLsCursor == UNDEFINED
	mov	[otxLsCursorTmp],ax	;otxLsCursorTmp = UNDEFINED
StmtInLine:

	cmp	[bdNodes.BD_pb],NULL
	jne	GotBuffer		;brif we already have node buffer

;	Always allocate the node buffer if this is for compiler list support.

	;Allocate the buffer which holds nodes
	; BdAlloc(&bdNodes, 0, IT_NO_OWNERS)
	PUSHI	ax,<dataOFFSET bdNodes>
	PUSHI	ax,CB_NODES_GROW	;initial size of nodes buffer
	PUSHI	ax,<IT_NO_OWNERS>;heap type
	call	BdAlloc
	or	ax,ax
	je	J1_ListErrExit		;brif out-of-memory error
GotBuffer:

GetTextSegAddr:
	GETSEG	es,[txdCur.TXD_bdlText_seg],,<SIZE,LOAD> ;[4]
					;es = segment for current text tbl
	jmp	SHORT Stg1Loop

GetRoom:
	call	GrowBdNodes		;grow node buffer
	jne	GotRoom			;branch if not out-of-memory
J1_ListErrExit:
	DJMP	jmp SHORT ListErrExit

;We created node for token of interest, and it is on top of Root stack.
;Replace topNode with [<topNnode> <LNT_CURSOR node>], so Stage2 loop
;will save column of this token.
;
SetNdLsCursor:
	mov	[ndLsCursor],di
	mov	[otxLsCursorTmp],UNDEFINED ;make sure we don't branch here again
	jmp	SHORT SetNdRet

; Stage1:
;   The outer loop of Stage1 fetches the next opcode, maps it
;   to a function, and jumps to the function, which jumps
;   back to  the top of the loop.  The function for the 2nd encountered
;   opBol/opEot terminates this loop by not jumping back to
;   the top of the loop.  If the text is in SS_Executable
;   state, after fetching the executor, it maps to the opcode
;   associated with this executor.  The functions dispatched
;   to by this loop can expect:
;
;	DS -> DGROUP
;	ES -> the segment with the current text table
;	SI -> the next opcode to be fetched,
;	DI -> next free byte in buffer described by bdNodes.
;
;   The current text table is locked in order to avoid the need to continually
;   update es after each FAR call.
;
;THE FOLLOWING IS TRUE FOR QBx but not EB [11]
;   Stage1's outer loop guarentees at least CB_NODES_MIN free bytes
;   in the buffer, which is large enough for all the nodes that could
;   be emitted by a single dispatch.  Any dispatches that could require
;   more than this call GrowBdNodes themselves.
;   The outer loop is the only place during Stage1 which
;   grows bdNodes, which is the only time heap movement can
;   occur.  When it does this, it fixes up ES and DI.
;
PUBLIC	Stg1Loop
Stg1Loop:
	DbAssertRel [oNodeTemp],e,0,LIST,<node found on temp stack at Stg1Loop>

	;make sure bdNodes > CB_NODES_MIN bytes, updating ES for movement
	
	mov	ax,CB_NODES_MIN 	;ax = min free we need
	add	ax,di			;ax = size we need buffer to be
	sub	ax,[bdNodes.BD_cbLogical] ;ax = free space
	jnc	GetRoom 		;brif not enough free bytes exist
GotRoom:

	cmp	si,[otxLsCursorTmp]
	jae	SetNdLsCursor		;brif caller wants to know column
					; of token for the last opcode listed.
					; Control returns to SetNdRet.
SetNdRet:
	lods	WORD PTR es:[si]	;ax = opcode
	cmp	[txdCur.TXD_scanState],SS_EXECUTE
	jne	GotOpcode
	;convert to opcode since text table's scan state is SS_EXECUTE
	xchg	bx,ax			;bx = executor's adr
	GetSegAddr CODE		 	;ax = adr of CODE seg
	mov	ds,ax			;ds = adr of CODE seg
	mov	ax,[bx-2]		;ax = executor's opcode
	push	ss			
	pop	ds			;restore ds = DGROUP
GotOpcode:
	mov	[opList],ax		;save for individual list rules
	and	ah,HIGH OPCODE_MASK	;upper bits sometimes used for operands
	mov	bx,ax			;bx = opcode
	shl	bx,1			;bx = opcode * 2
	mov	[opList2],bx		;save index for individual list rules
	jmp	[mpOpLister + bx]	;dispatch to lister function
					;function will branch back to Stg1Loop
					; or Stage2

;Some serious error has occured while listing (like out-of-memory)
; return error code
;
ListErrExit:
	mov	ax,UNDEFINED		;return error result
	jmp	ListLineExit		;return ax

;!!!WARNING!!!  No heap movement can occur during Stage2, because
; we convert all bdNode offsets to pointers while reversing the direction
; of the list. 
;This is not a problem in EB since the node buffer is static.
;
; Stage2:
;   Taking the tree which was built in step1, do a post-order traversal
;   dispatching to a special function for each node type.  Each of these
;   functions appends text to the current output line.
;   global register conventions:
;
; Stage2Inc is branched to when the terminating bol is for an included
;   line.  In this case, otxListNextInc has already been set.
;   
;
PUBLIC	Stage2, Stage2Inc
Stage2:
	call	GrowBdNodes		;this test will ensure that we
					;  have not written past end of buffer
	mov	[otxListNextInc],si	;save it for caller
Stage2Inc:
	mov	[otxListNext],si	;save it for caller


	;If there was a node that contained token of interest, replace
	;the node type with LNT_CURSOR (similar to a break-point), so
	;we don't have to compare at every stage2 dispatch.
	
	mov	bx,[ndLsCursor]
	inc	bx
	je	NoNdLsCursor		;brif line doesn't contain otxLsCursor
	add	bx,[bdNodes.BD_pb]	;convert offset to ptr
	mov	al,LNT_CURSOR
	xchg	[bx+LN_type-1],al
	mov	[lnTypeFindSave],al
NoNdLsCursor:




	mov	di,[pbdDst]		;di points to destination buffer dsc
	mov	ax,[di.BD_cbLogical]	;ax = max size of buffer
	mov	di,[di.BD_pb]		;di points to destination buffer
	mov	[colLastTok],di
	mov	[colLastLine],di
	add	ax,di			;ax points beyond end of buffer
	sub	ax,42d			;42 bytes is enough for an id, reserved
					; word, number, or char[s] node
					; Spaces/String/Col nodes check
					; for themselves
	DJMP	jc ListDstFull		;brif dst doesn't even have 42 bytes
	mov	[pbDstWarning],ax
	mov	si,[oNodeRoot]		;si = offset to root node
	or	si,si
	je	Stg2Done		;brif nothing to list
	sub	ax,ax			;prev node = NULL
	push	ax			;this will stop EndOfListLoop
;si=offset to start of list of nodes, to be listed in reverse order, i.e. last
;   node in list will be listed first, start of list will be listed last.
FindEndOfList:
	sub	cx,cx			;prev node = NULL
;si = offset to current node,
;cx points to previous node (if start of list, cx = 0),
;Traverse si's list to the end, then start listing nodes in reverse order
;
EndOfListLoop:
	add	si,[bdNodes.BD_pb]	;convert offset to ptr
	xchg	cx,LN_sib[si]		;cx = offset to next node (if any)
					;swap ptr from prev->next to next->prev
					; so we can work our way back
	jcxz	ListNodeSi		;brif we're at the end-of-list
	xchg	cx,si			;cx points to prev node
					;si = offset to current node
	jmp	SHORT EndOfListLoop

;si points to last node in list, list it, then list all the nodes which
; came before it in the linked-list
;di points to destination of next ASCII byte to be listed
;
DbPub	ListNodeSi
ListNodeSi:
	cmp	di,[pbDstWarning]
DJMP	jae	ListDstFull		;brif destination buf almost full
	mov	bl,LN_type[si]		;bx = node type
DispNodeBl:
	sub	bh,bh			;bh = 0
	jmp	[mpNodeLister + bx]	;dispatch to lister function
					;function will branch back to Stg2Cont

;si still points to node which was just listed.
; now, advance to previous sibling to be listed and list it.
;
DbPub	Stg2Cont
Stg2Cont:
	mov	si,LN_sib[si]		;si points to left sibling of node
	FLoadActive
	jne	ChkLineWrap		;see if _CrLf needs to be inserted
					; for a line > 250 chars long
					; (so BASCOM can read it)
					; jumps back to either LineWrapRet
LineWrapRet:
	or	si,si			; we just listed (0 if start-of-list)
	jne	ListNodeSi		;brif not at start of list yet
EndOfList:
	pop	si			;recover node stacked by ListChildList:
	or	si,si			;test for stopper pushed just before
					; FindEndOfList:
	jne	Stg2Cont		;brif still more to list
					;else, we've reached the 0 stopper
					; pushed by ListChildList

;NOTE: This is branched to by StaticBufFull
Stg2Done:
	sub	ch,ch			;clear high byte
	mov	cl,[fGotBol]		;cl = number of leading spaces or
					;UNDEFINED.
	inc	cl			;check for UNDEFINED
	jz	NoLeadingSpc		;return 0 if no leading spaces
	dec	cl			;return number of leading spaces
NoLeadingSpc:
	;Many opcode listers leave a space at the end, in case they are
	;followed by something else.  If the last char in the line is
	;a space, and the line does not contain only white space,
	;eliminate the last space, and then 0-byte terminate the line.
	
	mov	ax,di			;ax points beyond last destination byte
	mov	bx,[pbdDst]
	sub	ax,[bx.BD_pb]		;ax = number bytes listed to buffer
	je	NoTrailSpc		;brif 0 bytes listed
	cmp	BYTE PTR -1[di],' '
	jne	NoTrailSpc		;brif last byte is not " "
	cmp	ax,cx			;is the line blank?
	je	NoTrailSpc		;brif so, preserve trailing space
	dec	di			;eliminate trailing space
	dec	ax			;decrement return value (byte count)
NoTrailSpc:
	mov	BYTE PTR [di],0 	;0-byte terminate text
ListLineExit:				;return ax
	push	cx			;save leading space count
	push	ax			;save byte count
	mov	bx,[pbdDst]		
	call	SetLsCursor		;dx = col equivalent to otxLsCursor
	pop	ax			;ax = byte count
	pop	cx			;cx = leading space count
	mov	[cLeadingSpaces],cl	;record leading space count in 
					;	static
;NOTE: we can't release space held by bdNodes, or else we risk
;causing an out-of-memory error while trying to draw the debug
;screen, which would be an infinite loop.  If necessary, we could
;reset it in ParseNewInit
;;;	mov	[bdNodes.BD_cbLogical],0 ;Reset the buffer which holds nodes

cEnd	ListLine

;******
;NOTE
; Code below this point accesses variables on ListLine's BP frame.
; Don't insert any CEND macros between here and the next CEND.
;
;******
; ListDstFull
;
;Branched to when destination buffer is almost full
;Grows destination buffer (if possible) and tries again from the start
;of ListLine.  We have to do it from the start, since no heap movement
;can occur during stage 2 (for speed of typical case).
;
;For RELEASE code in EB we just assume that when we get here it must be
;  
ListDstFullDS:				
ListDstFull:
	mov	sp,[spSave]
					; destination buffer
	mov	bx,[pbdDst]		;bx points to destination buf desc.
	mov	ax,[bx.BD_cbLogical]
	cmp	ax,80d
	jb	StaticBufFull
	add	ax,128d			;try 128 bytes more
	push	bx			;pass pbdDst to BdRealloc
	push	ax			;pass cbNew to BdRealloc
	call	TxtGrowPsSrcEmScratch	;make sure bdEmScratch and
					;ps.bdpSrc both get updated
	or	ax,ax
	jne	NotOm			;brif not out-of-memory
	jmp	ListErrExit		;return UNDEFINED
NotOm:
	jmp	ListLineRestart

StaticBufFull:

	jmp	Stg2Done		;0-terminate line and return to caller

;We're doing an ASCII Save
;see if _<space><CR><LF> needs to be emitted for a line > 250 chars long
; (so BASCOM can read it)
; si points to node for next token to list
; di = column we're about to write next token to
; [colLastTok] = column we wrote last token to (initially 0)
; [colLastLine] = column which began last physical line (initially 0)
;
DbPub ChkLineWrap
ChkLineWrap:
	.errnz	LNT_CHAR_TOK	- 0
	.errnz	LNT_STR		- 2
	.errnz	LNT_ENSTR	- 4
	.errnz	LNT_CSSTR	- 6
	.errnz  LNT_LITSTR	- 8	
	;The following node types start lexical tokens
	.errnz	LNT_ONAM	- 10
	.errnz	LNT_LIST	- 12
	.errnz	LNT_RW		- 14
	.errnz	LNT_SPACES	- 16
	.errnz	LNT_NUM 	- 18
	.errnz	LNT_COL 	- 20
	.errnz	LNT_CHAR	- 22
	.errnz	LNT_CURSOR	- 24


	or	si,si
	je	EndOfList1		;brif end of linked list of nodes
	cmp	LN_type[si],LNT_ONAM
	jb	J1_LineWrapRet		;brif this node does not necessarily
					; begin a lexical token.  For example:
					; "string" = 3 nodes: char,str,char
					; X$ = 2 nodes: oNam,char
EndOfList1:
	mov	[colLastTok],di
J1_LineWrapRet:
	jmp	LineWrapRet

;***************************************************************************
; ChkDstFull
; Purpose:
;	Make sure there's room in destination buffer for a new item.
;	If pbdDst.cbLogical < 80, buffer is static and cannot be grown,
;	just truncate listing  (used by WatchName)
; Entry:
;	di = current pointer into destination buffer
;	cx = number new bytes needed in destination buffer
;	[18]for EB ds need not point to DGROUP. If there is no room then DS 
;		is restored to DGROUP before restarting ListLine.
; Exit:
;	If there's room, this function returns, else, ListLine
;	is restarted after growing buffer.
; Preserves:
;	all registers except flags and ax
;
;***************************************************************************
assumes	DS,NOTHING			
DbPub	ChkDstFull
ChkDstFull PROC NEAR
	mov	ax,di			;ax = current dest ptr
	add	ax,cx			;ax = result dest ptr (end of string)
	jc	ListDstFullDS		;brif wrapped past FFFF 
					;		(buf too small)
	cmp	ax,[pbDstWarning]
	jae	ListDstFullDS		;brif string too big - 
					;		grow dst buffer
	ret
ChkDstFull ENDP
assumes DS,DGROUP			

;***************************************************************************
; GrowBdNodes
; Purpose:
;	Grow the temporary buffer filled by Stg1Loop if necessary.
; Entry:
;	di = current offset into bdNodes buffer
; Exit:
;	ax = zero if out-of-memory (or out of static buffer space),
;	     condition codes set accordingly
;	es = segment adr of current text table
; Preserves:	cx
;
;***************************************************************************
PUBLIC	GrowBdNodes
GrowBdNodes PROC NEAR
	mov	ax,CB_NODES_MIN 	;ax = min free we need
	add	ax,di			;ax = size we need buffer to be
	sub	ax,[bdNodes.BD_cbLogical] ;ax = free space
	jc	GrowExit		;brif enough free bytes exist
					;ax is nonzero, psw.ZR is FALSE
	push	cx			;save caller's cx
	PUSHI	ax,<dataOFFSET bdNodes>
	PUSHI	ax,CB_NODES_GROW
	call	BdGrow
	GETSEG	es,[txdCur.TXD_bdlText_seg],,<SIZE,LOAD> ;[4]
					;es = segment for current text tbl
	pop	cx			;restore caller's cx
	or	ax,ax			;test BdGrow's return value
GrowExit:
	ret
GrowBdNodes ENDP

sEnd	LIST
sBegin	DATA

mpNodeLister LABEL WORD
	DW	LISTOFFSET ListCharsNode
	DW	LISTOFFSET ListStrNode
	DW	LISTOFFSET ListEnStrNode
	DW	LISTOFFSET ListCsStrNode
	DW	LISTOFFSET ListLitStrNode 
	DW	LISTOFFSET ListONamNode
	DW	LISTOFFSET ListChildList
	DW	LISTOFFSET ListRwNode
	DW	LISTOFFSET ListSpacesNode
	DW	LISTOFFSET ListNumNode
	DW	LISTOFFSET ListColNode
	DW	LISTOFFSET ListCharsNode
	DW	LISTOFFSET ListCursorNode
.errnz	LNT_CHAR_TOK	- 0
.errnz	LNT_STR		- 2
.errnz	LNT_ENSTR	- 4
.errnz	LNT_CSSTR	- 6
.errnz	LNT_LITSTR	- 8
.errnz	LNT_ONAM	- 10
.errnz	LNT_LIST	- 12
.errnz	LNT_RW		- 14
.errnz	LNT_SPACES	- 16
.errnz	LNT_NUM 	- 18
.errnz	LNT_COL 	- 20
.errnz	LNT_CHAR	- 22
.errnz	LNT_CURSOR	- 24

sEnd	DATA
sBegin	LIST
assumes CS,LIST

;===========================================================================
;		Stage 2 Node Listing Functions
;
; This section contains one routine for listing each node type.
; On Entry, they can all assume
;	DS points to DGROUP segment
;	DI points to next destination byte (buffer is within DS)
;	SI points to node being listed
;	ES contains garbage
;
;===========================================================================

;***************************************************************************
; ListChildList
; Purpose:
;	This node has no atomic ASCII source directly associated with it,
;	i.e. it is a hierarchy node which exists just to hold children
;	to sibling nodes.
;
;***************************************************************************
ListChildList:
	push	si			;gets popped by EndOfList:
	mov	si,WORD PTR LN_val_list[si]
					;si = offset to start of child list
	jmp	FindEndOfList		;recursively list it

;***************************************************************************
; ListONamNode
; Purpose:
;	Copy the content of an LNT_ONAM node to the output buffer.
;	Value is a 16 bit name table offset for identifier associated with
;	this node.
;
;***************************************************************************
ListONamNode:
	push	di			; pass dest byte ptr to CopyONamPb
	push	word ptr [si.LN_val_oNam] ; pass oNam to CopyONamPb
	call	CopyONamPb		;copy name to destination buffer
					;ax = # bytes in name
	add	di,ax			;update destination ptr
	jmp	Stg2Cont

;***************************************************************************
; ListCursorNode
; Purpose:
;	Return column offset for this node in colLsCursor.
;	It represents the column that corresponds with the opcode
;	at text offset otxLsCursor.
;
;***************************************************************************
ListCursorNode:
	mov	ax,di			;ax points to next destination byte
	mov	bx,[pbdDst]
	sub	ax,[bx.BD_pb]		;ax = number bytes listed to buffer
	mov	[colLsCursor],ax
	mov	bl,[lnTypeFindSave]
	jmp	DispNodeBl

;***************************************************************************
; ListCharsNode
; Purpose:
;	Copy the content of an LNT_CHAR node to the output buffer.
;	Value is 0, 1 or 2 ASCII characters associated with this node.
;
;***************************************************************************
ListCharsNode:
	mov	ax,WORD PTR [si.LN_val_char]
	cmp	ax,"OT"
	je	GotTo
CharLoop:
	or	al,al
	je	NoChar			;brif NULL char in this node
CharLoop1:
	mov	[di],al 		;copy char to destination buffer
	inc	di			;can't use stosb because ES <> DS
	mov	al,ah			;al = 2nd char (if any)
	sub	ah,ah			;don't output more than 2 chars
	jmp	SHORT CharLoop
NoChar:
	jmp	Stg2Cont

;It is space-cheaper to put spaces around TO here than in
;DIM's Stage1 code.
;
GotTo:
	mov	[di],"T "		;list " T"
	inc	di			;can't use stosb because ES <> DS
	inc	di
	mov	ax," O"			;list "O "
	jmp	SHORT CharLoop1


;***************************************************************************
; ListRwNode
; Purpose:
;	Copy the content of an LNT_RW node to the output buffer.
;	value is a 16 bit reserved word table offset for
;	reserved word or special character associated with this node.
;
;***************************************************************************
ListRwNode:
	jmp	FAR PTR ListRwNodeCP	

sEnd	LIST				

sBegin  CP
assumes	CS,CP				


DbPub	ListRwNodeCP
ListRwNodeCP:				
	push	si			;preserve si
	mov	ax,ds			;don't use push, need speed
	mov	es,ax			;es = DGROUP
	mov	ax,WORD PTR [si.LN_val_orw]
	mov	si,ax			;si = resword offset + 1st letter
	mov	al,ah			;al = (1st letter - 'A') * 4
	xor	ah,ah			
	shr	al,1			
	shr	al,1			;al = 1st letter - 'A'
	mov	bx,ax			
	shl	bx,1			;bx = word offset into tRw table
	add	al,'A'			;al = 1st letter
	stosb				;store 1st letter of res word
	and	si,03FFH		;si = offset into res word table
	mov	ax,cs			;ax = current address of CP segment
	mov	es,ax
	assumes ES,CP

	add	si,es:tRw[bx]		;convert offset to ptr
	
	call	GetRwFromTabCP		;cx = size of res word's name
					;dx = size of res word's atr block
					;si points to 1st letter of name
	assumes ES,NOTHING
	mov	ax,es			;don't use push, need speed
	mov	ds,ax			;ds = CP segment
	assumes DS,CP
	mov	ax,ss			;don't use push, need speed
	mov	es,ax			;es = DGROUP
	rep movsb			;copy reserved word from CP segment
					; to DGROUP table
	lodsb				;al = res word's flags
	test	al,RWF_STR
	je	NotStrRw		;brif res word does not end with '$'
	mov	al,'$'
	stosb				;list "$"
NotStrRw:
	mov	ax,ss			;don't use push, need speed
	mov	ds,ax			;ds = DGROUP
	assumes DS,DGROUP
	pop	si			;restore si
	jmp	FAR PTR Stg2Cont	


;*************************************************************************
; GetRwFromTabCP
; Purpose:
;	extract an entry from the reserved word table
; Entry:
;	si points to first char of entry in reserved word table 
;		which is the second char of the reserved word since the
;		first char is not stored in the table
;	for GetRwFromTabList:
;		es = segment address of reserved word table (i.e. CP) [09]
; Exit:
;	cx = size of res word's name
;	dx = size of res word's atr block
;	si points to 1st byte of res word name
;	preserves bx,di,es
;
; GetRwFromTab is a FAR entry point. It has the following DIFFERENCE:
;	es is not expected to be anything on entry,
;	on exit ax = size of res word's name instead of cx
;	Does not guarantee that bx and es are preserved
;*************************************************************************
PUBLIC	GetRwFromTabCP
GetRwFromTabCP PROC NEAR
	lods	BYTE PTR es:[si]	;al = (cbNam << 4) + cbAtr
	inc	al			;test al for FF
	jz	CbsInWord		;brif cbNam and cbAtr stored
					;	in following word
	dec	al			;restore al = Cbs
	sub	ah,ah			;ax = (cbNam << 4) + cbAtr
	mov	dx,ax			;dx = (cbNam << 4) + cbAtr
	shr	ax,1
	shr	ax,1
	shr	ax,1
	shr	ax,1			;ax = cbNam
	mov	cx,ax			;cx = cbNam
	and	dl,15			;dl = cbAtr
	ret

CbsInWord:				
	lods	WORD PTR es:[si]	;al = #bytes of attributes
					;ah = #bytes in reserved word
	mov	cl,ah
	xor	ch,ch			;cx = #bytes in reserved word
	xor	ah,ah
	xchg	dx,ax			;dx = #bytes of attributes
	ret				
GetRwFromTabCP ENDP

sEnd	CP				
sBegin	LIST				
assumes CS,LIST				

;***************************************************************************
; ListStrNode/ListLitStrNode
;	Rewritten for revision [18]
; Purpose:
;	Copy the content of an LNT_STR node to the output buffer.
;	Value is a 16 bit offset followed by 16 bit count
;	which identifies where the string is in the text table.
;	
;       Special processing is needed for listing LitStrNode's 
;	to handle listing '"' in a string.
;
; Entry:
;	bx = LNT_STR or LNT_LITSTR
;
;***************************************************************************
ListLitStrNode:
DbPub ListStrNode
ListStrNode:
	push	si			;preserve si
	mov	cx,WORD PTR [si.LN_val_cbStr]
					;cx = length of string
	mov	si,WORD PTR [si.LN_val_oStr]
					;si = offset into text table to string
	call	ChkDstFull		;make sure there's room for cx bytes
					; in dest buffer.  If not, don't return
	GETSEG	ds,[txdCur.TXD_bdlText_seg],,<SIZE,LOAD> ;[4]
					;ds = seg for current text table
	push	ss			
	pop	es			;es = DGROUP
	rep	movsb
	push	es
	pop	ds			;restore ds = DGROUP
LSNZeroLenExit:
	pop	si			;restore si
	jmp	Stg2Cont


;***************************************************************************
; ListEnStrNode
; Purpose:
;	Decode and copy the content of an LNT_ENSTR node to the output buffer.
;	Value is a 16 bit offset followed by 16 bit count
;	which identifies where the encoded string is in the text table.
;	An encoded string consists of standard ascii text, with character
;	runs of > 3 identical chars contained in a "compression" record.
;	The compression record, and format is described in prsutil.asm,
;	and basically consists of a flag byte, followed by a byte containing
;	the repetition factor, and the final byte is the character which
;	was repeated.
;
;***************************************************************************
DbPub	ListEnStrNode
ListEnStrNode:
	push	si			;preserve si
	mov	cx,WORD PTR [si.LN_val_cbEnStr]
					;cx = length of string
	jcxz	ListEnStrExit		;brif no text to list
	mov	si,WORD PTR [si.LN_val_oEnStr]
					;si = offset into text table to string
	call	ChkDstFull		;make sure there's room for cx bytes
					; in dest buffer.  If not, don't return
	GETSEG	ds,[txdCur.TXD_bdlText_seg],,<SIZE,LOAD> ;[4]
					;ds = seg for current text table
	push	ss
	pop	es			;es = DGROUP
EnStrLoop:
	lodsb				;get char
	cmp	al,STR_EncodedText	;is this the start of a compression
	je	ExpandText		; record?  brif so
	stosb				;list char
EnStrCont:
	loop	EnStrLoop		;continue until all chars listed

	push	es
	pop	ds			;restore ds = DGROUP
ListEnStrExit:
	pop	si			;restore si
	jmp	Stg2Cont

ExpandText:
	lodsw				;al = cbChars, ah = char
	dec	cx
	dec	cx			;adjust for size of compression record
	push	cx			;save cbCompressed
	mov	bx,di			;bx = current dest ptr
	add	bx,cx			;bx = result dest ptr (end of string)
					; before expanding text
	mov	cl,al
	sub	ch,ch			;cx = cbExpand
	mov	al,ah			;al = char
	add	bx,cx			;bx = result dest ptr after expansion
	jc	J1_ListDstFull		;brif wrapped past FFFF (buf too small)
	cmp	bx,es:[pbDstWarning]
	jae	J1_ListDstFull		;brif string too big - grow dst buffer

	rep	stosb			;blast out the encoded char string
	pop	cx			;recover cbLeft to list
	jmp	short EnStrCont 	;continue listing encoded string

J1_ListDstFull:
	push	es
	pop	ds			;reset ds to DGROUP
	jmp	ListDstFull


;***************************************************************************
; ListCsStrNode
; Purpose:
;	Copy the content of an LNT_CSSTR node to the output buffer.
;	Value is a 16 bit offset followed by 16 bit count
;	which identifies where the string is in the LIST segment.
;
;***************************************************************************
ListCsStrNode:
	push	si			;preserve si
	mov	si,WORD PTR [si.LN_val_CsStr]
					;si = offset into LIST segment to
					; str255 struct for string
	push	ds
	pop	es			;es = DGROUP
	push	cs
	pop	ds			;ds = code segment (LIST)
	lodsb				;al = length of string constant
					;si points to 1st byte of string
	cbw				;ax = length of string constant
					; NOTE: implies no LIST string constant
					; is longer than 128 bytes (very safe)
	xchg	cx,ax			;cx = length of string
	rep movsb			;copy string to destination buffer
	push	es
	pop	ds			;restore ds = DGROUP
	pop	si			;restore si
	jmp	Stg2Cont

;***************************************************************************
; ListSpacesNode
; Purpose:
;	Copy the content of an LNT_SPACES node to the output buffer.
;	Value is a 16 bit count of spaces
;
;***************************************************************************
ListSpacesNode:
	mov	cx,WORD PTR [si.LN_val_cbSpaces]
					;cx = number of spaces to emit
;cx = number of spaces to list
ListCol1:
	call	ChkDstFull		;make sure there's room for cx bytes
					; in dest buffer.  If not, don't return
	push	ds
	pop	es			;es = DGROUP
	mov	al,' '
	rep stosb			;store spaces in destination buffer
J1_Stg2Cont:
	jmp	Stg2Cont		;return to outer loop

;***************************************************************************
; ListColNode
; Purpose:
;	Emit white space until we get to indicated column.
;	Value is a 16 bit column to advance to.
;	High bit of argument is set if at least 1 space needs
;	to be output even if we're beyond the specified column.
;
;***************************************************************************
ListColNode:
	mov	bx,[pbdDst]
	mov	cx,[bx.BD_pb]		;cx points to destination buffer
	mov	ax,WORD PTR [si.LN_val_col]
	mov	dx,ax			;high bit of dx is set if at least
					; 1 space is to be output
	and	ah,7Fh			;ax = column to advance to
	add	cx,ax
					;cx points to column to advance to
	sub	cx,di			;subtract current column
	ja	ListCol1		;brif not already past that column
	mov	cx,1
	or	dx,dx
	js	ListCol1		;always list at least 1 space
	jmp	SHORT J1_Stg2Cont	;return to outer loop

subttl	List a numeric constant

;Table which maps LIT_xxx to runtime library's value types as follows:
;
; If the high bit is set, its a "special" number and:
;    the low 7 bits mean: 00cbbbbb where:
;        c = 0 for 16 bit integer, 1 for 32 bit integer,
;        bbbbb = base value (2, 8 or 16)
; Else the remaining 7 bits are the "runtime" value type
;    The runtime uses the convention that the low 4 bits = length of value
;
OrdConstStart 0
OrdConst LIT_I2		; % suffix
OrdConst LIT_O2		; &O prefix
OrdConst LIT_H2		; &H prefix
OrdConst LIT_I4		; & suffix
OrdConst LIT_O4		; &&O prefix
OrdConst LIT_H4		; &&H prefix
OrdConst LIT_R4		; ! suffix
OrdConst LIT_R8		; # suffix
OrdConst LIT_STR	; "xxx"

SNM_LONG EQU 40H	;indicates LONG special number

mpLtToRt LABEL WORD
	DB	VT_I2,0			;LIT_I2
	DB	084H,'O'		;LIT_O2
	DB	090H,'H'		;LIT_H2
	DB	VT_I4,'&'		;LIT_I4
	DB	0C4H,'O'		;LIT_O4
	DB	0D0H,'H'		;LIT_H4
	DB	VT_R4,'!'		;LIT_R4
	DB	VT_R8,'#'		;LIT_R8


;***************************************************************************
; ListNumNode
; Purpose:
;	List a numeric constant to the output buffer.
;
;***************************************************************************
DbPub ListNumNode
ListNumNode:
	push	si			;preserve node ptr
	GETSEG	es,[txdCur.TXD_bdlText_seg],,<SIZE,LOAD> ;[4]
					; es = seg for current text table
	xor	bh,bh
	mov	bl,[si.LN_val_clNum]	;bx = type of num (LIT_D2...LIT_LINENUM)
	mov	si,[si.LN_val_otxNum]	;es:si points to number
	cmp	bl,LIT_LINENUM
	je	ListUnsigned		;brif linenum


.errnz	LIT_I2
	or	bl,bl			;I2?
	jz	ListSigned
	shl	bx,1
	mov	ax,[mpLtToRt+bx]	;al = runtime's value type
					;ah = explicit type char
	or	al,al
	js	HexNum			;brif hex/octal/binary constant
	push	ds			;swap ds,es
	push	es
	pop	ds			;ds = seg adr of text table
	pop	es			;es = DGROUP
	mov	cx,ax			;cx = runtime's value type
	and	cx,000FH		;cx = # bytes in value
	push	di
	lea	di,[numBuf]		;di points to temp 8 byte buffer
	rep movsb			;copy value from pcode to numBuf
	pop	di			;restore di=ptr to next list byte
	push	es
	pop	ds			;restore ds = es = DGROUP

;al=runtime's value type, ah=explicit type char
CallFout:
	lea	bx,[numBuf]		;bx points to temp 8 byte buffer
	cmp	ah,'&'
	jne	NotLong
	push	ax
	mov	ax,[bx]
	cwd				;dx = 0 if ax <= 7FFF, else FFFF
	sub	dx,[bx+2]		;dx = 0 if number could be represented
					; as a short integer (i.e. '&' required)
	pop	ax
	je	NotLong			;brif '&' is necessary
	sub	ah,ah			;else list no explicit char
NotLong:
	call	far ptr ListNum		;copy ASCII number to di
NumDone:
	pop	si			;restore node ptr
	jmp	Stg2Cont		;return to outer loop

ListSigned:
	mov	si,es:[si]		;Get number to si
;	jmp	short ListUnsigned

DbPub	ListUnsigned
ListUnsigned:
;16-bit integer in si
	push	ds
	pop	es			;es=ds=dgroup
	xchg	ax,si			;put number in ax
	xor	cx,cx			;Initialize count of digits
	mov	bx,10
;While >10, divide by 10, counting digits
NextDig:
	inc	cx			;Count all digits
	cmp	ax,bx			;Less than 10?
	jb	SaveDig
	xor	dx,dx			;Extend to DWord
	div	bx			;Next digit is remainder in dx
	push	dx			;Save digit on stack
	jmp	NextDig

SaveDig:
	add	al,"0"			;Add ASCII bias
	stosb				;List a digit
	pop	ax			;Get next digit from stack
	loop	SaveDig
	xchg	si,ax			;Last thing popped was node pointer
	mov	byte ptr [di],0		;Terminate with zero
	jmp	Stg2Cont


;al & SNM_LONG is non-zero if LONG, ah = 'H', 'O', 'B' for hex,octal,binary
;es = text table's segment
;di = pointer to next byte to be listed (destination)
HexNum:
	push	ax			;save fLong
	xchg	bx,ax			;bl = fLong, bh = base char
	lods	WORD PTR es:[si]	;ax = low word of constant
	xchg	dx,ax			;dx = low word of constant
	sub	ax,ax			;default high word = 0
	test	bl,SNM_LONG
	je	NotLongHex		;branch if I2 constant
	lods	WORD PTR es:[si]	;ax = high word of constant
NotLongHex:
	mov	bl,'&'			;list '&'
	mov	[di],bx			;store &H, &O, or &B
	inc	di
	inc	di
	xchg	ax,dx			;dx:ax = I4 to output
	push	dx			;save high-word
	call	far ptr ListBaseNum
	pop	dx			;restore high-word
	pop	ax			;al = fLong
	or	dx,dx
	jne	NumDone			;brif implicitly long
	test	al,SNM_LONG
	je	NumDone			;brif implicitly short
	mov	BYTE PTR [di],'&'	;list second '&' to indicate long
	inc	di
	jmp	SHORT NumDone		;return to outer loop

;***************************************************************************
; ListBaseNum
; Purpose:
;	Copy the ASCII equivalent of a hex/octal number to a buffer
; NOTE:
;	This function assumes that the following runtime functions
;	cannot result in a runtime error: B$FCONVBASE, B$IFOUT
; Entry:
;	dx:ax = I4 to output
;	ds:di points to destination buffer
;	dh = 'H' or 'O' for hex/octal
; Exit:
;	di points beyond last byte of number (i.e. TO 0-byte terminator)
;	es = ds
;	
;***************************************************************************

cProc	ListBaseNum,<PUBLIC,FAR>,<si>

cBegin
; bh = 'H' for Hex, 'O' for Octal
; dx:ax = I4 to convert
; di points to next free byte in output buffer
;
DoConv:
	mov	cx,0F04h		;ch=mask, cl=shift count
	cmp	bh,'H'
	je	GotHex
	mov	cx,0703h		;ch=mask, cl=shift count
	DbAssertRelB bh,e,'O',LIST,<ListBaseNum called with bad dh>

GotHex:
	lea	bx,[di+12d]		;bx points to end of 12 byte buffer
	call	B$FCONVBASE		;dx points to 1st byte of result
					;bx = number of digits
					;es = ds
	mov	cx,bx			;cx = number of digits
	mov	si,dx			;bx points to 1st digit
	rep movsb			;copy ASCII string to list buffer
LBNExit:
	mov	BYTE PTR [di],0		;0 terminate result
cEnd


;***************************************************************************
; ListNum
; Purpose:
;	Copy the ASCII equivalent of a binary number to a buffer
;	Used by WATCH pcode as well as lister.
; Entry:
;	al = type of number (VT_I2, VT_I4, VT_R4, VT_R8, VT_CY (EB specific))
;	ah = explicit terminating char (i.e. %, !, &, #, @ (EB specific))
;	bx points to 1st byte of binary number (in DS)
;	di points to destination buffer (in DS)
; Exit:
;	di points beyond last byte of number (i.e. TO 0-byte terminator)
;	es = ds
;
;***************************************************************************
cProc	ListNum,<PUBLIC,FAR>,<si>
cBegin
	push	ds
	pop	es			;es = DGROUP
	push	ax			;save ah=explicit type char
					; pass B$IFOUT ptr to value in es:bx
					; pass B$IFOUT valTyp in al
	call	B$IFOUT			;bx = adr of ascii string
					;ax = byte count
	pop	dx			;restore dh = explicit type char
	cmp	BYTE PTR [bx],' '
	jne	NoSpc			;brif no leading space
	inc	bx			;skip leading space
	dec	ax
NoSpc:
	xchg	cx,ax			;cx = # bytes in ASCII string
	mov	si,bx			;si points to start of ASCII string

	;copy ascii string from BIFOUT's static buffer to result buffer
NumLoop:
	lodsb				;al = next byte to transfer
	stosb				;list it
	cmp	al,'E'
	je	Not0to9
	cmp	al,'D'
	je	Not0to9
	cmp	al,'.'
	jne	Its0to9
	cmp	dh,'#'
	je	Its0to9			;. isn't strong enough to not list #
					;else 10.5# would list as 10.5 (R4)
	cmp	dh,'@'
	je	Its0to9			;same goes for @
Not0to9:
	sub	dh,dh			;no explicit type needed, we got
					; a period, E or D
Its0to9:
	loop	NumLoop
	or	dh,dh
	je	LnExit			;brif not explicit type char needed
	mov	al,dh			;al = explicit type char
	stosb				;list it
LnExit:
	mov	BYTE PTR [di],0		;0 terminate result
cEnd

;***************************************************************************
; SetLsCursor				; - function added in this rev.
; Entry:
;	ax = number of bytes in line
;	bx = pbdDst
;	[colLsCursor] = column of interest (equivalent to otxLsCursor)
; Exit:
;	dx = column of interest (equivalent to otxLsCursor)
;
;***************************************************************************
cProc	SetLsCursor,<NEAR>,<si>
cBegin
	mov	cx,UNDEFINED		;for cheaper comparisons below
	mov	dx,[colLsCursor]
	cmp	dx,cx
	jne	NotStmtEnd		;brif colLsCursor already set
	cmp	[ndLsCursor],cx
	je	NoColCursor		;brif otxLsCursor wasn't in line
	xchg	dx,ax			;else, this is it, dx=column

;Make sure that column of interest isn't pointing to a blank,
;If it is, make first token after blank column of interest
NotStmtEnd:
	cmp	dx,cx
	je	NoColCursor		;brif otxLsCursor not in this line
	mov	si,[bx.BD_pb]		;si points to 1st byte of result
	add	si,dx			;si points to column of interest
SkipBlanks:
	lodsb				;al = byte at column of interest
	cmp	al,' '
	je	SkipBlanks
	cmp	al,0
	je	NoColCursor		;brif blanks are at end-of-line
	dec	si			;si points to 1st non blank
	mov	dx,si
	sub	dx,[bx.BD_pb]		;dx = offset to 1st non blank
NoColCursor:
cEnd


;***************************************************************************
; B$FConvBase
; Purpose:
;	Convert an I4 to ASCII Hex/Octal/Binary without causing any
;	Heap Movement.  This is needed by QBI.
; Entry:
;	BX points beyond last byte of 11 byte buffer (32 bytes for _BFBIN)
;	CH = Mask, CL = Shift count, i.e.
;	   0703 for Octal, 0F04 for Hex
;	DX:AX = number to convert
; Exit:
;	DX points to 1st byte of resulting string
;	BX = number of digits in resulting string
;	ES = DS
;
;***************************************************************************
cProc	B$FCONVBASE,<NEAR>,<DI>	
cBegin
	MOV	DI,BX		;DI points beyond destination
	XCHG	AX,BX		;DX:AX = I4 to be converted
	STD			;move from high to low
	PUSH	DS		;set ES=DS
	POP	ES
	XOR	AH,AH		;init char count

; At this point the following conditions exist:
;	AH = Character count
;	CH = Mask
;	CL = Shift count
;	DX:BX = I4 to convert
;	DI = pointer to digit buffer
; Perform the conversion by shifting DX:BX by CL bits and masm out
; unused bits with CH.	Take this number and convert to ascii char
; representing digit. Stuff the char in the buffer, bump the char
; count and continue until no non-zero digits remain.

CONVERT_LOOP:
	MOV	AL,BL		;Bring number to accumulator
	AND	AL,CH		;Mask down to the bits that count
;Trick 6-byte hex conversion
	ADD	AL,90H
	DAA
	ADC	AL,40H
	DAA			;Number in hex now
	STOSB			;Save in string
	INC	AH		;Count the digits
	PUSH	CX		;Save mask/shift count
	XOR	CH,CH		;zero out mask, leaving shift count

SHIFT_LOOP:
	SHR	DX,1		;shift low bit into carry, zero high bit
	RCR	BX,1		;rotate carry into low word
	LOOP	SHIFT_LOOP	;repeat shift count times

	POP	CX		;recover mask/shift count
	PUSH	BX
	OR	BX,DX		;is rest of I4 = 0?
	POP	BX
	JNZ	CONVERT_LOOP	;brif not, convert next digit

	CLD			;Restore direction UP
	INC	DI		;Point to most significant digit
	MOV	DX,DI		;Put string pointer in DX
	MOV	BL,AH		;Digit count in BX (BH already zero)
cEnd
sEnd	LIST

end
