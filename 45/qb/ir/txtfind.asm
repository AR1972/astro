	TITLE	txtfind.asm - Text Table Searching functions

;==========================================================================
;
;Module:  txtfind.asm - Text Table Searching functions
;System:  Quick BASIC Interpreter
;
;=========================================================================*/

	include		version.inc
	TXTFIND_ASM = ON
	includeOnce	architec
	includeOnce	context
	includeOnce	executor	;for EnStaticStructs
	includeOnce	names
	includeOnce	opcontrl
	includeOnce	opid
	includeOnce	opmin
	includeOnce	opstmt
	includeOnce	optables
	includeOnce	pcode
	includeOnce	scanner
	includeOnce	txtint
	includeOnce	txtmgr
	includeOnce	ui

	assumes	DS,DATA
	assumes	SS,DATA
	assumes	ES,NOTHING


;-------------------------------------------------------------------------
;		DATA Segment Declarations
;-------------------------------------------------------------------------

sBegin	DATA

	cbTbSearchOps = (OP_MAX/8)+1	;byte size of TbSearchOps
pOpListLast	DW 0			;cached ptr to table of opcodes from
					;last call to TxtFindOp
tbSearchOps	DB cbTbSearchOps DUP(0) ;bit packed array for TxtFindOp
					;contains a bit for each opcode in
					;the system
lnFindLast	DW UNDEFINED		;used by FindBol to reduce searching
otxFindLast	DW UNDEFINED		;used by FindBol to reduce searching

sEnd	DATA

sBegin CODE

;Table of opcodes which mark the beginning of a line
;
tOpBol	LABEL WORD
	opTabStart	BOL
	opTabEntry	BOL,opBolInclude
	opTabEntry	BOL,opBolIncludeSp
	opTabEntry	BOL,opBol
	opTabEntry	BOL,opBolLab
	opTabEntry	BOL,opBolSp
	opTabEntry	BOL,opBolLabSp
	opTabEntry	BOL,opEndProg
	opTabEntry	BOL,opEot

;Table of opcodes which mark the beginning of a stmt within a line
;
tOpBos	LABEL WORD
	opTabStart	BOS
	opTabEntry	BOS,opBos
	opTabEntry	BOS,opBosSp
	opTabEntry	BOS,opEot

;Table of opcodes which mark the beginning of a stmt or a line
;
tOpBosBol LABEL WORD
	opTabStart	BOSBOL
	opTabEntry	BOSBOL,opBos
	opTabEntry	BOSBOL,opBosSp
	opTabEntry	BOSBOL,opBol
	opTabEntry	BOSBOL,opBolLab
	opTabEntry	BOSBOL,opBolSp
	opTabEntry	BOSBOL,opBolLabSp
	opTabEntry	BOSBOL,opBolInclude
	opTabEntry	BOSBOL,opBolIncludeSp
	opTabEntry	BOSBOL,opEndProg
	opTabEntry	BOSBOL,opEot

;Table of opcodes which mark the beginning of a stmt within a line
; or start of a then/else clause for single line IF RESUME compatability.
;
tOpResume LABEL WORD
	opTabStart	RESUME
	opTabEntry	RESUME,opStIf
	opTabEntry	RESUME,opStElse
		RESUME_opIfElseMax EQU RESUME_opStElse
	opTabEntry	RESUME,opBos
	opTabEntry	RESUME,opBosSp
	opTabEntry	RESUME,opEot

;Table of opcodes which mark the beginning of a stmt or a line
; or start of a then/else clause for single line IF RESUME NEXT compatability.
tOpResumeNext LABEL WORD
	opTabStart	RESUMENEXT
	opTabEntry	RESUMENEXT,opStIf
		RESUMENEXT_opIfMax EQU RESUMENEXT_opStIf
	opTabEntry	RESUMENEXT,opBos
	opTabEntry	RESUMENEXT,opBosSp
	opTabEntry	RESUMENEXT,opBol
	opTabEntry	RESUMENEXT,opBolLab
	opTabEntry	RESUMENEXT,opBolSp
	opTabEntry	RESUMENEXT,opBolLabSp
	opTabEntry	RESUMENEXT,opBolInclude
	opTabEntry	RESUMENEXT,opBolIncludeSp
	opTabEntry	RESUMENEXT,opEndProg
	opTabEntry	RESUMENEXT,opEot

;Table of legal opcodes which may appear between a SELECT CASE and the
;   First CASE item, CASE ELSE, or END SELECT

tOpSelect label   word
	opTabStart	SEL
	opTabEntry	SEL,opBos
	opTabEntry	SEL,opBosSp
	opTabEntry	SEL,opBol
	opTabEntry	SEL,opBolSp
	opTabEntry	SEL,opBolInclude
	opTabEntry	SEL,opBolIncludeSp
	opTabEntry	SEL,opNoType
	opTabEntry	SEL,opQuoteRem
	opTabEntry	SEL,opStRem
	SEL_opValidMax	EQU SEL_opStRem 	;max valid opcode

	opTabEntry	SEL,opBolLab		;labels aren't allowed
	opTabEntry	SEL,opBolLabSp		;between SELECT / 1st item
	opTabEntry	SEL,opEot




sEnd	CODE

;-------------------------------------------------------------------------
;		CP Segment Functions
;-------------------------------------------------------------------------

sBegin	CP
assumes	cs,CP


;***
;InitSearchTable - Initialize search table for TxtFindOpcode
;
;Purpose:
;	Builds a bit packed array for the specified search opcodes.
;	Each opcode in the passed list will have a bit set in
;	the constructed table.	All other opcodes in the table
;	will have their associated bits set to 0.
;Entry:
;	di - ptr to tbSearchOps (in DGROUP) [6]
;	segCode:si - ptr to list of search opcodes.
;		The first word of the table contains the number
;		of opcodes in the table.
;Exit:
;	tbSearchOps - constructed bitpacked array of search opcodes
;
;***************************************************************************
DbPub 	InitSearchTable
cProc	InitSearchTable,<NEAR>,<si>
cBegin
	GetSegAddr CODE			;ax = current address of CODE seg
	mov	ds,ax			;ds:si = pOpcodeList

assumes ds,NOTHING 
	DbSegMoveOff			;can't allow seg movement
	push	ss
	pop	es			;es = DGROUP

	mov	cx,cbTbSearchOps	;get byte size of table

	push	di			;save table base
	sub	ax,ax			;clear previous table
	shr	cx,1			;byte count -> word count
	rep	stosw			;clear table
	jnc	EvenCount		;brif count even

	stosb				;clear the last (odd) byte
EvenCount:
	pop	di			;recover table base


	lodsw				;pick up cwOpcodes
	and	ah,7fH			;mask out Search all opcodes bit
	mov	dx,ax			;dx = count of opcodes in list

ConstructLoop:
	lodsw				;get opcode
	mov	bx,ax
	shr	bx,1			;divide opcode by 8 to get
	shr	bx,1			;table index for specified opcode
	shr	bx,1			;bx = table index for opcode
	and	al,7			;al = bit number to set in byte
	mov	cl,1
	xchg	ax,cx
	shl	al,cl			;generate bit mask for table byte
	or	es:[di+bx],al		;set bit for opcode
	dec	dx
	jne	ConstructLoop		;get next opcode

	push	ss
	pop	ds			;restore ds = DGROUP
	DbSegMoveOn			;OK to move segs again
assumes ds,DGROUP

cEnd


;*************************************************************************
; TxtFindOpcode(al:operation, otxStart, pOpcodeList)
; Purpose:
;	Given an offset into the current text table, and a
;	list of opcodes to search for, perform a search skipping
;	each opcode's operands.  As a FUTURE optimization all opBol
;	opcodes could be linked.  This would eliminate many calls to
;	this routine.
;	Note: this routine was initially a lot smaller, but SS_EXECUTE
;	and Non SS_EXECUTE searches were split for performance.  This
;	routine shows up significantly in ASCII load/Rude Scan/ and
;	CUT/PASTE/COPY operations.
; Entry:
;	[al] = 0 or more TFC_xxx flags indicating what to do
	TFC_FindNext EQU 1  ; skip 1st opcode in buffer before doing search
	TFC_FindInDS EQU 2  ;search in DS segment, not current text table
	TFC_FindExec EQU 4  ;pcode buffer is in EXECUTE scan state
;	parm1: ushort otxStart - offset into buffer to start search.
;	       [14] A value of 0 always means start at StartOtx.
;	parm2: ushort *pOpcodeList - If this parm is NULL,
;	  the current opcode is skipped.
;	  If it is not NULL, it points to a list of opcodes to search for.
;	  The first word in the list is the count of opcodes which follow.
;	  If the high bit of this first word is set, it means the table
;	  is to be searched even for opcodes which don't have the
;	  OPA_fTxtFind attribute flag set (speed optimization).
;	  NOTE: Make sure one of them is opEot to ensure that we don't
;	  search past end-of-text.
;	  NOTE: This table must reside in the CODE segment.
;	grs.fDirect, oMrsCur, oPrsCur identify the text table being
;	  searched.  txdCur describes its text table.
;	txdCur.TXD_scanState indicates text table's scan state
;
; Exit:
;	DL = txtFindIndex (global static ushort variable) = 0 if 1st opcode in
;	   list was found, 1 if 2nd opcode in list was found etc.
;	   (this is only set if parm2 is not NULL on entry)
;	AX = offset within text where one of the opcodes was found
;	pOpListLast - cached ptr to list of search opcodes.
;			(Speed opt to avoid reconstructing tbSearchOps
;			for each call to TxtFindOpcode).
;
;*************************************************************************
PUBLIC	TxtFindOpDS
TxtFindOpDS PROC NEAR
	mov	al,TFC_FindInDS
	jmp	SHORT TxtFindOpcode
TxtFindOpDS ENDP

PUBLIC	TxtFindNextOpDS
TxtFindNextOpDS PROC NEAR
	mov	al,TFC_FindNext + TFC_FindInDS
	jmp	SHORT TxtFindOpcode
TxtFindNextOpDS ENDP

;TxtFindOpExec and TxtFindNextOpExec are called by scanner when it knows
;that pcode is in SS_EXECUTE even though txdCur.scanState = SS_PARSE.
;It does this when it found an error during scan, and is backing out
;of the scan.

cProc	TxtFindOpExec,<PUBLIC,FAR>	
	parmW	otxStart		
	parmW	pOpcodeList		
cBegin	TxtFindOpExec			
	mov	al,TFC_FindExec 	
	cCall	TxtFindOpcode,<otxStart, pOpcodeList>	
cEnd	TxtFindOpExec			

DbPub TxtFindNextOpExec
TxtFindNextOpExec PROC NEAR
	mov	al,TFC_FindNext + TFC_FindExec
	jmp	SHORT TxtFindOpcode
TxtFindNextOpExec ENDP

PUBLIC	TxtFindOp
TxtFindOp PROC NEAR
	mov	al,TFC_FindExec 	
	cmp	[txdCur.TXD_scanState],SS_EXECUTE
	je	TxtFindOpcode		; brif txt tbl in executeable state

	cmp	[grs.GRS_fDirect],FALSE
	jne	TxtFindOpcode		; brif direct mode buffer (This func
					; can assume it is in executable state)
	sub	al,al
	jmp	SHORT TxtFindOpcode	;branch to TxtFindNextOp shared code
TxtFindOp ENDP

PUBLIC	TxtFindNextOp
TxtFindNextOp PROC NEAR
	cmp	[txdCur.TXD_scanState],SS_EXECUTE
	je	TxtFindNextOpExec	;brif txt table is in executable state
	cmp	[grs.GRS_fDirect],FALSE
	jne	TxtFindNextOpExec	;brif direct mode buffer (This func
					; can assume it is in executable state)
	mov	al,TFC_FindNext
TxtFindNextOp ENDP
	;fall into TxtFindOpcode

cProc	TxtFindOpcode,<NEAR>,<si,di>
	parmW	otxStart
	parmW	pOpcodeList
	localB	flags
cBegin	;TxtFindOpcode

	DbChk	ConStatStructs		;ensure static structures, else
					; txdCur structure contains garbage.
	mov	[flags],al		;remember what we need to do
	DbChk	TxdCur			;perform sanity check on txdCur

	mov	si,[pOpcodeList]	;get ptr to opcode list
	mov	di,si			;set up in case its TxtSkipOp

	or	si,si			;was an Opcode list specified
	je	NoInitTable		;brif not - skip one opcode case

	mov	di,offset dgroup:tbSearchOps
	cmp	si,[pOpListLast]	;is table already built?
	je	NoInitTable		;brif so - use it

	mov	[pOpListLast],si	;cache constructed table

	call	InitSearchTable 	;Init opcode search table
					;es:di - ptr to search opcode table
NoInitTable:
	GetSegAddr CODE			;ax = current address of CODE segment
	push	ax			;push addr of code seg.
					;  later popped into es
	DbSegMoveOff			;can't allow seg movement

	test	[flags],TFC_FindInDS
	jne	DoItInDS		;brif entry was TxtFind[Next]OpDS
					; in which case the txt table is the
					; direct mode buffer which is in DGROUP
	GetSegTxtCur			;[24] es = seg adr of current txt tbl
	push	es			; ds = seg of current txt table
	pop	ds			
assumes	ds,NOTHING

DoItInDS:
	pop	es			;es = adr of CODE seg

	;********************************************************
	;NOTE: DS register points to text table until end-of-loop
	;      ES points to CODE, where opcodes in execute state can be accessed
	;      SS still points to DGROUP, so local vars can be accessed
	;********************************************************

	mov	si,[otxStart]		;ds:si = ptr to txt table of opcode
					;to start search with
	or	di,di			;is this a TxtSkip request?
	jne	NotSkipOp		;brif not

; We just want to skip one opcode in the text table.
;	ds:si - points to opcode to skip.

	lodsw		 		;ax = current opcode from text table
	test	[flags],TFC_FindExec
	je	SkipOpNotExec		;brif scanState == SS_EXECUTABLE

; map executor to opcode
	xchg	bx,ax			;bx = executor's adr
	mov	ax,es:[bx-2]		;ax = executor's opcode
SkipOpNotExec:
	and	ah,HIGH OPCODE_MASK	;upper bits sometimes used for operands
	mov	dx,ax
	mov	bx,dx			;bx = opcode
	mov	al,cs:[mpOpAtr_UNDONE + bx]    ; al = #bytes of operands
	and	ax,OPA_CntMask		;Isolate attribute count
.errnz	OPA_CntMask AND 0FF00H		;must use ax in next line if not
	cmp	al,OPA_CntMask		;Test for cnt field in operand
	jne	SkipNotVar		;brif variable length operand count

	lodsw				;ax = # bytes of operands
	inc	ax			;round up to odd count as follows:
	and	al,-2			; {0,1,2,3,4,...} => {1,3,3,5,5,...}
SkipNotVar:
	add	si,ax			;si points to next opcode
	push	ss
	pop	ds			;ds -> DGROUP
	DJMP	jmp SHORT FindExit

J1_FindOpExec:
	DJMP	jmp SHORT FindOpExec

; Search for first opcode in text table which matches an opcode specified
; in the search table.

NotSkipOp:
	test	[flags],TFC_FindExec
	jne	J1_FindOpExec		;brif scanState == SS_EXECUTABLE

	test	[flags],TFC_FindNext
	je	TestOp			;brif entry point was TxtFindOp[DS]
	lodsw		 		;ax = current opcode from text table

	and	ah,HIGH OPCODE_MASK	;upper bits sometimes used for operands
	mov	dx,ax

	;Register conventions for TxtFind loop
	;ds:si	- pts to current opcode in txt table
	;ss:di	- pts to base of search opcode bit array
	;dx	- current opcode

TryNext:
	mov	bx,dx			;bx = opcode
	mov	al,cs:[mpOpAtr_UNDONE + bx]    ; al = #bytes of operands
	and	ax,OPA_CntMask		;Isolate attribute count
.errnz	OPA_CntMask AND 0FF00H		;must use ax in next line if not
	cmp	al,OPA_CntMask		;Test for cnt field in operand
	je	VarLenOpcode		;brif variable length operand count
VarLenRet:
	add	si,ax			;si points to next opcode
TestOp:
	lodsw		 		;ax=opcode to search for (from txt tbl)
	and	ah,HIGH OPCODE_MASK	;upper bits sometimes used for operands

; test to see if opcode bit is set in search table

	mov	dx,ax			;dx = current opcode
	mov	bx,ax
	shr	bx,1			;divide opcode by 8 to get byte index
	shr	bx,1			;into bit-packed array for this opcode
	shr	bx,1			;bx=byte in bit packed array for opcode
	and	al,7			;al = bit number in array byte
	mov	cl,al
	mov	al,1
	shl	al,cl			;al = bit mask for opcode in array byte
	test	ss:[di+bx],al		;test opcode bit for match
	jz	TryNext 		;brif no match, try next opcode

; We have found the opcode.  We reenter here from FindOpExec.
;    si - pts two bytes past found opcode.

FoundOpcode:
	dec	si
	dec	si			;si points to found opcode

	push	ss
	pop	ds			;restore ds -> DGROUP
assumes ds,DGROUP
					;es still points to CODE
	mov	di,[pOpcodeList]	;es:di = ptr to original list of opcodes
	inc	di
	inc	di			;advance past size of table to first op
	mov	cx,-1
	xchg	ax,dx			;ax = opcode found
	repne	scasw			;search for found opcode in table
	not	cx			;cx = index+1 into table where opcode
					;was found
	dec	cx			;cx = index
	DbAssertRel cx,be,[cOpcodes],CP,<Error In TxtFindOp: Incorrect opTable>
	mov	[txtFindIndex],cl	;return index for matched opcode in
					; global static variable txtFindIndex
	mov	dl,cl			;return txtFindIndex in dl
FindExit:
	xchg	ax,si			;return offset in ax
	DbSegMoveOn			;OK to move segs again
cEnd	;TxtFindOpcode

; We have a variable length opcode in SS_RUDE or SS_PARSE state.
; Pick up the byte count of the operands, and skip to the next opcode.
;
VarLenOpcode:
	lodsw				;ax = # bytes of operands
	inc	ax			;round up to odd count as follows:
	and	al,-2			; {0,1,2,3,4,...} => {1,3,3,5,5,...}
	jmp	SHORT VarLenRet


	;********************************************************
	;NOTE: DS register points to text table until end-of-loop
	;      ES points to CODE, where opcodes in execute state can be accessed
	;      SS still points to DGROUP, so local vars can be accessed
	;********************************************************
assumes	ds,NOTHING

; We are in SS_EXECUTE state.  Search the pcode for an opcode from
; the passed table.

FindOpExec:
	test	[flags],TFC_FindNext
	je	TestOpExec		;brif entry point was TxtFindOp[DS]
	lodsw		 		;ax = current opcode from text table
; map executor to opcode
	xchg	bx,ax			;bx = executor's adr
	mov	ax,es:[bx-2]		;ax = executor's opcode

	and	ah,HIGH OPCODE_MASK	;upper bits sometimes used for operands
	mov	dx,ax

	;Register conventions for FinOpExec loop
	;ds:si	- pts to current opcode in txt table
	;ss:di	- pts to base of search opcode bit array
	;es	- pts to CODE seg for execute state searches
	;dx	- current opcode

TryNextExec:
	mov	bx,dx			;bx = opcode
	mov	al,cs:[mpOpAtr_UNDONE + bx]    ; al = #bytes of operands
	and	ax,OPA_CntMask		;Isolate attribute count
.errnz	OPA_CntMask AND 0FF00H		;must use ax in next line if not
	cmp	al,OPA_CntMask		;Test for cnt field in operand
	je	VarLenOpcodeExec	;brif variable length operand count
VarLenExecRet:
	add	si,ax			;si points to next opcode
TestOpExec:
	lodsw		 		;ax=opcode to search for (from txt tbl)

; map executor to opcode
	mov	bx,ax			;bx = executor's adr
	mov	ax,es:[bx-2]		;ax = executor's opcode

	and	ah,HIGH OPCODE_MASK	;upper bits sometimes used for operands

; test to see if opcode bit is set in search table

	mov	dx,ax			;dx = current opcode
	mov	bx,ax
	shr	bx,1			;divide opcode by 8 to get byte index
	shr	bx,1			;into bit-packed array for this opcode
	shr	bx,1			;bx=byte in bit packed array for opcode
	and	al,7			;al = bit number in array byte
	mov	cl,al
	mov	al,1
	shl	al,cl			;al = bit mask for opcode in array byte
	test	ss:[di+bx],al		;test opcode bit for match
	jz	TryNextExec		;brif no match, try next opcode
	jmp	FoundOpcode

; We have a variable length opcode in SS_EXECUTE state.
; Pick up the byte count of the operands, and skip to the next opcode.
;
VarLenOpcodeExec:
	lodsw				;ax = # bytes of operands
	inc	ax			;round up to odd count as follows:
	and	al,-2			; {0,1,2,3,4,...} => {1,3,3,5,5,...}
	jmp	SHORT VarLenExecRet

assumes ds,DGROUP


;*************************************************************************
; TxtFindOpFar
; Purpose:
;	Same as TxtFindOp, only has a FAR interface
;
;*************************************************************************
cProc	TxtFindOpFar,<PUBLIC,FAR>
	parmW	otxStart
	parmW	pOpcodeList
cBegin
	cCall	TxtFindOp,<otxStart,pOpcodeList>
cEnd

;*************************************************************************
; TxtFindNextOpFar
; Purpose:
;	Same as TxtFindNextOp, only has a FAR interface
;
;*************************************************************************
cProc	TxtFindNextOpFar,<PUBLIC,FAR>
	parmW	otxStart
	parmW	pOpcodeList
cBegin
	cCall	TxtFindNextOp,<otxStart,pOpcodeList>
cEnd

;*************************************************************************
; TxtSkipOp
; Purpose:
;	Skip over 1 opcode.  Text table can be in any scan-state.
; Entry:
;	grs.fDirect, grs.oRsCur identify text table
;	ax = offset of opcode to be skipped
; Exit:
;	ax = offset beyond skipped opcode
;
;*************************************************************************
cProc	TxtSkipOp,<PUBLIC,NEAR>
cBegin
	push	ax
	PUSHI	ax,0
	cCall	TxtFindNextOp		;ax = result
cEnd

;*************************************************************************
; TxtSkipOpFar
; Purpose:
;	Same as TxtSkipOp, only has a FAR interface, and otx is in BX
;
;*************************************************************************
cProc	TxtSkipOpFar,<PUBLIC,FAR>
cBegin
	xchg	ax, bx			; ax = input value
	cCall	TxtSkipOp		;ax = result
cEnd

;*************************************************************************
; TxtChkValidOpsExec
; Purpose:
;	Verify that expected opcodes exist in an oTx range. The text
;	table is assumed to be in SS_EXECUTE state.  Ensures that only
;	white space and remarks come between a SELECT CASE and the
;	first CASE, END SELECT clause.
; Entry:
;	parm1: ushort oTxFirst - oTx of first opcode of interest.
;	parm2: ushort oTxLast  - oTx of last opcode of interest.
; Exit:
;	ax = oTx of first opcode not found in list, or oTxLast if all opcodes
;	     valid.
;	dx == 0 if all opcodes valid.
; Preserves:
;	si, di
;*************************************************************************
cProc	TxtChkValidOpsExec,<PUBLIC,FAR>,<si>	
parmW	oTxFirst
parmW	oTxLast
cBegin
	DbChk	Otx,otxFirst		
	DbChk	Otx,otxLast		
	mov	ax,oTxFirst		;start at first opcode of interest

TxtChkValidLoop:
	push	ax			;stack oTxCur for Second TxtFind call

	push	ax
	PUSHI	ax,<codeOFFSET tOpSelect> ;get valid op table addr
	call	TxtFindNextOpExec	;ax = oTx of next opcode in list
	pop	bx			;recover oTxCur

	cmp	dl,SEL_opValidMax	;is op within valid range?
	ja	TxtChkValidX		;ax = oTx of bad guy, psw.z clear

	xchg	ax,si			;remember found opcode

	push	bx			;oTxCur
	PUSHI	ax,0			;find next op (oTxCur already stacked)
	cCall	TxtFindNextOpExec	;ax = oTx of next opcode
	cmp	ax,si			;next opcode should have been in list
	jnz	TxtChkValidX		;brif not - offender in AX, psw.z clear

	cmp	ax,oTxLast		;more to search?
	jb	TxtChkValidLoop 	;brif so.

	cmp	ax,ax			;all hunky dory - set psw.z

TxtChkValidX:
	mov	dx,sp			
	jnz	@F			; brif not all opcodes valid

	sub	dx,dx			
@@:					
cEnd

;*************************************************************************
; LOCAL FindBol
; Purpose:
;	Search the current text table from its start for a certain
;	beginning of line opcode, given the following 2 stopping conditions:
;	1 - stop after cx beginning of lines have been seen,
;	2 - stop after going beyond a certain text offset
; NOTE: This routine is crucial for PageUp speed.  Thus some code has been
;	duplicated from TxtFindOpcode to help Page Up performance.
; Entry:
;	ax = txt offset to stop at
;	cx = line # to stop at
; Exit:
;	grs.fDirect is reset to FALSE
;	ax = text offset to beginning of line opcode
;	dx = text offset to beginning of line opcode for previous line
;	cx = initial cx - # lines skipped
;	[fLnNotIncl] = zero if given line was an INCLUDEd line
;
; For example, if pcode contained:
;	[0]opBol,[2]opStop,[4]opBol,[6]opStop,[8]opEndProg,[0A]opEot
; The following inputs would produce the following results:
;	  ax   cx  =>	ax   cx   dx
;	0000 FFFF  => 0004 FFFF 0000
;	0001 FFFF  => 0004 FFFF 0000
;	0002 FFFF  => 0004 FFFF 0000
;	0003 FFFF  => 0004 FFFF 0000
;	0004 FFFF  => 0008 FFFE 0004
;	0005 FFFF  => 0008 FFFE 0004
;	0006 FFFF  => 0008 FFFE 0004
;	0007 FFFF  => 0008 FFFE 0004
;	0008 FFFF  => 0008 FFFE 0004
;	FFFF 0000  => 0000 0000 0000
;	FFFF 0001  => 0004 0000 0000
;	FFFF 0002  => 0008 0000 0004
;	FFFF 0003  => 0008 0000 0004
;
;*************************************************************************

StartFromTop:
	xor	ax,ax			
	mov	[lnFindLast],ax		;set lnFindLast to line # zero

;We must search for the first Bol since the scanner can insert opNoList's
;before the first line.
	push	cx			;save skip line count
	push	ax			
	PUSHI	ax,<CODEOFFSET tOpBol>	;pass ptr to start-of-line table
	call	TxtFindOp		;ax = offset to 1st opBos/opBol/opEot
	pop	cx			;restore cx = skip line count
	mov 	dx,ax			;dx = offset of 0th line
	mov	[otxFindLast],ax	;otxFindLast = offset of 0th line
	jmp	SHORT StartFromLast

; We are in SS_EXECUTE state. Map executor address in ax to opcode.
FBLGotExec:
	xchg	ax,bx			;executor address is in bx
	mov	ax,es:[bx-2]		;get opcode
DJMP	jmp	short FBLGotExecRet

;This value of CLINES_CACHE means in the QBI screen editor, a time
;consuming OtxOfLn search is performed every few page-up keys
;when the user is scrolling up through his file.

CLINES_CACHE EQU 60
DbPub	FindBol
cProc	FindBol,<NEAR>,<si,di,es>
localW	cLnCache
localW	oTxBolPrev
cBegin
	DbChk	Otx,ax			
	SetfDirect dl,FALSE		;turn off direct mode
					;	 for TxtFindNextOp
	mov	[fLnNotIncl],NOT FALSE
	xchg	di,ax			;di = stopping text offset
	mov	ax,[otxFindLast]	;ax = oTxCur
	mov	dx,ax			;dx = oTxPrev

	cmp	di,ax
	jb	StartFromTop		;brif can't used cached value
	cmp	cx,[lnFindLast]
	jb	StartFromTop		;brif can't used cached value
	sub	cx,[lnFindLast]		;start search from last saved location
StartFromLast:
	inc	di			;Adjust otxMax since Lodsw in loop will
	inc	di			; advance 2 bytes past current opcode.
DJMP	jcxz	FindBolExit		;brif we've skipped enough lines (0)
	mov	[cLnCache],CLINES_CACHE+1 ;load count-down counter
	push	dx
	push	cx
	call	TxtSkipOp		;skip first opcode
	xchg	ax,si			;update oTxCur

	GetSegAddr CODE			;ax = current address of CODE seg
	push	ax			;save addr of CODE seg
	GetSegTxtCur			;[24]es = seg of curr txt table
	push	es			
	pop	ds			;ds = seg addr of current text table
assumes	ds,NOTHING
	pop	es			;es = seg addr of CODE seg

	pop	cx			
	pop	dx			

	;*******************************************************************
	; Register useage during loop
	;  DS - register points to text table until end-of-loop
	;  ES - points to CODE, where opcodes in execute state
	;	  can be accessed
	;  SS - still points to DGROUP, so local vars can be accessed
	;  DI - otxMax (stop search if beyond otxMax)
	;  SI - otxCur (current otx of opcode we are searching)
	;  DX - otxBolCur (otx of last BOL encountered)
	;  CX - cLnMax (maximum number of BOLs to visit)
	;  BX - scratch
	;  AX - scratch
	;*******************************************************************

FindBolLoop:
	dec	[cLnCache]
	je	UpdateCache		;brif time to save reference point

FindBolLoop1:
	mov	[oTxBolPrev],dx 	;remember Bol of previous line
FBLInnerLoop:

	lodsw				;ax = opcode/executor
	cmp	[txdCur.TXD_scanState],SS_EXECUTE ;is this table in execute?
DJMP	je	FBLGotExec		;brif so, map executor to opcode

FBLGotExecRet:
	and	ah,HIGH OPCODE_MASK	;upper bits sometimes used for operands
	mov	bx,ax			;bx = opcode
.errnz	opBol
.errnz	opBolSp-1
.errnz	opBolInclude-opBolSp-1
.errnz	opBolIncludeSp-opBolInclude-1
.errnz	opBolLab-opBolIncludeSp-1
.errnz	opBolLabSp-opBolLab-1
.errnz	opBos-opBolLabSp-1
.errnz	opBosSp-opBos-1
.errnz	opEot-opBosSp-1
.errnz	opEndProg-opEot-1

	cmp	ax,opEndProg		;is this opBol -> opEndProg?
	jbe	FBLGotHit		;brif so, we have an interesting opcode

FBLGotHitCont:
	mov	al,cs:[mpOpAtr_UNDONE + bx]    ; al = #bytes in opcode
	and	ax,OPA_CntMask		;Isolate attribute count
.errnz	OPA_CntMask AND 0FF00H
	cmp	al,OPA_CntMask		;is this a variable length opcode?
	je	FBLVarLenOpcode 	;brif so

FBLVarLenRet:
	add	si,ax			;si points to next opcode
	jmp	short FBLInnerLoop	;go grab next opcode

FBLLoopCont:
	loop	FindBolLoop		;brif haven't reached line count limit

;We ran out of lines before we passed oTxMax.  Return the otx of the
; last Bol encountered.
	xchg	ax,dx			;return oTxBolCur
	jmp	short FBLExit1

FindBolDone:
	xchg	ax,si			;return text offset of Bol in ax.
	dec	ax
	dec	ax			;back up to opcode
FBLExit1:
	mov	dx,[oTxBolPrev] 	;return BOL of previous line

FindBolExit:
	push	ss			
	pop	ds			;recover ds=DGROUP
;	DbChk	AtBosBol,ax		;sanity check on ret val
	DbChk	AtBosBol,dx		;sanity check on ret val
cEnd

;We have a variable length opcode. Grab and return count of bytes for opcode
;
FBLVarLenOpcode:
	lodsw				;ax = # bytes of operands
	inc	ax			;round up to odd count as follows:
	and	al,-2			; {0,1,2,3,4,...} => {1,3,3,5,5,...}
	jmp	short FBLVarLenRet


;This gets invoked every CLINES_CACHE times through FindBol's loop
;
UpdateCache:
	mov	[otxFindLast],dx	;save reference text offset
	mov	ax,CLINES_CACHE 	;reload count-down counter
	mov	[cLnCache],ax
	add	[lnFindLast],ax 	;update reference line number
	jmp	FindBolLoop1

;We have an interesting opcode.  It is either an opBos/Bol variant, or
; it is an opEot, opEndProg
FBLGotHit:
	cmp	ax,opBos		;is it an opBol variant?
	jb	FBLGotBol		;brif so
	cmp	ax,opEot		;is it an opBos variant?
	jb	FBLGotHitCont		;brif so, continue search

;We must have either an opEot, or and opEndProg.
;If we found an opEndProg, we're done.  If we found an opEot, it could only
;mean that we were called with an empty text table, and a search line > 0.
;In this case, we force the otxFound to 0.
;
	jne	FindBolDone		;it was an opEndProg
	SetStartOtx ax			;oTxBol = first line
	mov	dx,ax			;oTxBolPrev = first line
	jmp	SHORT FindBolExit

FBLGotBol:
	cmp	si,di			;are we beyond oTxMax?
	ja	FindBolDone		;brif so

	mov	dx,si			;remember this BOL
	dec	dx
	dec	dx			;back up to opBol opcode
;Advance to next opcode
	mov	al,cs:[mpOpAtr_UNDONE + bx]    ; al = #bytes in opcode
	and	ax,OPA_CntMask		;Isolate attribute count
.errnz	OPA_CntMask AND 0FF00H

	add	si,ax			;si points to next opcode
	cmp	[fViewInclude],FALSE
FBLLoopCont_NE:
	jne	FBLLoopCont		;brif user wants to see INCLUDEd text

	mov	[fLnNotIncl],NOT FALSE	;assume line was not included
	cmp	bx,opBolInclude
	jb	FBLLoopCont		;brif line not included
	cmp	bx,opBolIncludeSp
	ja	FBLLoopCont_NE		;brif line not included
	mov	[fLnNotIncl],FALSE	;Line was included
	jmp	FindBolLoop1		;dont count this line if it was included

assumes ds,DGROUP

;*************************************************************************
; TxtChkCache
; Purpose:
;	This function is called whenever:
;	 - Text is editted in the text table (TxtChange, TxtDelete)
;	It resets Watch and History, as well as calling TxtFlushCache if
;	  the editted pcode was prior to the previous cached value.
; Entry:
;	si - oTx of lowest text offset affected by pcode movement
;
;*************************************************************************
;*************************************************************************
; TxtMoved
; Purpose:
;	This function is called whenever:
;	 - Text is moved in the text table (TxtChange, TxtDelete, SsScan,
;	   SsDeScan)
;	It resets Watch and History, as well as calling TxtFlushCache.
;
;*************************************************************************
;*************************************************************************
; TxtFlushCache
; Purpose:
;	This function is called whenever:
;	 - The current text table changes (TxtActivate, TxtCurInit)
;	It discards any static variables used to speed up searching
;	of text tables.
;
;*************************************************************************
PUBLIC	TxtChkCache
TxtChkCache PROC NEAR
	cmp	si,[otxFindLast]	;is affected pcode after cached otx?
	jb	TxtMoved	 	;brif not
					;else txtmgr's cached otx still valid
	call	UiFlushCache		;tell User Interface to flush its cache
	jmp	SHORT TxtMovedNoFlush 	;brif so, cached otx still valid
TxtChkCache ENDP

PUBLIC	TxtMoved
TxtMoved PROC NEAR
	call	TxtFlushCache		;flush cached values
TxtMovedNoFlush:
	test	[mrsCur.MRS_flags2],FM2_NoPcode OR FM2_Include
	jne	NoPcode			;brif editing INCLUDE or DOCUMENT
	cmp	[grs.GRS_fDirect],FALSE
	jne	NoPcode			;brif editing direct mode stmt
	call	HistReset
	call	WatchMoved
NoPcode:
	ret
TxtMoved ENDP

PUBLIC	TxtFlushCache
TxtFlushCache PROC NEAR
	mov	ax,UNDEFINED
	mov	[lnFindLast],ax
	mov	[otxFindLast],ax
	call	UiFlushCache		;tell User Interface to flush its buffer
	ret
TxtFlushCache ENDP

;*************************************************************************
; LnOfOtx
;
; Purpose:
;	Given an  offset within the current text table, return
;	the physical line number of the line containing the text.
;	Used for error reporting.
;
; Entry:
;	parm1: ushort otx
;
; Exit:
;	grs.fDirect is reset to FALSE
;	AX = 0 relative line number for line containing the offset
;	[fLnNotIncl] = zero if given line was an INCLUDEd line
;
; For example, if pcode contained:
;	[0]opBol,[2]opStop,[4]opBol,[6]opStop,[8]opEndProg,[0A]opEot
; The following inputs would produce the following results:
;	0000 => 0000
;	0001 => 0000
;	0002 => 0000
;	0003 => 0000
;	0004 => 0001
;	0005 => 0001
;	0006 => 0001
;	0007 => 0001
;	0008 => fatal error (only checked in non-release versions)
;
;*************************************************************************
cProc	LnOfOtx,<PUBLIC,FAR>
	parmW	otx
cBegin	LnOfOtx
	DbChk	TxdCur			;perform sanity check on txdCur
	mov	cx,0FFFFH		;don't stop on line count
	mov	ax,[otx]		;stop when we've reached this otx
	call	FindBol 		;search current text table
	mov	ax,0FFFFH
	sub	ax,cx			;ax = line number (0..n)
cEnd	LnOfOtx

;*************************************************************************
; OtxOfLn
;
; Purpose:
;	Given a  physical line	offset (0..n) within the current text
;	table, return  the byte offset into the text table for
;	its opBol opcode.
;
; Entry:
;	parm1: ushort lnSearch
;
; Exit:
;	grs.fDirect is reset to FALSE
;	AX = byte offset into text table
;	[fLnNotIncl] = zero if given line was an INCLUDEd line
;
; For example, if pcode contained:
;	[0]opBol,[2]opStop,[4]opBol,[6]opStop,[8]opEndProg,[0A]opEot
; The following inputs would produce the following results:
;	0000  => 0000
;	0001  => 0004
;	0002  => 0008
;	0003  => fatal error (only checked in non-release versions)
;
;*************************************************************************
cProc	OtxOfLn,<PUBLIC,FAR>
	parmW	ln
cBegin
	DbChk	TxdCur			;perform sanity check on txdCur
	mov	cx,[ln]			;cx = # lines to skip
	mov	ax,[txdCur.TXD_bdlText_cbLogical] ;ax = offset to end-of-text
	call	FindBol 		;ax = offset to bol
cEnd

;*************************************************************************
; OtxBolOfOtx
;
; Purpose:
;	Given an  offset within the current text table, return
;	the text offset to the start of line.
; Entry:
;	parm1: ushort otx
; Exit:
;	grs.fDirect is reset to FALSE
;	AX = byte offset into text table for line's opBol
;	[fLnNotIncl] = zero if given line was an INCLUDEd line
;
; For example, if pcode contained:
;	[0]opBol,[2]opStop,[4]opBol,[6]opStop,[8]opEndProg,[0A]opEot
; The following inputs would produce the following results:
;	0000 => 0000
;	0001 => 0000
;	0002 => 0000
;	0003 => 0000
;	0004 => 0004
;	0005 => 0004
;	0006 => 0004
;	0007 => 0004
;	0008 => fatal error (only checked in non-release versions)
;
;*************************************************************************
cProc	OtxBolOfOtx,<PUBLIC,FAR>
	parmW	otx
cBegin	OtxBolOfOtx
	DbChk	TxdCur			;perform sanity check on txdCur
	mov	cx,0FFFFH		;don't stop on line count
	mov	ax,[otx]		;stop when we've reached this otx
	call	FindBol
	xchg	ax,dx			;ax = offset to start of line
cEnd	OtxBolOfOtx

;*************************************************************************
; OtxBosOfOtx
;
; Purpose:
;	Given an  offset within the current text table, return
;	the text offset to the start of statement.
; Entry:
;	parm1: ushort otx
; Exit:
;	grs.fDirect is reset to FALSE
;	AX = byte offset into text table for stmt's opBol/opBos
;
; For example, if pcode contained:
;	[0]opBol,[2]opStop,[4]opBol,[6]opStop,[8]opEndProg,[0A]opEot
; The following inputs would produce the following results:
;	0000 => 0000
;	0001 => 0000
;	0002 => 0000
;	0003 => 0000
;	0004 => 0004
;	0005 => 0004
;	0006 => 0004
;	0007 => 0004
;	0008 => fatal error (only checked in non-release versions)
;
;*************************************************************************
cProc	OtxBosOfOtx,<PUBLIC,FAR>
	parmW	otx
cBegin	OtxBosOfOtx
	DbChk	TxdCur			;perform sanity check on txdCur
	push	[otx]
	call	OtxBolOfOtx		;ax = offset to opBol for this line
BosLoop:
	push	ax			;save potential return value
	push	ax			;pass current text offset
	PUSHI	ax,<CODEOFFSET tOpBos>	;pass ptr to start-of-line table
	call	TxtFindNextOp		;ax = offset to next opBos/opEot
	pop	dx			;dx = potential return value
	cmp	[otx],ax
	ja	BosLoop 		;brif bos is before otx of interest
	xchg	ax,dx			;ax = return value
cEnd	OtxBosOfOtx

;*************************************************************************
; OtxBosNext
; Purpose:
;	Given a text offset into current text table or Direct Mode buffer,
;	find the text offset for the next beginning of statement/line.
;	This is used by UI debugger to find statement boundaries to
;	highlight for trace.
; Entry:
;	parm1 = text offset of interest
;	grs.fDirect, oMrsCur, oPrsCur identify the text table being
;	  searched.  txdCur describes its text table.
; Exit:
;	ax = text offset to next opBol/opBos/opEot opcode
;
;*************************************************************************
cProc	OtxBosNext,<PUBLIC,FAR>		
	parmW	otx			
cBegin	OtxBosNext			
	DbChk	Otx,otx			
	push	[otx]			;pass offset of interest to TxtFind...
	PUSHI	ax,<CODEOFFSET tOpBosBol> ;pass ptr to start-of-line table
	call	TxtFindNextOp		;ax = offset to next opBos/opBol/opEot
cEnd	OtxBosNext			

;*************************************************************************
; OtxBolNext0
; Purpose:
;	Given a text offset into current text table or Direct Mode buffer,
;	find the text offset for the next beginning of line.
;	This is used by CALLS menu and PreScanAsChg().
; Entry:
;	parm1 = text offset of interest
;	grs.fDirect, oMrsCur, oPrsCur identify the text table being
;	  searched.  txdCur describes its text table.
; Exit:
;	ax = text offset to next opBol/opEot opcode
;
;*************************************************************************
cProc	OtxBolNext0,<FAR,PUBLIC>	
	parmW	otx			
cBegin	OtxBolNext0			
	DbChk	Otx,otx			
	push	[otx]			;pass offset of interest
	PUSHI	ax,<CODEOFFSET tOpBol>	;pass ptr to start-of-line table
	call	TxtFindOp		;ax = offset to next opBos/opBol/opEot
cEnd	OtxBolNext0			


;*************************************************************************
; OtxResume
;
; Purpose:
;	Same as OtxBosOfOtx, except that it will also stop at opStIf
;	and opStElse opcodes.  This is needed for RESUME compatability
;	in single line IF statements.
; Entry:
;	parm1: ushort otx
; Exit:
;	grs.fDirect is reset to FALSE
;	AX = byte offset into text table for stmt's opBol/opBos, or
;	     past opStElse, opStIf operand.
;
;*************************************************************************
cProc	OtxResume,<PUBLIC,NEAR>
	parmW	otx
cBegin	OtxResume
	DbChk	TxdCur			;perform sanity check on txdCur
	push	[otx]
	call	OtxBolOfOtx		;ax = offset to opBol for this line
ResLoop:
	push	ax			;save potential return value
	push	ax			;pass current text offset
	PUSHI	ax,<CODEOFFSET tOpResume> ;pass ptr to start-of-line table
	call	TxtFindNextOp		;ax = offset to next opBos/opEot

; We are sensitive to opStIf and opStElse here so that an error in a
; then or else clause will cause RESUME to restart execution in the
; appropriate clause instead of at the beginning of the IF expression.
; This is necessary for compiler compatability. This only applies to
; Single line IF/THEN/ELSE, as the multi line versions are line oriented.

	cmp	dl,RESUME_opIfElseMax	;did we stop at a single IF/ELSE opcode?
	ja	ResNotIfElse		;brif not
	call	TxtSkipOp		;ax = opcode beyond opStIf/opStElse
ResNotIfElse:
	pop	dx			;dx = potential return value
	cmp	[otx],ax
	ja	ResLoop 		;brif bos is before otx of interest
	xchg	ax,dx			;ax = return value
cEnd	OtxResume

;*************************************************************************
; OtxResumeNext
; Purpose:
;	Given a text offset into current text table or Direct Mode buffer,
;	find the text offset for the next beginning of statement/line.
;	This is used by RESUME NEXT statement.
; Entry:
;	parm1 = text offset of interest
;	grs.fDirect, oMrsCur, oPrsCur identify the text table being
;	  searched.  txdCur describes its text table.
; Exit:
;	ax = text offset to next opBol/opBos/opEot opcode, or opcode after
;	  opStIf.
;
;*************************************************************************
cProc	OtxResumeNext,<PUBLIC,FAR>	
	parmW	otx			
cBegin	OtxResumeNext			
	;Preserve and clear grs.fDirect in case RESUME NEXT is being entered
	;from direct mode.
	
	push	WORD PTR ([grs.GRS_fDirect])
	SetfDirect al,FALSE		;switch off direct mode

	push	[otx]			;pass offset of interest to TxtFind...
	PUSHI	ax,<CODEOFFSET tOpResumeNext> ;pass ptr to start-of-line table
	call	TxtFindNextOp		;ax = offset to next opBos/opBol/opEot

;  We need to be sensitive to opStIf in case the error occurred in the IF
;  expression.	An error in the If expresion, and RESUME next in the compiler
;  will cause execution to restart in the THEN clause.	Wierd, but we must
;  do this to be compatable.  An error in the then clause however, will
;  not cause execution to RESUME NEXT into the else clause, execution starts
;  after the else clause, thus we don't need to be sensitive to opStElse here.
;  This is only a concern for Single line IF/THEN/ELSE, as the multi-line
;  variants are line oriented by nature.
;
	cmp	dl,RESUMENEXT_opIfMax	;did we stop at a single IF/ELSE opcode?
	ja	ResNxtNotIf		;brif not
	call	TxtSkipOp		;ax = opcode beyond opStIf/opStElse
ResNxtNotIf:
	PopfDirect dx			
cEnd	OtxResumeNext			

;*************************************************************************
; OtxLabOfOtx
;
; Purpose:
;	Given an  offset within the current text table, return
;	the text offset for the highest opLab/opBolLab/opLabSp/opBolLabSp
;	opcode which precedes this text offset.
; Entry:
;	ax = otx of interest
; Exit:
;	ax = UNDEFINED if no label precedes this text offset
;	     else, offset into text table to link field of label opcode
;
;*************************************************************************
PUBLIC	OtxLabOfOtx
OtxLabOfOtx PROC NEAR
	DbChk	Otx,ax			
	DbChk	TxdCur			;perform sanity check on txdCur
	xchg	dx,ax			;dx = text offset of interest
	GetSegTxtTblCur	es		;[24]es = seg adr of current text tbl
	mov	ax,UNDEFINED		;ax = default return value (not found)
	mov	bx,[txdCur.TXD_otxLabLink] ;bx = offset to 1st link in list
LinkLoop:
	cmp	bx,dx
	jae	LinkExit		;brif beyond otx of interest
	mov	ax,bx			;ax = previous link
	mov	bx,es:[bx]		;bx points to next in linked list
	jmp	SHORT LinkLoop

LinkExit:
	ret
OtxLabOfOtx ENDP

;**************************************************************
; OtxNoInclude,OtxNoInclPrev
; Purpose:
;	Return the text offset to the next/previous line which did not
;	come from an INCLUDE file.
; Entry:
;	parm ushort otx = text offset where search is to start
; Exit:
;	grs.fDirect is reset to FALSE
;	ax = otx to opBol/opEot for next line which was not INCLUDed
;
;**************************************************************
PUBLIC	OtxNoInclude
OtxNoInclude PROC FAR
	mov	ax,1
	SKIP2_PSW			;skip following sub ax,ax
OtxNoInclude ENDP
PUBLIC	OtxNoInclPrev
OtxNoInclPrev PROC FAR
	sub	ax,ax
OtxNoInclPrev ENDP
;Common entry for OtxNoInclude, OtxNoInclPrev
?DFP = DFP_NONE 			; don't generate non-Release
					;   code to smash regs
cProc	NoIncl,<FAR>			
	ParmW	otx			
cBegin	NoIncl				
?DFP = DFP_CP				; restore switch value
	mov	bx,[otx]		;bx = otx of where to start search
	push	WORD PTR [fViewInclude]
	mov	[fViewInclude],FALSE

	push	ax			;save ln increment
	push	bx			;pass otxStart
	call	LnOfOtx			;ax = line (0..n)
	pop	bx			;bx = ln increment
	add	ax,bx			;bump line# by 1 if OtxNoInclude
	push	ax
	call	OtxOfLn			;ax = text offset to ln

	pop	dx
	mov	[fViewInclude],dl	;restore previous value
cEnd	NoIncl				

;*************************************************************************
; OPrsOfOtx
;
; Purpose:
;	Given an  offset within the current text table, return
;	the oPrs for the DEF-FN which it falls within.
;	Text table is assumed to not be in SS_EXECUTE state.
; Entry:
;	parm1: ushort otx
; Exit:
;	if text table's scan state = SS_RUDE or if otx does not fall
;	   within a DEF FN, this function returns AX = UNDEFINED
;	else AX = byte offset into procedure table for DEF FN
;
;*************************************************************************
cProc	OPrsOfOtx,<PUBLIC,FAR>
	parmW	otx
cBegin
	DbChk	Otx,otx			
 DbAssertRelB [txdCur.TXD_scanState],ne,SS_EXECUTE,CP,<OPrsOfOtx: bad scanstate>
	DbChk	TxdCur			;perform sanity check on txdCur
	GetSegTxtTblCur	 es		;es = seg adr of cur txt tbl
	mov	ax,UNDEFINED		;default return value = UNDEFINED
	cmp	[grs.GRS_oPrsCur],ax
	je	NotInProc		;branch if not in SUB/FUNCTION
	cmp	[prsCur.PRS_procType],PT_DEFFN
	jne	DefFnExit		;no DEF FNs within SUB/FUNCTION
NotInProc:
	cmp	[txdCur.TXD_scanState],SS_RUDE
	jae	DefFnExit		; return UNDEFINED if SS_RUDE
	mov	bx,[mrsCur.MRS_otxDefFnLink]
DefFnLoop:
	cmp	bx,[otx]
	ja	DefFnExit		;brif beyond otx of interest
	mov	ax,UNDEFINED		;default return value = UNDEFINED
	cmp	WORD PTR es:-4[bx],opStDefFn
	jne	NotInDef		;brif its an END DEF
	mov	ax,es:2[bx]		;ax = oPrs of this DEF FN
NotInDef:
	mov	bx,es:[bx]		;bx points to next in linked list
	jmp	SHORT DefFnLoop

DefFnExit:
cEnd

;*************************************************************************
; OtxTypDefined
;
; Purpose:
;	Given an oNam, return an offset within the current text table,
;	to the TYPE statement that defines type oNam.
;	Text table is assumed to not be in SS_EXECUTE state.
; Entry:
;	parm1: ushort oNam
; Exit:
;	AX = UNDEFINED if type is not defined
;	     else, offset into text table to opStType's link field
;
;*************************************************************************
cProc	OtxTypDefined,<PUBLIC,NEAR>,<si>
	parmW	oNam
cBegin
 DbAssertRelB [txdCur.TXD_scanState],ne,SS_EXECUTE,CP,<OtxTypDefined: bad state>
	DbChk	TxdCur			;perform sanity check on txdCur
	push	[grs.GRS_oPrsCur]	;save caller's prs
	call	PrsDeactivate		;make module's text table active
	GetSegTxtTblCur	es		;es = seg adr of cur txt tbl
	mov	si,[txdCur.TXD_otxTypeLink]
	mov	ax,[oNam]
	DbChk	ONam,ax
OtxTypeLoop:
	cmp	si,UNDEFINED
	je	OtxTypeDone		;brif not found
	cmp	WORD PTR es:2[si],ax
	je	OtxTypeDone		;brif found
	mov	si,es:[si]		;si points to next in linked list
	jmp	SHORT OtxTypeLoop

OtxTypeDone:
	call	PrsActivateCP		;re-activate caller's prs.  Parm pushed
					; at entry to function, may be UNDEFINED
	mov	ax,si			;return result in ax
	inc	si
	je	OtxTypeExit		;brif type is UNDEFINED
	test	[txdCur.TXD_flags],FTX_mrs
	jne	OtxTypeExit		;brif ref is in module text table
	SetStartOtx ax			;for procedure references, forward
					; referncing is not a problem,
					; pretend definition is at start of text
OtxTypeExit:
cEnd




sEnd	CP


end
