
;=============================================================================
; prsnt1.asm - Parser's Non-Terminal Functions (continued)
;
; Copyright <C> 1985, Microsoft Corporation
;
; Purpose: 
;	In an ideal world, these functions would reside in prsnt.asm.
;	They live here to reduce the number of include files needed
;	by prsnt.asm, so MASM won't run out of memory when assembling it.
;	See general comments at top of prsnt.asm for non-terminal function
;	conventions.
;
;
;=============================================================================

	include	version.inc
	PRSNT1_ASM = ON
	includeOnce	architec	
	includeOnce	names
	includeOnce	opstmt
	includeOnce	opid 
	includeOnce	parser
	includeOnce	prstab
	includeOnce	psint
	includeOnce	qbimsgs
	includeOnce	types
	includeOnce	util

	assumes	CS,CP
	assumes	DS,DGROUP
	assumes	SS,DGROUP
	assumes	ES,DGROUP


sBegin	DATA
szB		DB "B",0
szBF		DB "BF",0
szF		DB "F",0
sEnd	DATA

sBegin	CP

subttl	misc parser nonterminal recognizers

;**********************************************************************
; PARSE_RESULT NEAR NtRwB()
;
; Purpose:
;	Parse the identifier B
; Exit:
;	Returns PR_GoodSyntax (and consumes token) or PR_NotFound
;
;******************************************************************
PUBLIC	NtRwB
NtRwB	PROC NEAR
	mov	ax,dataOFFSET szB
	jmp	SHORT CheckRw1
NtRwB	ENDP

;**********************************************************************
; PARSE_RESULT NEAR NtRwBF()
;
; Purpose:
;	Parse the identifier BF
; Exit:
;	Returns PR_GoodSyntax (and consumes token) or PR_NotFound
;
;******************************************************************
PUBLIC	NtRwBF
NtRwBF	PROC NEAR
	mov	ax,dataOFFSET szBF
	mov	cx,8002h		;highbit says don't change case
					; of existing namtbl id, incase
					; user has variable 'bF'
	jmp	SHORT CheckRw
NtRwBF	ENDP

;**********************************************************************
; PARSE_RESULT NEAR NtRwF()
;
; Purpose:
;	Parse the identifier F (as found in the statement LINE ...,B F
; Exit:
;	Returns PR_GoodSyntax (and consumes token) or PR_NotFound
;
;******************************************************************

PUBLIC	NtRwF
NtRwF	PROC NEAR
	mov	ax,dataOFFSET szF
CheckRw1:
	mov	cx,8001h		;highbit says don't change case
					; of existing namtbl id, incase
					; user has variable 'f'
NtRwF	ENDP
; Fall into CheckRw

;*******************************************************************************
; PARSE_RESULT NEAR CheckRw(ax=pbRw,cx=cbRw)
;
; Purpose:
;	Checks for the reserved word and eats it if found.
;
; Entry:
;	ax=pbRw	- Pointer to string containing the reserved word to check for.
;	cx=cbRw	- Length of the reserved word string.
;	
; Exit:
;	PR_GoodSyntax or PR_NotFound based on finding the pseudo reserved word.
;
;***************************************************************************
CheckRw PROC NEAR
	mov	bx,[pTokScan]		;bx points to current token
	cmp	[bx.TOK_class],CL_id
	jne	CheckNotFound		;branch if token isn't an identifier
	push	bx			;save pointer to token
	;ax = pbRw, cx = cbRw
	call	ONamOfPbCb		;ax = oNam (or 0 if out-of-memory)
					; no-need to check for out-of-memory
					; since comparison will fail, and we'll
					; get another out-of-memory when we
					; try to insert the text in a txttbl.
	pop	bx			;restore pointer to token
	cmp	[bx.TOK_id_oNam],ax
	jne	CheckNotFound
	call	ScanTok			;skip identifier 
	mov	al,PR_GoodSyntax
	ret
CheckNotFound:
	sub	al,al			;return(PR_NotFound)
	ret
CheckRw	ENDP

sEnd	CP

sBegin	DATA

str255Include	DB 7,'INCLUDE'
str255Static	DB 6,'STATIC'
str255Dynamic	DB 7,'DYNAMIC'

IMETA_INCLUDE	EQU 0
IMETA_STATIC	EQU 1
IMETA_DYNAMIC	EQU 2
NMETA EQU 3

	EVEN
tstr255Meta DW	OFFSET DGROUP:str255Include
	DW	OFFSET DGROUP:str255Static
	DW	OFFSET DGROUP:str255Dynamic
	EVEN

topMeta	DW	op_Include
	DW	op_Static
	DW	op_Dynamic

sEnd	DATA

sBegin	CP

;**********************************************************************
; STATICF(PARSE_RESULT) NtEmitRem(opcode)
;
; Purpose:
;	Emit opcode which encapsulates text argument for REM, DATA, _, or '.
;	Meta command syntax is:
;	  $DYNAMIC
;	  $STATIC
;	  $INCLUDE <spaces>:<spaces>'<filename>'
;
; Entry:
;	opcode is: opStRem for REM, opQuoteRem for ', opStData for DATA
;
; Exit:
;	opcode = opcode to be generated
;	An end-of-line terminates all of the above.
;	Sequential runs of a single char > 6 will be specified by opRemRepeat.
;	An additional terminator for a DATA statement is : outside of quotes.
;	Condition codes set based on value in al
;
;******************************************************************
cProc	NtEmitRem <PUBLIC,NODATA,NEAR>,<si,di>
	parmW	opcode
	localW	oSrcRemArg
	localW	fCompress
	localW	oDstCbRem
	localW	oDstTerm
	localW	cbMeta
	localW	iMeta
	localB	fMetaOk
	;fMetaOk is intended to allow meta commands only if the 1st
	; non-white-space char on the line is '$'
cBegin	NtEmitRem
	mov	[fCompress],sp		;we start by assuming that we want
					; to compress text
	mov	[fMetaOk],1		;$meta commands are valid
	mov	bx,[pTokScan]		;bx points to current token
	mov	si,[bx.TOK_oSrc]	;si = offset in source line to token
	mov	ax,[opcode]		;ax = opcode to be emitted

	;skip past reserved word in source buffer based on opcode 
	inc	si			;assume its a single-quote rem
	cmp	ax,opQuoteRem
	je	KeywordSkipped		;branch if we're doing '
	inc	si			;'REM' is 3 bytes long
	inc	si
	cmp	ax,opStRem
	je	KeywordSkipped		;branch if we're doing REM
	inc	si			;else it must be 'DATA' (4 bytes long)
	mov	[fCompress],0		;don't compress DATA text
KeywordSkipped:

;si = offset in current source line to 1st byte of remark's data
;     i.e. to 1st byte beyond "REM", "DATA", "'", "$STATIC" etc.
;
RemLoop:
	mov	[oSrcRemArg],si		;save offset to start of REM/DATA text
	add	si,[ps.PS_bdpSrc.BDP_pb] ;convert offset to ptr
					; si points to start of source buffer
	mov	[iMeta],UNDEFINED	;assume we'll see no $<meta>
	sub	dl,dl			;remember we're not inside a quote

;search for end of remark/DATA 
; si points to next byte of remark's data
; dl = non-zero if we're in a DATA statement and within double quotes
;      (so we know to not treat : as end-of-stmt)
;
LoopTillEos:
	lodsb				;al = next byte of source
	or	al,al
	je	J1_EmitRem		;branch if end-of-line
	cmp	[opcode],opStData
	je	GotData			;branch if we're scanning a DATA stmt
	cmp	[fMetaOk],0
	je	LoopTillEos		;brif $meta commands not legal
	cmp	al,"$"			;else we're in some form of REM stmt
	je	CheckMeta		;branch if '$' inside a REM
	cmp	al," "
	je	LoopTillEos		;brif haven't seen anything but space
	and	[fMetaOk],NOT 1		;$meta commands are no longer valid
					; unless we've already parsed one
	jmp	SHORT LoopTillEos	;test for end-of-line (end-of-REM)

GotData:
	cmp	al,34			;compare with '"' (double quote)
	jne	NotDoubleQuote
	not	dl			;toggle in-quote state 
	jmp	SHORT LoopTillEos

NotDoubleQuote:
	cmp	al,":"
	jne	LoopTillEos		;branch if not ':'
	or	dl,dl
	jne	LoopTillEos		;branch if we're inside a "xxx"
J1_EmitRem:
	jmp	SHORT EmitRem		;else, the DATA stmt is terminated

BadMeta:
	mov	ax,ER_ADF		; Map all $INCLUDE errors to AFE
	call	PErrMsg_AX		;al = PR_BadSyntax
	jmp	NtRemExit

;At this point, we've seen a dollar sign in a REMARK which could
;be the start of a $META directive.
;
CheckMeta:
	or	[fMetaOk],2		;$meta commands are now valid until
					; the end of this remark
	mov	di,NMETA		;number of meta commands to search for
MetaLoop:
	dec	di
	js	LoopTillEos		;branch if didn't find a $<meta>
	mov	bx,di			;bx = iMeta
	shl	bx,1			;bx = 2*iMeta
	mov	bx,WORD PTR tstr255Meta[bx]
					;bx points to str255 struct for string
	mov	al,[bx]			;al = length of meta command
	inc	bx			;bx points to 1st letter of meta
	cbw				;ax = cbMeta
	mov	[cbMeta],ax		;save it for later
	xchg 	cx,ax			;pass byte count in cx
	call	CmpStrIns		;do a case insensitive string compare 
					; of strings *bx and *si
	jne	MetaLoop		;branch if they don't match

	;We found a $META command in the REM
	;si points past '$' byte of $META directive
	mov	[iMeta],di		;save meta command's id
	or	di,di			;test for IMETA_INCLUDE
	jne	EmitRem			;branch if not $INCLUDE

	; got $INCLUDE <spaces>:<spaces>'<filename>'
	mov	di,si			;di points beyond "$"
	add	di,[cbMeta]		;di points beyond "$<meta>" 
	push	ds
	pop	es			;es = DGROUP for repne scasb

	jmp	short BadMeta		; Always give AFE error

;Copy bytes from source buffer ps.bdpSrc to pcode buffer ps.bdpDst 
; oSrcRemArg = offset to first byte to emit
; si points beyond text's terminator
;
EmitRem:
	mov	di,[opcode]		;di = REM/DATA/<meta> opcode
	cmp	di,opStData
	je	ItsData			;brif DATA (leave room for 0-terminator)
	dec	si			;si points to text's terminator
ItsData:
	sub	si,[ps.PS_bdpSrc.BDP_pb] ;si = offset beyond last byte to emit
	push	di			;pass opcode
	call	Emit16			;emit opcode
	mov	cx,[oSrcRemArg]		;cx = offset to first byte to emit
	mov	ax,si			;ax = offset beyond last byte to emit
	sub	ax,cx			;ax = size of opcode's argument
	push	cx			;pass offset to EmitSrc (below)
	push	ax			;pass cb to EmitSrc (below)
	mov	cx,[ps.PS_bdpDst.BDP_cbLogical] ;get offset to where cb will be
	mov	[oDstCbRem],cx		;emitted
	cmp	di,opStData
	je	LinkedOpcode		;branch if this opcode has link field
	cmp	di,opQuoteRem
	jne	UnlinkedOpcode		;branch if opcode has no column field
LinkedOpcode:
	inc	ax			;add size of link field to size
	inc	ax
	mov	dx,[ps.PS_bdpDst.BDP_pbCur]
	sub	dx,[ps.PS_bdpDst.BDP_pb] ;dx = #bytes in output buffer
	inc	dx			;add 2 bytes for cb field
					;subtract 1 for 0-terminator
	add	dx,ax			;dx = offset to 0-terminate DATA stmt
	mov	[oDstTerm],dx		;save offset to 0-terminate DATA stmt
	call	Emit16_AX		;emit the size field
	mov	bx,[pTokScan]		;bx points to current token
	mov	ax,[bx.TOK_oSrc]	;ax = source column token started in
					;for opSQuote, we are emitting the
					; column so we can accurately list it.
					; For all other opcodes, we are making
					; room for a link field 
UnLinkedOpcode:
	call	Emit16_AX		;emit the size field/link field
	cmp	[fCompress],0		;should we compress chars in string?
	jne	CompressText		;brif so
	call	EmitSrc			;parms were pushed (above)
	jmp	short ChkOpData
CompressText:
	push	[oDstCbRem]		;pass offset of cbRem in case
					; we compress the text
	call	EmitSrcCompress 	;other parms were pushed (above)
ChkOpData:
	cmp	di,opStData
	jne	NotData
	mov	bx,[ps.PS_bdpDst.BDP_pb]
	add	bx,[oDstTerm]		;bx points 1 byte beyond dst DATA
	mov	BYTE PTR [bx],0		;0-terminate DATA stmt
	dec	si			;rescan terminator
NotData:
	mov	bx,[iMeta]
	shl	bx,1			;bx = 2 * iMeta
	jc	GotLastRem		;branch if no more $<meta> commands on
					; this line
	;generate another opcode for the $META directive 
	mov	ax,topMeta[bx]		;ax = meta directive's opcode
	mov	[opcode],ax
	add	si,[cbMeta]
	inc	si
	mov	[fCompress],0		;don't compress $<meta> command text
	jmp	RemLoop

;si points beyond REM/DATA stmt
GotLastRem:
	add	si,[ps.PS_bdpSrc.BDP_pb]
	mov	[ps.PS_bdpSrc.BDP_pbCur],si
	call	ScanTok			;pTokScan points to end-of-stmt or
					; end-of-line token 
	mov	al,PR_GoodSyntax
NtRemExit:
	or	al,al			;set condition codes for caller
cEnd	NtEmitRem



sEnd	CP

end
