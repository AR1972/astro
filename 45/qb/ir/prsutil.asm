;=============================================================================
; prsutil.asm - Parser's ASM Utility Functions
;
; Copyright <C> 1985, Microsoft Corporation
;
; Purpose:
;	Contains Parser Utility Functions
;
;
;=============================================================================

	include	version.inc
	PRSUTIL_ASM = ON
	includeOnce	architec
	includeOnce	context
	includeOnce	heap
	includeOnce	lister
	includeOnce	parser
	includeOnce	prstab
	includeOnce	prsirw
	includeOnce	psint
	includeOnce	qbimsgs
	includeOnce	scanner
	includeOnce	txtmgr
	includeOnce	variable
	includeOnce	ui
	includeOnce	util

	assumes	DS,DGROUP
	assumes	SS,DGROUP
	assumes	ES,NOTHING

sBegin	CP
assumes	CS,CP

subttl	pcode emitting functions

;=====================================================================
;    B d   S T A T I C   B U F F E R   R O U T I N E S
;=====================================================================



;=====================================================================
;    P C O D E    G E N E R A T I O N    F U N C T I O N S
;=====================================================================

;*********************************************************************
; BOOL NEAR CheckFreeDstBuf(cbExtra)
; Purpose:
;	Checks to see if free space exists for the parser's
;	pcode buffer ps.bdpDst. If the extra space couldn't be
;	obtained, then a parser OM error is issued.
; Entry:
;	cx = cbExtra
; Exit:
;	ax = 0 if out-of-memory error
;	Flags set on value in ax
;
;*********************************************************************
PUBLIC	CheckFreeDstBuf 		
CheckFreeDstBuf PROC NEAR
	PUSHI	ax,<dataOFFSET ps.PS_bdpDst>
	push	cx			;cbExtra
	call	BdCheckFree
	or	ax,ax
	je	CheckFreeOMError	;brif out-of-memory
CheckFreeExit:
	ret
CheckFreeDstBuf ENDP

CheckFreeOMError:
	call	ParseErrOm		;Error "Out of memory"
	sub	ax,ax
	jmp	SHORT CheckFreeExit

		
;*********************************************************************
; VOID NEAR Emit16(code16)
; Purpose:
;	Emit 16 bits of pcode to parser's pcode buffer ps.bdpDst
;
;*********************************************************************
cProc	Emit16,<PUBLIC,NEAR,NODATA>
	parmW	code16
cBegin	Emit16
	mov	ax,code16
	call	Emit16_AX
cEnd	Emit16

;**********************************************************************
; VOID NEAR Emit16_0
; Purpose:
;	Emit 16 bits of 0 to parser's pcode buffer ps.bdpDst
;
;**********************************************************************
;**********************************************************************
; VOID NEAR Emit16_AX
; Purpose:
;	Emit 16 bits (in AX) to parser's pcode buffer ps.bdpDst
;
;**********************************************************************
PUBLIC	Emit16_0
PUBLIC	Emit16_AX
Emit16_0 PROC NEAR
	sub	ax,ax			;ax = 0
Emit16_0 ENDP				;fall into Emit16_AX
Emit16_AX PROC NEAR
	DbChkPsStk			;see if this is a new high-water stack
	mov	bx,[ps.PS_bdpDst.BDP_cbLogical]
	inc	bx
	inc	bx
	cmp	bx,[ps.PS_bdpDst.BDP_cbPhysical]
	ja	Emit16Grow		;brif dest buffer needs to grow
Emit16GotRoom:
	mov	[ps.PS_bdpDst.BDP_cbLogical],bx
	mov	bx,[ps.PS_bdpDst.BDP_pbCur]
	mov	[bx],ax			;store pcode in buffer
	inc	bx
	inc	bx
	mov	[ps.PS_bdpDst.BDP_pbCur],bx
Emit16Exit:
	ret
Emit16_AX ENDP
Emit16Grow:
	push	ax			;save pcode to be emitted
	push	bx			;save new value of cbLogical
	mov	cx,2			;need 2 more bytes
	call	CheckFreeDstBuf 	;see if enough free space
	pop	bx			;restore new value of cbLogical
	pop	ax			;restore pcode to be emitted
	jne	Emit16GotRoom		;brif buffer grown successfully
	jmp	SHORT Emit16Exit

PUBLIC	Emit32
cProc	Emit32,<PUBLIC,NEAR,NODATA>
	parmW	code32HIGH
	parmW	code32LOW
cBegin	Emit32
	mov	ax,code32LOW
	call	Emit16_AX		;Emit least significant word first
	mov	ax,code32HIGH
	call	Emit16_AX		;Emit most significant word second
cEnd	Emit32

;*********************************************************************
; VOID NEAR EmitSrc(oSrcText, cbText)
;
; Purpose:
;	Copy bytes from source buffer ps.bdpSrc to pcode buffer ps.bdpDst.
;
; Entry:
;	oSrcText is an offset into the source buffer for 1st byte to copy
;	cbText = number of bytes to be copied
;
; Exit:
;	<text> is copied from the source buffer to the destination pcode buffer.
;	If cbText is odd, an extra filler byte is appended to <text> to keep
;	pcode buffer even-byte aligned.
;	If out-of-memory error, ps.errCode = ER_OM
;
;*********************************************************************
cProc	EmitSrc,<PUBLIC,NEAR,NODATA>,<si,di>
	parmW	oSrcText
	parmW	cbText
cBegin	EmitSrc
	mov	di,cbText		;round cbText (si) up to even value
	inc	di
	and	di,0FFFEH
	mov	cx,di
	call	CheckFreeDstBuf 	;make sure enough free space exits
					;in pcode buffer
	je	EmitSrcExit		;brif out-of-memory

;	set si -> where to copy chars from in bdpSrc
	mov	si,[ps.PS_bdpSrc.BDP_pb]
	add	si,[oSrcText]		


;	push arguments for call
	PUSHI	ax,<dataOFFSET ps.PS_bdpDst>
	push	si
	push	di
	;BdAppend((bd *)(&ps.bdpDst), ps.bdpSrc.pb + oSrcText, cbText)
	call	BdAppend
	DbAssertRel  ax,nz,0,CP,<EmitSrc: BdAppend returned Out of Memory>

	add	[ps.PS_bdpDst.BDP_pbCur],di  
EmitSrcExit:				
cEnd	EmitSrc

;*********************************************************************
; VOID NEAR EmitSrcCompress(oSrcText, cbText, oDstCbRem)
;
; Purpose:
;	Copy bytes from source buffer ps.bdpSrc to pcode buffer ps.bdpDst.
;	Runs of characters longer than 3 bytes will be compressed and
;	encoded as follows:
;	   Byte 1 - PS_EncodedText
;	   Byte 2 - repeat count for char
;	   Byte 3 - char
;	Chars with ASCII values equal to PS_EncodedText are represented
;	as an encoded char with byte count >= 1.
;
; Entry:
;	oSrcText is an offset into the source buffer for 1st byte to copy
;	cbText = number of bytes to be copied
;	oDstCbRem = offset to emitted cb for this REM
;
; Exit:
;	Encoded <text> is copied from the source buffer to the
;	  destination pcode buffer.
;	If cbText is odd, an extra filler byte is appended to <text> to keep
;	  pcode buffer even-byte aligned.
;	If out-of-memory error, ps.errCode = ER_OM
;
;*********************************************************************
cProc	EmitSrcCompress,<PUBLIC,NEAR>,<si,di>
	parmW	oSrcText
	parmW	cbText
	parmW	oDstCbRem
	localW	cbLeft
	localW	cbCompressed
	localW	cbRep
	localB	chPrev
cBegin	EmitSrcCompress
	push	[ps.PS_bdpDst.BDP_cbLogical] ;save current size of dest

	sub	ax,ax
	mov	[cbCompressed],ax
	mov	[chPrev],al
	inc	ax
	mov	[cbRep],ax
	mov	cx,[cbText]		;round cbText (si) up to even value
	mov	[cbLeft],cx
	inc	cx
	and	cl,0FEH
	jcxz	J1_CompressSkip 	;brif no text
	call	CheckFreeDstBuf 	;make sure pcode buffer has enough room
	jne	CompressTxt		;brif enough room
J1_CompressSkip:
DJMP	jmp	SHORT CompressSkip	;space, brif out-of-memory

CompressTxt:
	push	ds
	pop	es			;es=ds
	mov	si,[oSrcText]
	add	si,[ps.PS_bdpSrc.BDP_pb]
	mov	di,[ps.PS_bdpDst.BDP_pbCur]

;	si = pSrcBuffer 	di = pDstBuffer
;	[cbLeft] = count of bytes remaining to process in src buffer
;	[cbCompressed] = count of bytes compressed from src buffer
;	[chPrev] = last character processed
;	[cbRep] = count of repeated [chPrev] chars processed so far

CompressLoop:
	lodsb				;al = next char
	cmp	al,[chPrev]		;same as last char?
	je	SameChar		;brif so

StartNewRun:				
; Current char does not match last char.  If repeat count for last
; char is > 3 then compress repeated chars out of emitted text.
	call	CompressText		;try to compress txt of last char string
	stosb				;emit current char
	cmp	al,STR_EncodedText	;does this happen to match the
					;special Encode char?
	je	GotSpecialChar		;brif so, do special processing

NoCompress:
	mov	[cbRep],1
	jmp	short CompressNext

; We have a run of identical chars.  If char is "special char" then
; just increment emitted count of special chars.  Else increment
; count of chars seen, and emit the char

SameChar:
	cmp	al,STR_EncodedText	;is this the "special char"?
djmp	je	RunOfSpecialChars	;brif so, bump count in emit buf
	cmp	[cbRep],0FFh		; enough room for this one?
	je	StartNewRun		; brif not -- start a new run
	stosb				;emit char
	inc	[cbRep]
DbAssertRel [cbRep],be,0FFH,CP,<EmitSrcCompress: err1>

CompressNext:
	mov	[chPrev],al		;save new "Last char seen"
	dec	[cbLeft]		;dec remaining char count
	jne	CompressLoop		;brif more to process

CompressDone:
	call	CompressText		;try to compress txt of last char string
	mov	di,[oDstCbRem]		;get offset to emitted cbRem
	add	di,[ps.PS_bdpDst.BDP_pb] ;convert to ptr
	mov	ax,[cbCompressed]
	sub	[di],ax 		;adjust emitted cb for compression
	neg	ax
	add	ax,[cbText]		;adjust and pad size of original
	inc	ax			; for compression
	and	al,0FEH
	add	[ps.PS_bdpDst.BDP_pbCur],ax ;adjust for added text
	pop	[ps.PS_bdpDst.BDP_cbLogical] ;recover entry size
	add	[ps.PS_bdpDst.BDP_cbLogical],ax ;adjust by inserted size
CompressExit:
cEnd	EmitSrcCompress

CompressSkip:
	pop	ax			;clean stack
	jmp	SHORT CompressExit	;and exit quick

;=====================================================================
; We have encountered a char which matches our special flag byte.
; We will emit it as a compressed char record. Note: we have already
; emitted the char by this point.
;=====================================================================

GotSpecialChar:
	add	[cbCompressed],2	;compression record costs us 2 bytes
	js	GotSpecGrow		;brif we need to grow dest buffer

GotSpecCont:
	mov	ah,al			;mov char into high byte
	mov	al,1			;set initial count of chars
	stosw				;emit count and char
	mov	al,ah			;recover char
	jmp	SHORT CompressNext	;process next char

GotSpecGrow:
; We actually need to grow the destination buffer because we have inserted
; enough "special chars" to override the compression savings.
	sub	si,[ps.PS_bdpSrc.BDP_pb] ;conv ptr to offset in case of movement
	sub	di,[ps.PS_bdpDst.BDP_pb] ;conv ptr to offset in case of movement
	mov	cx,2			;size of compression record is 2 bytes
	call	CheckFreeDstBuf 	;ensure enough room in pcode buffer
	je	CompressSkip		;brif out-of-memory
	add	si,[ps.PS_bdpSrc.BDP_pb] ;conv offset back to ptr
	add	di,[ps.PS_bdpDst.BDP_pb] ;conv offset back to ptr
	mov	al,STR_EncodedText	;get back char
	jmp	short GotSpecCont

;=====================================================================
; We are processing a run of special chars. The first one caused us
; to emit an encoded record for the char, so just bump the count
; of these chars.
;=====================================================================

RunOfSpecialChars:
DbAssertRelB [di-3],e,STR_EncodedText,CP,<EmitSrcCompress: err2>
DbAssertRelB [di-1],e,STR_EncodedText,CP,<EmitSrcCompress: err3>

	inc	byte ptr [di-2] 	;bump count of special chars
	jz	OverFlow		; brif too many
	inc	[cbCompressed]		;compressed 1 more char
	jmp	CompressNext

OverFlow:				
	dec	byte ptr [di-2] 	; restore count of special chars
	jmp	StartNewRun		; start another run


;*********************************************************************
; CompressText
;
; Purpose:
;	Compress text in buffer DI if possible.
;	Note: this routine should only be called from EmitSrcCompress,
;	as it utilizes frame variables defined by EmitSrcCompress.
;
; Entry:
;	[cbRep] = repeat count for last char in buffer.
;	[cbCompressed] = current count of compressed chars in buffer.
;
; Exit:
;	[cbCompressed] updated if text was compressed
;	DI updated for compression
; Preserves:
;	AX
;*********************************************************************
CompressText:
	push	ax			;save chCur
	mov	ax,[cbRep]		;get repetition factor for chLast

	cmp	al,3			;Don't compress if repeat count < 3
	jbe	CompressTextExit
	sub	di,ax			;back up pDst to first repeated char
	add	[cbCompressed],ax	;bump count of compressed bytes
	sub	[cbCompressed],3	; - compression overhead (2)
					; - 1 (make cbRep 0 relative)
	mov	ah,al			;high byte has count
	mov	al,STR_EncodedText	;low byte has encoded flag
	stosw				;emit encoded flag/cb encoded
	inc	di			;skip char
	mov	byte ptr[di],0		;zero potential extra byte

CompressTextExit:
	pop	ax			;recover chCur
	ret


;*********************************************************************
; boolean ListStdMsgToBd(iMsg, pbdDst)
;
; Purpose:
;	List a standard ASCII message to the end of a buffer.
;
; Entry:
;	iMsg is standard error index from qbimsgs.h
;	pbdDst points to buffer descriptor where message is to be listed
;
; Exit:
;	If out-of-memory error, returns FALSE
;	else returns TRUE
;
;*********************************************************************
cProc	ListStdMsgToBd,<PUBLIC,FAR,NODATA>
	parmW	iMsg
	parmW	pbdDst			
cBegin	ListStdMsgToBd
	push	[iMsg]
	call	ListStdMsgFar		;copy text of msg to bufStdMsg
					; ax = # bytes in message
	;return(BdAppend(pbdDst, pMsgText, cbText))
	push	[pbdDst]			;pass ptr to destination buffer
	PUSHI	bx,<dataOFFSET bufStdMsg>
					;pass ptr to 1st byte of text
	push	ax			;push byte count
	call	BdAppend		;ax = FALSE if out-of-memory
ListMsgExit:
cEnd	ListStdMsgToBd


;*************************************************************************
; ListIRW 
; Purpose:
;	Map a reserved word from index (0..n) to ASCII.
;	Used by user-interface's context sensitive help.
; Entry:
;	parm1: reserved word index (0,1,2,...n)
; Exit:
;	returns byte count (0 if index is too large).
;	zero terminated string for reserved word is copied to bufStdMsg
;
;*************************************************************************
cProc	ListIRW,<PUBLIC,FAR>,<si,di>
	parmW	iRw
	localW	pbDst			
	localW	iRwCur
	localB	letterCur
cBegin
	mov	ax,IRW_ALPHA_FIRST
	mov	[iRwCur],ax		;initialize cur res word counter/index
	mov	bx,[iRw]		;bx=reserved word of interest
	cmp	bx,ax
	jae	NotSpecChar		;brif iRw represents a word from
					; res word table, and not a special
					; char like +, *,$ etc.
	mov	al,BYTE PTR cs:mpIRWtoChar[bx]
	mov	[letterCur],al
	sub	cx,cx			;cbNam = 0
	jmp	SHORT SrchEnd

NotSpecChar:
	mov	[letterCur],'A'
	mov	ax,cs:[tRw]		;ax points to A's res word tbl
	inc	ax			;skip IRW for 1st entry in table
	inc	ax
	xchg	si,ax			;si points into A's res word tbl
	push	cs
	pop	es			;es = CP segment (for GetRwFromTab)

;Register usage:
;	si->current res word, cx = cbNam, dx=cbAtr
;	es = segment adr of reserved word table (CP)
;
SrchLoop:
	cmp	BYTE PTR cs:[si],0
	jne	NotEndOfTbl		;brif not at end of current table

	;we just moved into next letter's reserved word table
	inc	[letterCur]
	sub	dx,dx			;prepare to return 0
	cmp	[letterCur],'Z'
	ja	ListEnd 		;brif IRW not found (return 0)
	add	si,3			;skip 0-byte terminator and
	jmp	SHORT SrchLoop		; iRw for 1st entry in next table

NotEndOfTbl:
EXTRN	GetRwFromTabCP:near
	call	GetRwFromTabCP		;cx = size of res word's name
					;dx = size of res word's atr block
					;si points to 1st byte of res word name
	mov	ax,[iRwCur]		;ax = current reserved word's index
	inc	[iRwCur]
	cmp	ax,[iRw]
	je	SrchEnd			;brif current res word is one
					; we've been looking for
	add	si,cx			;skip cbNam bytes
	add	si,dx			;skip cbAtr bytes
	jmp	SHORT SrchLoop

;[letterCur] = 1st letter of reserved word
;cx = # bytes in reserved word, excluding first char
;
SrchEnd:
	mov	dx,cx
	inc	dx			;dx = real number of bytes in res word
	push	ds
	pop	es			;es=ds for block transfer
	mov	di,DATAOFFSET bufStdMsg
	mov	al,[letterCur]
	stosb				;store 1st char in buffer
	jcxz	ListExit		;branch if 1 letter res word (like +)
OutLoop:
	lods	BYTE PTR cs:[si]	;al = next letter of res word
	stosb				;store it in buffer
	loop	OutLoop			;until cx=0
	lods	BYTE PTR cs:[si]	;al = flags byte
	test	al,RWF_STR
	je	ListExit		;brif doesn't end with '$'
	mov	al,'$'
	stosb				;store it in buffer
	inc	dx			;dx = real number of bytes in res word
ListExit:
	xchg	ax,cx			;ax = 0
	stosb				;store 0-terminator
ListEnd:
	xchg	ax,dx			;return result in ax
cEnd


subttl	Error reporting functions

;=======================================================================
;	  E R R O R    R E P O R T I N G    F U N C T I O N S
;
; Example:
;	Assume the statement we are parsing is defined by the parse tree:
;
;	            A
;	           / \
;	          B   C
;	           \   \
;	            +---+
;	                 \
;	                 <accept>
;
; and assume A is a non-terminal which is described by the parse tree:
;
;	            X
;	           / \
;	          /   Z
;	          \    \
;	           +----+
;	                 \
;	                 <accept>
;
; If Parse(A) fails to match A or B, we want to produce the error message
; "Expected X or B" (since A is really known to the user as X).
;	The way this is accomplished is as follows:
;	- Every NonTerminal parsing function (like Parse() and Ntxxx())
;	  returns 1 of 3 values:
;	  PR_GoodSyntax if tokens were recognized & pcode emitted
;	  PR_NotFound if tokens were not recognized, and no tokens were consumed
;	  PR_BadSyntax if some tokens got consumed before we detect a syntax
;	   error.  In this case, the NonTerminal parsing function is responsible
;	   for generating a complete error message by calling one or more of
;	   the functions: PErrState(), PErrExpectedOr(), PErrMsg(),
;	     PErrExpMsg(), PErrExpRw().
;
;  Control Flow:
;
;	      PErrState
;                 |
;           +-----+----+
;           |          |
;       PErrExpMsg PErrExpRw
;           |
;       PErrMsg_AX ParseErrOm ParseErr0 PErrVarMgr ParseErrTokScan
;           |           |        |          |           |
;           +-----------+--------+----------+-----------+
;                       |
;	           ParseErr
;
;=====================================================================

;*********************************************************************
; void NEAR ParseErr(ax:errCode, bx:pTokErr)
; Purpose:
;	Record the fact that a parser error has occurred.
; Entry:
;	ax = errCode is a standard error code from qbimsgs.h or
;	     PSERR_fAsciiMsg with 0 or more of: PSERR_fAlert, PSERR_fRude
;	bx = pointer to token where error occurred (used for column
;	     error reporting).
;	     bx = 0 if caller doesn't know what token caused the error.
; Exit:
;	psFlags bit PSIF_fBindVars is reset so we don't continue binding
;	   variables in a statement which is already known to be bad
;
;********************************************************************/

PUBLIC	ParseErrOm
ParseErrOm PROC NEAR
	mov	ax,ER_OM OR PSERR_fAlert
ParseErrOm ENDP
	;fall into ParseErr0
PUBLIC	ParseErr0
ParseErr0 PROC NEAR
	sub	bx,bx			;token/source-column = unknown
ParseErr0 ENDP
	;fall into ParseErr
PUBLIC	ParseErr
ParseErr PROC NEAR
	mov	dx,ax
	and	dx,PSERR_errcode
	cmp	dx,ER_CN
	jne	NotCantCont

	;variable mgr and context mgr return ER_CN if asked to grow a
	;variable table when CONT is possible.  Since variable tables
	;can't move during program execution, either AskCantCont will
	;disable CONT, or the edit must be backed out of.
	
	DbAssertRelB [txdCur.TXD_scanState],ne,SS_RUDE,CP,<ParseErr: err1>
	;If this assertion failed, we could be in an infinite loop of retries.

	call	AskCantCont_CP		;ask user "Want to back out?"
	mov	al,PSF_UndoEdit
	je	BackOut			;brif user wants to back out
	mov	al,PSF_fRetry		;tell caller to call ParseLine again
BackOut:
	or	[ps.PS_flags],al
	mov	al,ER_IER		;this error should never get to user
;ax = errCode
NotCantCont:
	mov	[ps.PS_errCode],ax
	or	bx,bx
	je	PerrNoOSrc		;brif we don't know column of error
	mov	bx,[bx.TOK_oSrc]	;bx = token's source line offset
PerrNoOSrc:
	mov	[ps.PS_oSrcErr],bx
	and	[psFlags],NOT PSIF_fBindVars
	ret
ParseErr ENDP

;*********************************************************************
; void NEAR ParseErrTokScan(ax:errCode)
; Purpose:
;	Same as ParseErr.  This should be called if the caller wants
;	to flag an error, but continue checking for bad syntax.
;	If a syntax error is found later in the line, it will
;	over-write this error message.  Call PErrMsg_AX if
;	syntax analysis of the line is to stop.
; Entry:
;	ax = errCode is a standard error code from qbimsgs.h or
;	     PSERR_fAsciiMsg with 0 or more of: PSERR_fAlert, PSERR_fRude
;	[pTokScan] = pointer to token where error occurred (used for column
;	     error reporting).
; Exit:
;	Same as ParseErr
;
;********************************************************************/
PUBLIC	ParseErrTokScan
ParseErrTokScan PROC NEAR
	mov	bx,[pTokScan]
	jmp	SHORT ParseErr
ParseErrTokScan ENDP

;*********************************************************************
; void NEAR PErrVarMgr(ax:errCode, bx:pTokErr)
; Purpose:
;	Handle error code returned by MakeVariable
;	Note that this can be an error returned by ScanAndExec as well.
;	If PRS_ER_RE bit is set (Rude edit error)
;	   set PSERR_fRude bit in ParseLine's return value
;	Else If PRS_ER_RP bit is NOT set (passive reparse error)
;	   set PSERR_fAlert bit in ParseLine's return value
;	Else
;	   caller of ParseLine may defer reporting the error until
;	   the user tries to run the program.
; Entry:
;	ax = errCode is an error code as returned by MakeVariable
;	bx = (used for column
;	     error reporting).
;	     bx = 0 if caller doesn't know what token caused the error.
; Exit:
;	psFlags bit PSIF_fBindVars is reset so we don't continue
;	   binding variables in a statement which is bad
;	We will continue to check rest of statement for syntax errors,
;	   since these are stronger (more useful to user) than
;	   variable manager errors.  If any syntax errors are found,
;	   any information recorded in ps.err... by this function
;	   will be overwritten.
;
;********************************************************************/
PUBLIC	PErrVarMgr
PErrVarMgr PROC NEAR
	test	ah,(PRS_ER_RP / 100h) AND 7Fh
	jne	ReparseErr		;brif Reparse Error
	test	ah,(PRS_ER_RE / 100h) AND 7Fh
	.errnz	PSERR_fRude AND 0FFH
	mov	ah,PSERR_fRude / 100h	;set rude edit flag in result
	jne	BindRude		;brif a rude edit
	.errnz	PSERR_fAlert AND 0FFH
	mov	ah,PSERR_fAlert / 100h	;set ALERT flag in result
					;this error is reported at entry
					;time, rather than waiting until
					;the pre-run reparse loop
	SKIP2_PSW			;skip following mov ah,0 instruction
ReparseErr:
	mov	ah,0			;DON'T alter condition codes
BindRude:
	jmp	SHORT ParseErr		;ParseErr(ax,bx)
					; result doesn't matter because of error
PErrVarMgr ENDP

;error "id can't end with % & ! # or $", al=PR_BadSyntax on exit
;
PUBLIC	PErrExpIdImp
PErrExpIdImp PROC NEAR
	mov	ax,MSG_IdImp		;"id can't end with % & ! # or $"
PErrExpIdImp ENDP
;fall into PErrMsg_AX
;*********************************************************************
; VOID NEAR PErrMsg_AX, PErrPrevTok_AX, PErrMsg_AX_BX
;
; Purpose:
;	Append the standard message identified by iMsgErr to current error line.
;
; Entry:
;	ax = error code (from qbimsgs.inc)
;	pTokScan->oSrc is used for column offset by PErrMsg_AX
;	OR
;	bx->oSrc is used for column offset by PErrMsg_AX_BX [09]
;
; Exit:
;	al = PR_BadSyntax, condition codes set based on al
;
;*********************************************************************
;PErrPrevTok_Ax uses the column offset of the last token consumed 
;for reporting the error
PUBLIC	PErrPrevTok_AX			
PErrPrevTok_AX PROC NEAR		
	mov	bx,[pTokLastConsumed]	;bx -> last consumed token
	; fall into PErrMsg_AX_BX	
	jmp	SHORT PErrMsg_Ax_Bx	
PErrPrevTok_AX ENDP			

PUBLIC	PErrMsg_AX
PErrMsg_AX PROC NEAR
	mov	bx,[pTokScan]
	; fall into PErrMsg_AX_BX	
PErrMsg_AX ENDP				

cProc	PErrMsg_AX_BX,<PUBLIC,NEAR>	
cBegin	PErrMsg_AX_BX			
	test	BYTE PTR([ps.PS_errCode+1]),PSERR_fAsciiMsg / 100h
	jne	GotEos1			;brif not 1st msg for this line
	test	BYTE PTR([ps.PS_errCode+1]),PSERR_fAlert / 100h
	jne	NoOmErr			;brif already got an error not
					; consisting of pasting together
					; messages into ps.bdErr (like
					; Id too long, or Out of memory)
					;This still overrides errors like ER_DD
					; so syntax errors get reported before
					; variable mgr re-parse type errors
					; (because a syntax error is more
					;  valuable to the user).

	push	ax			;save msg id
	mov	[ps.PS_bdErr.BD_cbLogical],0 ;reset bdErr buffer

	mov	ax,PSERR_fAsciiMsg + PSERR_fAlert
					;tells ParseLine's caller that err msg
					; is in ps.bdErr
	call	ParseErr		;set ps.errCode, ps.oSrcErr
GetEos:
	call	NtEndStatement
	jne	GotEos			;brif current token is end-of-stmt
	call	ScanTok
	jmp	SHORT GetEos

GotEos:
	pop	ax			;restore ax = msg id
GotEos1:
	push	ax			;pass msgId to ListStdMsgToBd
	PUSHI	ax,<dataOFFSET ps.PS_bdErr>
	call	ListStdMsgToBd		;append std msg to error buffer
	or	ax,ax
	jne	NoOmErr			;brif not out-of-memory
	call	ParseErrOm		;Error "Out of memory"
NoOmErr:
	mov	al,PR_BadSyntax
	or	al,al			;set condition codes for caller
cEnd	PErrMsg_AX_BX

;*********************************************************************
; VOID NEAR PErrExpectedOr()
;
; Purpose:
;	If this is the 1st clause of this err msg, output 'Expected: '
;	Otherwise output ' or '.
;
; Exit:
;	al = PR_BadSyntax, condition codes set based on al
;
;*********************************************************************
PErrExpectedOr PROC NEAR
	mov	ax,MSG_or
	test	BYTE PTR([ps.PS_errCode+1]),PSERR_fAsciiMsg / 100h
	jne	OrNot1stMsg		;brif not 1st msg for this line
	mov	ax,MSG_expected
OrNot1stMsg:
	jmp	SHORT PErrMsg_AX	;emit msg, al = PR_BadSyntax
					; return to caller
PErrExpectedOr	ENDP

;*********************************************************************
; VOID NEAR PErrExpRw_AX
;
; Purpose:
;	Produce the error message:  "Expected <reserved word>"
;
; Entry:
;	ax = IRW for expected reserved word (from prsirw.inc)
; Exit:
;	al = PR_BadSyntax
; 
;*********************************************************************
cProc	PErrExpRw_AX,<PUBLIC,NEAR,NODATA>,<si>
cBegin
	xchg	si,ax			;si = IRW for expected reserved word
	DbAssertRel si,be,NTOKENS,CP,<Illegal token in PErrExpRw>
	;list ASCII this opcode maps to
	call	PErrExpectedOr		;output "Expected" or "or"
	cmp	si,IRW_NewLine
	jne	NotNewLine
	mov	ax,MSG_eos
	call	PErrMsg_AX		;output "end of statement"
	jmp	SHORT PErrExpRwExit

NotNewLine:
	push	si
	call	ListIRW			;output reserved word's name to
					; bufStdMsg, ax = byte count
	DbAssertRel ax,ne,0,CP,<ListIRW called with bad IRW>

	PUSHI	bx,<dataOFFSET ps.PS_bdErr>	;pass ptr to destination buffer
	PUSHI	bx,<dataOFFSET bufStdMsg>
					;pass ptr to 1st byte of text
	push	ax			;push byte count
	call	BdAppend		
PErrExpRwExit:
	mov	al,PR_BadSyntax		;return PR_BadSyntax
cEnd

;error "Expected id", al=PR_BadSyntax on exit
;
PUBLIC	PErrExpId
PErrExpId PROC NEAR
	mov	ax,MSG_ExpId
;;	jmp	SHORT PErrExpMsg_AX
PErrExpId ENDP
;fall into PErrExpMsg_AX
;*********************************************************************
; VOID NEAR PErrExpMsg_AX
;
; Purpose:
;	Produce the error message:  "Expected <Standard BASIC Message>"
;
; Entry:
;	ax = error code (from qbimsgs.inc)
;
; Exit:
;	al = PR_BadSyntax, condition codes set based on al
;
;*********************************************************************
PUBLIC	PErrExpMsg_AX
PErrExpMsg_AX PROC NEAR
	push	ax			;save msgId
	call	PErrExpectedOr
	pop	ax			;restore ax = msgId
	jmp	PErrMsg_AX		;al = PR_BadSyntax
					;return to caller
PErrExpMsg_AX ENDP

if ND_ACCEPT - 0
	Error: PErrState assumes ND_ACCEPT = 0
endif
if ND_REJECT - 1
	Error: PErrState assumes ND_REJECT = 1
endif
if ND_MARK - 2
	Error: PErrState assumes ND_MARK = 2
endif
if ND_EMIT - 3
	Error: PErrState assumes ND_EMIT = 3
endif
if ND_BRANCH - 4
	Error: PErrState assumes ND_BRANCH = 4
endif

;*************************************************************************
; VOID NEAR PErrState
;
; Purpose:
;	Append an error message to the error buffer indicating
;	'A or B or C ...  or Z'  was expected.
;
; Entry:
;	ushort pStateLastScan - pointer into tState for expected syntax
;	  when last token was scanned.
;	ushort pStateLastGood - pointer into tState for expected syntax
;	  when last non-terminal was accepted.
;
; Exit:
;	al = PR_BadSyntax
;	  
;*************************************************************************
cProc	PErrState,<PUBLIC,NODATA,NEAR>,<si>
cBegin	PErrState
	test	BYTE PTR([ps.PS_errCode+1]),PSERR_fAlert / 100h
	jne	GotBadSyntax		;brif already got an error not
					; consisting of pasting together
					; messages into ps.bdErr (like
					; Id too long, or Out of memory)
					;This still overrides errors like ER_DD
					; so syntax errors get reported before
					; variable mgr re-parse type errors
					; (because a syntax error is more
					;  valuable to the user).
	mov	ax,[pStateLastScan]
	call	PErrState1		;PErrState(pStateLastScan - tState)
	test	BYTE PTR([ps.PS_errCode+1]),PSERR_fAsciiMsg / 100h
	jne	GotBadSyntax	      ;brif PErrState1 didn't generate anything
	mov	ax,[pStateLastGood]
	call	PErrState1
GotBadSyntax:
	mov	al,PR_BadSyntax
cEnd	PErrState

;Register usage:
;	dx = pStateTrue
;	cx = nodeId
;	si = pState
;
cProc	PErrState1,<NODATA,NEAR>,<si>
cBegin	PErrState1
	xchg	si,ax			;si = pState
	or	si,si
	jne	ErrLoop			;brif not NULL pState
J1_ErrLoopExit:
	jmp	ErrLoopExit		;brif ACCEPT or REJECT node (done)

ErrLoop:
	lods	BYTE PTR cs:[si]	;al = node id
	cmp	al,ND_REJECT
	jbe	J1_ErrLoopExit		;brif ACCEPT or REJECT node

	cmp	al,ND_EMIT
	ja	NotEmitOrMark		;brif not EMIT or MARK node
	lodsb				;bump si without changing status flags
	jne	ErrLoop			;brif MARK node (1 byte operand)
	inc	si			;EMIT has 2 byte operand
	jmp	SHORT ErrLoop

;got a branch, token, or nonterminal - fetch its id
NotEmitOrMark:
	sub	ah,ah			;ax = nodeId
	cmp	al,ENCODE1BYTE
	jb	OneByteNodeId		;brif 1 byte nodeId

	;nodeId = ((nodeId-ENCODE1BYTE) << 8) + (*pState++) + ENCODE1BYTE
	mov	ah,al
	lods	BYTE PTR cs:[si]	;ax = nodeId * 256 + next byte
	sub	ax,255 * ENCODE1BYTE
OneByteNodeId:	
	xchg	cx,ax			;save nodeId in cx
;node is followed by 1 or 2 byte branch operand, fetch it
	lods	BYTE PTR cs:[si]
	sub	ah,ah			;ax = operand
	cmp	al,ENCODE1BYTE
	jb	OneByteOperand		;brif 1 byte operand (< ENCODE1BYTE)
	cmp	al,255
	je	HandleNode		;brif special id->accept node
					; never generated for a BRANCH node,
					; which is the only reason we care
					; about the operand in this function
	;operand is 2 byte offset into tState
	;operand = ((operand-ENCODE1BYTE) << 8) + *pState++
	mov	ah,al
	lods	BYTE PTR cs:[si]	;ax = operand * 256 + next byte
	sub	ax,256 * ENCODE1BYTE
	add	ax,OFFSET CP:tState
	xchg	dx,ax			;pStateTrue = &tState[operand]
	jmp	SHORT HandleNode

OneByteOperand:
	;operand is 1 byte relative branch in state table
	mov	dx,ax			;dx = offset to new state
	add	dx,si			;pStateTrue = pState + operand
	cmp	al,ENCODE1BYTE/2
	jbe	HandleNode		;brif positive relative branch
	sub	dx,ENCODE1BYTE		;negative relative branch
HandleNode:
	xchg	ax,cx			;ax = nodeId
	cmp	ax,ND_BRANCH
	jne	NotBranchNode
	mov	si,dx			;unconditional branch to another state,
					; pState = pStateTrue
	jmp	SHORT ErrLoop		; continue scanning

NotBranchNode:
	sub	ax,ND_BRANCH + 1
	cmp	ax,NUMNTINT
	jae	NotIntNt

	;we expected a non-terminal described by a state tree
	xchg	bx,ax
	shl	bx,1
	mov	ax,cs:tIntNtDisp[bx]
	add	ax,CPOFFSET tState	;ax = pState
	call	PErrState1		;recurse
J1_ErrLoop:
	jmp	SHORT ErrLoop

NotIntNt:
	sub	ax,NUMNTINT
	cmp	ax,NUMNTEXT
	jae	NotExtNt

	;we expected a non-terminal defined by a C function
	xchg	bx,ax
	shl	bx,1
	mov	ax,cs:tExtNtHelp[bx]
					;iMsg = tExtNtHelp[nodeId]
	or	ax,ax
	je	J1_ErrLoop		;brif no error msg for this nonterminal
	call	PErrExpMsg_AX		;emit the message
	jmp	SHORT J1_ErrLoop

NotExtNt:
	;we expected a reserved word:  Error "Expected <reserved word>"
	sub	ax,NUMNTEXT		;ax = nodeId
	call	PErrExpRw_AX		;PErrExpRw(ax:nodeId)
	jmp	SHORT J1_ErrLoop

ErrLoopExit:
cEnd	PErrState1
sEnd	CP




end
