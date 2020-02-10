	TITLE	prslex.asm - Parser's Lexical Analyzer (token fetcher)

;=============================================================================
; prslex.asm - Parser's Utility Functions
;
; Copyright <C> 1985, Microsoft Corporation
;
; Purpose:
;	Contains QBI Lexical Analyzer Functions
;
;
;=============================================================================

	include version.inc
	PRSLEX_ASM = ON
	includeOnce	architec
	includeOnce	context
	includeOnce	exint		;needed for CoR4R8
	includeOnce	heap
	includeOnce	names
	includeOnce	parser
	includeOnce	prstab
	includeOnce	prsirw
	includeOnce	psint
	includeOnce	qbimsgs
	includeOnce	rtinterp
	includeOnce	txtmgr
	includeOnce	variable	;for FVI_FNNAME [3]

	assumes DS,DGROUP
	assumes SS,DGROUP
	assumes ES,NOTHING


sBegin	DATA

	extrn	$i8_inpbas:byte		; inpbas now supported in AltMath


;List of bytes which, when they follow digits, mean its not an I2
tbFloat	label byte
	db	'.' - '0'
	db	'e' - '0'
	db	'E' - '0'
	db	'd' - '0'
	db	'D' - '0'
	db	'&' - '0'
	db	'!' - '0'
	db	'#' - '0'
	db	'%' - '0'  ;even though % means integer, its not fast-path
			   ;through the code (would slow down typical cases
			   ;to check for % in TryI2)
CB_tbFloat EQU $-tbFloat

sEnd	DATA

sBegin	CP
assumes CS,CP

QUOTE	=	022H			;quote char (")

; It is assumed that all classes of characters < TC_PERIOD are valid
; in identifiers.

OrdConstStart	0
OrdConst	TC_UCASE	;* Upper Case Alpha
OrdConst	TC_LCASE	;* Lower Case Alpha
OrdConst	TC_NUM		;* numeric (or .)
OrdConst	TC_PERIOD	;* "." (record separator / number / part of id)
OrdConst	TC_NEWLINE	;* end-of-line
OrdConst	TC_WHITE	;* white space (tab or space)
OrdConst	TC_SPECIAL	;* special character
OrdConst	TC_PRINT	;* '?' (short hand for PRINT)
OrdConst	TC_RW	;* special character reserved word

typChar LABEL	BYTE
	DB	TC_NEWLINE ; NUL
	DB	TC_SPECIAL ; SOH
	DB	TC_SPECIAL ; STX
	DB	TC_SPECIAL ; ETX
	DB	TC_SPECIAL ; EOT
	DB	TC_SPECIAL ; ENQ
	DB	TC_SPECIAL ; ACK
	DB	TC_SPECIAL ; BEL
	DB	TC_SPECIAL ; BS
	DB	TC_WHITE   ; HT
		;Tabs are expanded to spaces in GetLineBd.
		;Don't be tempted to do it at lex time because
		;if ReParse, source buffer is copied to pcode.
	DB	TC_NEWLINE ; LF
	DB	TC_SPECIAL ; VT
	DB	TC_SPECIAL ; FF
	DB	TC_NEWLINE ; CR
	DB	TC_SPECIAL ; SO
	DB	TC_SPECIAL ; SI
	DB	TC_SPECIAL ; DLE
	DB	TC_SPECIAL ; DC1
	DB	TC_SPECIAL ; DC2
	DB	TC_SPECIAL ; DC3
	DB	TC_SPECIAL ; DC4
	DB	TC_SPECIAL ; NAK
	DB	TC_SPECIAL ; SYN
	DB	TC_SPECIAL ; ETB
	DB	TC_SPECIAL ; CAN
	DB	TC_SPECIAL ; EM
	DB	TC_SPECIAL ; SUB
	DB	TC_SPECIAL ; ESC
	DB	TC_SPECIAL ; FS
	DB	TC_SPECIAL ; GS
	DB	TC_SPECIAL ; RS
	DB	TC_SPECIAL ; US

	DB	TC_WHITE ; spc
	DB	TC_RW + IRW_EtSingle ; !
	DB	TC_RW + IRW_DQuote ; "
	DB	TC_RW + IRW_Lbs ; #
	DB	TC_RW + IRW_EtString ; $
	DB	TC_RW + IRW_EtInteger ; %
	DB	TC_RW + IRW_EtLong ; &
	DB	TC_RW + IRW_SQuote ; '
	DB	TC_RW + IRW_LParen ; (
	DB	TC_RW + IRW_RParen ; )
	DB	TC_RW + IRW_Mult ; *
	DB	TC_RW + IRW_Add ; +
	DB	TC_RW + IRW_Comma ; ,
	DB	TC_RW + IRW_Minus ; -
	DB	TC_PERIOD ; .
	DB	TC_RW + IRW_Div ; /
	DB	TC_NUM ; 0
	DB	TC_NUM ; 1
	DB	TC_NUM ; 2
	DB	TC_NUM ; 3
	DB	TC_NUM ; 4
	DB	TC_NUM ; 5
	DB	TC_NUM ; 6
	DB	TC_NUM ; 7
	DB	TC_NUM ; 8
	DB	TC_NUM ; 9
	DB	TC_RW + IRW_Colon ; :
	DB	TC_RW + IRW_SColon ; 
	DB	TC_RW + IRW_LT ; <
	DB	TC_RW + IRW_EQ ; =
	DB	TC_RW + IRW_GT ; >
	DB	TC_PRINT ; ?

	DB	TC_SPECIAL ; @
	DB	TC_UCASE ; A
	DB	TC_UCASE ; B
	DB	TC_UCASE ; C
	DB	TC_UCASE ; D
	DB	TC_UCASE ; E
	DB	TC_UCASE ; F
	DB	TC_UCASE ; G
	DB	TC_UCASE ; H
	DB	TC_UCASE ; I
	DB	TC_UCASE ; J
	DB	TC_UCASE ; K
	DB	TC_UCASE ; L
	DB	TC_UCASE ; M
	DB	TC_UCASE ; N
	DB	TC_UCASE ; O
	DB	TC_UCASE ; P
	DB	TC_UCASE ; Q
	DB	TC_UCASE ; R
	DB	TC_UCASE ; S
	DB	TC_UCASE ; T
	DB	TC_UCASE ; U
	DB	TC_UCASE ; V
	DB	TC_UCASE ; W
	DB	TC_UCASE ; X
	DB	TC_UCASE ; Y
	DB	TC_UCASE ; Z
	DB	TC_RW + IRW_LParen ; map '[' to '(' for BASICA compatibility
	DB	TC_RW + IRW_Idiv ; \
	DB	TC_RW + IRW_RParen ; map ']' to ')' for BASICA compatibility
	DB	TC_RW + IRW_Pwr ; ^
	DB	TC_SPECIAL; _ (underscore)

	DB	TC_SPECIAL ; `
	DB	TC_LCASE ; a
	DB	TC_LCASE ; b
	DB	TC_LCASE ; c
	DB	TC_LCASE ; d
	DB	TC_LCASE ; e
	DB	TC_LCASE ; f
	DB	TC_LCASE ; g
	DB	TC_LCASE ; h
	DB	TC_LCASE ; i
	DB	TC_LCASE ; j
	DB	TC_LCASE ; k
	DB	TC_LCASE ; l
	DB	TC_LCASE ; m
	DB	TC_LCASE ; n
	DB	TC_LCASE ; o
	DB	TC_LCASE ; p
	DB	TC_LCASE ; q
	DB	TC_LCASE ; r
	DB	TC_LCASE ; s
	DB	TC_LCASE 
	DB	TC_LCASE ; u
	DB	TC_LCASE ; v
	DB	TC_LCASE ; w
	DB	TC_LCASE ; x
	DB	TC_LCASE ; y
	DB	TC_LCASE ; z
	DB	TC_SPECIAL ; {
	DB	TC_SPECIAL ; |
	DB	TC_SPECIAL ; }
	DB	TC_SPECIAL ; ~
	DB	TC_SPECIAL ; DEL




subttl	FindRw

;================ External Procedures Referenced by this module ========


;***************************************************************************
; NEAR BOOL FindRw(pToken, pSym, cbSym, fStr)
; Purpose:
;	Search the reserved word table for a specified symbol.
;
; Entry:
;	'pToken' points to descriptor for current token being fetched
;	'pSym' points to the 1st byte of the symbol to search for.
;	'cbSym' = number of bytes in symbol (always assumed to be > 1)
;	'fStr' is true if the symbol was terminated by "$".
;	[ES] = [DS]
;	NOTE: This assumes reserved word table is ordered ALPHABETICALLY
;
; Exit:
;	If the reserved word was not found,
;	   returns with carry clear
;	else
;	   returns with carry set
;	The fields of the token structure are filled in with the description
;	of the reserved word.
;	[11]In EB FindRw may also determine that the string passed to it is a
;	command equivalent name in which case the token structure is filled
;	in describing it.
;
;*******************************************************************************
; Register allocation within procedure:
;	si is always pointing to next byte in reserved word table
;	di = temporary copy of pbSym
;	cx = temporary copy of cbSym

cProc	FindRw	<PUBLIC,NEAR>, <si,di>
	ParmW	pToken			;ptr to token being built
	ParmW	pSym			;ptr to 1st byte of name to search for
	ParmW	cbSym			;byte count of name to search for
	ParmB	fStr			;TRUE if name ends with '$'

	localW	nStmts			;num statements that begin with
					; this reserved word

cBegin	FindRw
	mov	si,[pSym]		;si points to 1st letter of id
	lodsb				;al = 1st letter of id
	dec	[cbSym] 		;decrement byte count for same reason
	sub	al,'A'			;al = (0..25) for (A..Z) 
	cbw	
	cmp	al,'Z'-'A'		
	ja	WordNotFound1		;brif first letter can not start RW
	shl	ax,1			;ax = word index into tRw
	xchg	si,ax			;si = index into tRw
	mov	si,WORD PTR cs:tRw[si]	;si->res word table for 1st letter of id

	;======================================================
	; NOTE: Beginning of block which has DS->CP (not DGROUP)
	;	If any static variables need be accessed in this
	;	block, access them with an ES override.
	;======================================================
	push	cs
	pop	ds			;ds = Code Segment (CP)
	assumes DS,CP

	lodsw				;[ax] = res word index for 1st entry
	mov	bx,ax			;iRw = res word index for 1st entry
	mov	cx,[cbSym]		;initialize
	jmp	SHORT NextEntry

;Register usage:
; dx points to next entry in reserved word table
; bx contains the res word index for current symbol
; si points into current reserved word table entry
; 
CbsInWord:				
	lodsw				;al = #bytes of attributes
					;ah = #bytes in reserved word
	jmp	SHORT GotCbs		

TryNextWord:
	mov	cx,[cbSym]		;cx = byte count for that name
TryNextWordCX:
	inc	bx
	mov	si,dx			;si points to start of next entry
NextEntry:
	lodsb				;al = size of next entry
					; high 4 bits = #bytes in name
					; low 4 bits = #bytes of attributes
	or	al,al
	jz	WordNotFound		;brif end of this res word table
	inc	al			
	jz	CbsInWord		;brif counts are in following word
	dec	al			

	mov	ah,al			;ah = #bytes in name * 16
	and	al,0FH			;al = #bytes of attributes
	shr	ah,1			;ah = #bytes in name * 8
	shr	ah,1			;ah = #bytes in name * 4
	shr	ah,1			;ah = #bytes in name * 2
	shr	ah,1			;ah = #bytes in name
GotCbs:
	mov	dl,ah			;dl = #bytes in name
	add	dl,al			;dl = #bytes in name & attributes
	sub	dh,dh			;dx = #bytes in name & attributes
	add	dx,si			;dx->next entry
	cmp	ah,cl
	jne	TryNextWordCX		;brif name lengths not equal
	mov	di,[pSym]
	inc	di			;di->2nd byte of name to search for
	rep cmpsb			;compare pSym with res word entry
	jc	TryNextWord		;brif pSym is alphabetically > this
					;entry in res word tbl.
	jne	WordNotFound		;brif names are not identical
					;and pSym is alphabetically < this
					;entry in res word tbl.
	lodsb				;al = flags byte
	sub	ah,ah			;ah = STRING:FALSE
	test	al,RWF_STR
	je	NotStr
	mov	ah,1			;ah = STRING:TRUE
NotStr:
	cmp	ah,[fStr]
	jne	TryNextWord		;brif one name ends with $ and
					; the other doesn't (no match)

;At this point, we have found a match in the reserved word table.
;
	push	ss
	pop	ds			;restore DS = DGROUP
	assumes DS,DGROUP
	;======================================================
	; NOTE: End of block which has DS->CP
	;======================================================
	mov	di,[pToken]		;di -> token descriptor being built
	mov	[di.TOK_class],CL_RESWORD
	mov	[di.TOK_rw_rwf],ax	;pToken->dsc.rwf = res word flags
	test	al,RWF_OPERATOR
	mov	ax,UNDEFINED
	je	NotOper 		;brif res word is not an operator
	lods	BYTE PTR cs:[si]	;al = IOP_xxx index for res word
	sub	ah,ah			;ax = IOP_xxx index for res word
NotOper:
	mov	[di.TOK_rw_iOperator],ax;save it in pToken
	mov	[di.TOK_rw_iRw],bx	;save res word's unique index
	mov	[di.TOK_rw_pArgs],si	;save ptr to func/stmt args
	stc				;tell caller reserved word was found
	jmp	SHORT FindRwExit	;exit function

WordNotFound:
	push	ss
	pop	ds			;restore DS = DGROUP
WordNotFound1:
	clc				;tell caller reserved word not found
FindRwExit:
cEnd	FindRw


subttl	FetchToken

;*********************************************************************
; VOID PLM NEAR FetchToken(di:pToken)
;
; Purpose:
;  This is the lexical analyzer for BASIC.  It scans
;  ASCII text and sets fields in the structure pToken
;  which identify the scanned token.
;  It filters out white space such as tabs, newlines, and spaces.
;
; Entry:
;  di (pToken) points to the receiving token descriptor structure.
;	ps.bdpSrc is a buffer descriptor which holds current line being parsed
;
; Exit:
;  The structure pointed to by pToken is filled in
;     id:
;	 pToken->class = CL_ID
;	 pToken->dsc.id.oNam = ONamOfPbCb offset for the identifier.
;	 pToken->dsc.id.oTyp = explicit type which followed
;	    identifier (ET_IMP for implicit, ET_I2 for % ET_R8 for # etc.)
;	 pToken->dsc.id.charFirst = [0..25] if 1st letter is [A..Z]
;	    used to determine default type for implicit variable references
;	 pToken->dsc.id.vmFlags has one or more of the following bits set:
;		FVI_FNNAME if id began with FN
;		(Even though MakeVariable ignores value of FVI_FNNAME in
;		 mkVar.flags, we set it for parser's own internal use).
;	 pToken->dsc.id.lexFlags has one or more of the following bits set:
;		FLX_hasPeriod if id has '.' in it
;     lit:
;	 pToken->class = CL_LIT
;	 pToken->dsc.lit.type = type of value (ET_I2, ET_SD etc.)
;	 pToken->dsc.lit.litType = type of literal (LIT_I2, LIT_H2, LIT_J2 etc.)
;	 pToken->dsc.lit.value.I2 = value of 2 byte integer.
;	 pToken->dsc.lit.value.I4 = value of 4 byte integer.
;	 pToken->dsc.lit.value.R4 = value of 4 byte real.
;	 pToken->dsc.lit.value.R8 = value of 8 byte real.
;	 pToken->dsc.lit.value.cbStr = number of bytes in string literal.
;
; Algorithm:
;	From first character, determine what class the token is,
;	and then take class specific action to consume it.
;
;*********************************************************************

cProc	FetchToken <PUBLIC,NEAR>, <si>
	localV	namBuf,CB_IDNAM_MAX+2	
	localW	cbId
	localW	endOfGO
	localW	tokFlags
	localB	firstChar
	localB	lastChar
	localB	charType

cBegin	FetchToken
	DbChkPsStk			;see if this is a new high-water stack
	and	[psFlags],NOT PSIF_fLexPeriodOk
	push	ds			;set up es register for string copy
	pop	es
	mov	si,[ps.PS_bdpSrc.BDP_pbCur]
SkipWhiteSpace_0:
	mov	ax,si			;ax -> next source byte to be fetched
	sub	ax,[ps.PS_bdpSrc.BDP_pb]
	mov	[di.TOK_oSrc],ax	;pToken->oSrc = ps.bdpSrc.pbCur -
					; ps.bdpSrc.pb	tells parser where
					; token began in source buffer
	sub	ah,ah			;ax = al (code below assumes ah=0)
	jmp	short SkipWhiteSpace_1
SkipWhiteSpace:
	inc	[di.TOK_oSrc]		;Just do this if we know we just read
					;  a single char
SkipWhiteSpace_1:
	lodsb				;al = next source byte
	mov	dl,al			;save current char in dl
	or	al,al
	js	NotASCII2		;branch if al is not a 7-bit ASCII char

	mov	bx,CPOFFSET typChar	;bx->table to map char to char-class
	xlat	BYTE PTR cs:[bx]	;al = ax = character type
NotASCII1:
	cmp	al,TC_RW
	jnb	ResWord			;brif this is a 'res word' char
					; like +,* etc.
; At this point we have either an alpha, a numeric, white space,
; end-of-line character, or a special character which BASIC doesn't consider
; part of its character set.  Dispatch based on the character-class.
; This can result in a numeric literal, id, reserved word, or special char.
; ax = character type, dl = char
;
NotResWord:
	mov	bx,ax
	shl	bx,1			;bx = index into dispatch table
	jmp	[bx + CaseJmpTbl]	;dispatch to case

ResWord:
	;We have a non-alpha-numeric char.  Try to map it
	; to a reserved word/operator
	
	sub	al,TC_RW		;al = ax = IRW (res word index)
	mov	[di.TOK_rw_iRw],ax	;save it in token descriptor
	mov	[di.TOK_class],CL_RESWORD
	mov	[di.TOK_rw_rwf],0	;res word is not a stmt/func
	mov	bx,OFFSET CP:mpIRWtoIOP ;bx -> table for mapping RW to IOP
	xlat	BYTE PTR cs:[bx]	;al = IOP (operator) index for res word
					;ax = IOP since ah is still 0
	or	al,al
	js	NotOperator		;brif this res word char isn't an
					; operator
	mov	[di.TOK_rw_iOperator],ax
	jmp	UpdateSrcPtr		;save si in ps.PS_bdpSrc.BDP_pbCur and exit

NotASCII2:
	mov	al,TC_SPECIAL		;all chars >127 are special chars
	jmp	SHORT NotASCII1

;The special character we parsed is not an operator,
; Try '&<number>' or "<quoted string literal>"
; dl = current source char, its not an operator
NotOperator:
	mov	[di.TOK_rw_iOperator],UNDEFINED
	cmp	dl,'&'
	je	GotLit			;brif numeric literal like &Hffff
	cmp	dl,QUOTE
	jne	J_UpdateSrcPtr		;branch if current char is not "
	mov	dx,si			;dx points to 1st char in literal
LoopInString:
	lodsb				;al = next byte of source
	cmp	al,QUOTE
	je	EndOfString
	or	al,al
	jne	LoopInString		;brif we're not at end-of-line
	dec	si			;so next call to FetchToken will
					; re-fetch end-of-line
	dec	dx			;adjust dx so length comes out right
EndOfString:
	mov	ax,si			;ax points beyond last char of literal
	sub	ax,dx			;ax = # chars since start of token
	dec	ax			;don't count the last double quote
	mov	WORD PTR [di.TOK_lit_value_cbStr],ax
	mov	[di.TOK_class],CL_LIT
	mov	[di.TOK_lit_type],ET_SD
	mov	[di.TOK_lit_litType],LIT_STR
	mov	WORD PTR ([di.TOK_lit_errCode]),0 ;set errCode and flags field
	.errnz	TOK_lit_flags - TOK_lit_errCode - 1
J_UpdateSrcPtr:
	jmp	UpdateSrcPtr		;save si in ps.PS_bdpSrc.BDP_pbCur and exit

	
sEnd CP
sBegin DATA
CaseJmpTbl	LABEL	WORD
	dw	Got_TC_ALPHA		;Upper Case
	dw	Got_TC_ALPHA		;Lower Case Alpha
	dw	GotLit			;0-9
	dw	Got_TC_PERIOD		;.
	dw	Got_TC_NEWLINE		;End-of-line Character
	dw	SkipWhiteSpace		;White Space Character
	dw	Got_TC_SPECIAL		;Special Character
	dw	Got_TC_PRINT		;? = shorthand for "PRINT"
sEnd DATA
sBegin CP
assumes CS,CP

;got a "." at start of token.  It may be a number, or a record separator
Got_TC_PERIOD:
	mov	al,[si]
	cmp	al,'0'
	jb	Got_TC_SPECIAL		;brif char after "." isn't 0-9
	cmp	al,'9'
	ja	Got_TC_SPECIAL		;brif char after "." isn't 0-9
;0-9 or .
GotLit:
	;Call ScanLit, trapping runtime overflow errors
	mov	WORD PTR [di.TOK_lit_errCode],0 ;zero errCode and flags
	.errnz	TOK_lit_flags - TOK_lit_errCode - 1
	PUSHI	ax,<CODEOFFSET ScanLit>
	call	CallRtTrap_CODE 	; al = 0 if no error, else ER_OV
					; in QBJ version ax may be error
					; for bad KANJI constant
	mov	[di.TOK_lit_errCode],al ;return error code
	jmp	UpdateSrcPtr		;save si in ps.PS_bdpSrc.BDP_pbCur and exit

;End-of-line Character
Got_TC_NEWLINE:
	mov	[di.TOK_class],CL_RESWORD
	mov	[di.TOK_rw_iRw],IRW_NewLine
	mov	[di.TOK_rw_rwf],0	;res word is not an operator/stmt/func
	mov	[di.TOK_rw_iOperator],UNDEFINED
	jmp	ExitFetchToken		;don't save new si in
					;ps.PS_bdpSrc.BDP_pbCur so next call to
					;FetchToken will get the same token
					;until a new line is read into ps

;Special Character
Got_TC_SPECIAL:
	mov	[di.TOK_class],CL_UNKNOWNCHAR
	mov	[di.TOK_unknownChar_unknownChar],dl
	cmp	dl,'_'
	jne	NotLineCont		;brif not underscore
	FLoadActive
	je	NotLineCont		;brif not loading a file
	dec	si			;don't include _ in logical line
	mov	bx,dataOFFSET ps.PS_bdpSrc ;pass pbd in bx
	sub	si,[bx.BD_pb]		;si = #bytes in buffer before _
	mov	cx,si			;pass cbAppend in cx
	call	GetLineBd		;append next physical line to current
					; buffer
SkipWhiteSpace_2:
	mov	[di.TOK_oSrc],si	;update current offset
	add	si,[ps.PS_bdpSrc.BDP_pb] ;si = ptr to next byte after _
	sub	ah,ah			;ax = al (SkipWhiteSpace_1 assumes ah=0)
	jmp	SkipWhiteSpace_1

NotLineCont:
	jmp	UpdateSrcPtr		;save si in ps.PS_bdpSrc.BDP_pbCur and exit

;While scanning a token, we found a FN followed by a space.
;In all versions of BASIC, FN xxx is treated the same as FNxxx.
;Squeeze source of ' 'out of pcode buffer
;because we need to call ONamOfPbCb with case sensitive FNxxx
;BdShiftLeft((bd *)&ps.bdpSrc, oSpace, 1)
;
FNspace:
	pop	di			;restore di points to token descriptor
	PUSHI	ax,<dataOFFSET ps.PS_bdpSrc>
	mov	ax,si
	dec	ax			;ax points to space
	sub	ax,[ps.PS_bdpSrc.BDP_pb] ;ax = offset to space
	push	ax			;pass oSpace
	PUSHI	ax,1			;shift 1 byte left
	call	BdShiftLeft		;causes no heap movement
	mov	si,[di.TOK_oSrc]	;si points to start of FNxxx
	jmp	SkipWhiteSpace_2	;re-scan token

;Convert '?' into "PRINT"
Got_TC_PRINT:
	push	di			;save ptr to result token descriptor
	lea	di,[namBuf]		;di -> temporary name buffer
	mov	ax,'RP' 		;namBuf = "PRINT"
	stosw
	mov	ax,'NI'
	stosw
	mov	al,'T'
	stosb
	mov	[lastChar],al		;anything but '$'
	inc	si			;compensate for dec si below
	mov	ax,5			;[ax] = #chars in "PRINT"
	pop	di			;di points to result token descriptor
	jmp	GotId	

;Upper and Lower Case Alpha
Got_TC_ALPHA:				
	mov	cx,CB_IDNAM_MAX+1	
	sub	ax,ax
	mov	[di.TOK_id_lexFlags],al
	mov	[endOfGO],ax		;assume we're not looking at 'GO'
	mov	[tokFlags],ax		;default flags = 0
	push	di			;save ptr to result token descriptor
	lea	di,[namBuf]		;di -> temporary name buffer
	mov	[di],ax			;init first two bytes of namBuf
					; to 0 so the "cmp [namBuf],'NF'"
					; below will always work.
	mov	al,dl			;al = source char
IdLoop0:
	mov	bx,CPOFFSET typChar	;bx->table to map char to char-class
GotInternational:			

;Loop to convert id to upper case and copy to temp buffer
; In the case of GO TO	or  GO SUB, we copy the entire reserved word
; into namBuf, squeezing out any white space after 'GO'.  The goal
; is to make people who use GO TO pay more than people who use GOTO.
; At this point,
;	al = UCASE(current source char)
;	si points to next source char
;	di points to destination of next char of id
;	bx points to typChar table
;	cx = max number of chars left in identifier
;	es = ds = DGROUP
;
IdLoop:
	and	al,0DFh 		;force to upper case
					;Note that it is OK to do this even
					;if char is numeric, since no res words
					;contain numerics, if it is numeric,
					;it will never match.
	stosb				;store UCASE(source char) in temp buf
	lodsb				;al = next source char
	dec	cx
	jcxz	IdTooLong 		;branch if name length exceeded
IdLoop1:
	or	al,al			;test for non-ascii char ( > 127)
	js	GotNonASCII		;branch if we got a non ASCII char

	mov	dl,al			;save current char in dl
	xlat	BYTE PTR cs:[bx]	;al = ax = character type
	cmp	al,TC_PERIOD
	xchg	ax,dx			;dl = char type, al=char
	jb	IdLoop			;brif got an alpha or numeric
	jne	EndOfId 		;brif didn't get a "."
	test	[psFlags],PSIF_fPeriodOk OR PSIF_fLexPeriodOk
	jne	IdLoop			;brif "." can be part of id token
	cmp	[nambuf],'NF'
	je	IdLoop			;brif FNxxx.xxx (def fn ids can have
					; periods in them)

GotNonASCII:
	mov	dl,TC_SPECIAL

;al = char which terminated id (may be '$','#' etc.)
;dl = type of last char (TC_xxx)
;si points beyond char which terminated string
;
EndOfId:
	mov	[lastChar],al		;save last char
	mov	[charType],dl		;save type of terminator
	mov	ax,CB_IDNAM_MAX+1	
	sub	ax,cx			;ax = # chars in id
	mov	dx,[nambuf]
	cmp	al,2
	jne	NotGO			;branch if id can't be 'GO'
	cmp	dx,'OG'			;in namBuf, least sig byte comes last,
					; so GO == 'OG'
	jne	NotGO			;branch if id isn't 'GO'
	cmp	[lastChar],' '
	jne	NotGO			;branch if not 'GO' followed by space
	mov	[endOfGO],si		;save ptr to end of 'GO'
;Skip white space, then try to find GO TO or GO SUB
SkipSpc:
	lodsb
	cmp	al,' '
	jne	IdLoop1
	jmp	SHORT SkipSpc

;Got an id which was too long - generate tricky error message.
;If we didn't call ParseErr before PErrMsg_AX, PErrMsg_AX would call NtEndStmt
;which would recursively call FetchToken, which would be bad news.
;
IdTooLong:
	pop	bx			;bx points to result token descriptor
					; for error column reporting
	push	bx			;re-save it on stack
	mov	ax,PSERR_fAlert + MSG_IdTooLong
	call	IdErr
	mov	cx,1			;only pass 40 byte name to ONamOfPbCb
	mov	al,[si]
	jmp	SHORT EndOfId		; it can't handle anything longer

;Got 'FN' as start of name
;di points beyond of name in nambuf, ax=length of name, dx = [nambuf]
FNid:
	cmp	al,2
	jne	NotFN2			;brif we got more than just FN
	cmp	[lastChar],' '		
	jne	IllegalFNid		;brif FN not followed by ' '
					; will get error like "Expected FNid"
	jmp	FNspace			;treat FN x  like  FNx

IllegalFNid:
	pop	di			;di points to result token descriptor
	mov	bx,di
	mov	ax,PSERR_fAlert + MSG_BadId
	call	IdErr
	jmp	SkipWhiteSpace_0	;re-scan token

;Got FNxxx, not just FN
NotFN2:
	or	[tokFlags],FVI_FNNAME	;set FVI_FNNAME bit in tok.id.flags
	mov	dl,[namBuf+2]		;dl = UCASE(1st char beyond FN)
	cmp	dl,'A'			
	jae	NotGO_Cont		;brif got FN<letter>xxx
	cmp	dl,('.'	AND 0DFh)	;test if char was a '.' before 
					; anding with '.'
	jne	IllegalFNid 		;brif got FN<digit>xxx
	mov	dl,26+'A'		;'.' maps to 26 (Z+1)
					; FN.xxx is always single precision
	jmp	short NotGO_Cont

;di points beyond of name in nambuf, ax=length of name, dx = [nambuf]
NotGO:
	cmp	dx,'NF'			;test for 'FN' (FN == 'NF' because
					; in namBuf, least sig byte comes last)
	je	FNid			;brif got 'FN'

;dl=1st letter of name (or 3rd letter of FNname)
;ax=length of name, [lastChar]=terminator
NotGO_Cont:
	pop	di			;di points to result token descriptor
	mov	[firstChar],dl

;di points to token descriptor, ax=length of name, [lastChar]=terminator
GotId:
	mov	[cbId],ax		;save byte count of id
	cmp	al,1
	je	NotResWordId		;1 letter id can't be res word
	cmp	[lastChar],'.'
	je	NotResWordId		;res word can't be followed by period

	push	di			;pass ptr to token descriptor to FindRw
	lea	bx,namBuf
	push	bx			;pass ptr to UCASE(1st char)
	push	ax			;pass char count
	sub	ax,ax			;fStr = FALSE
	cmp	[lastChar],'$'
	jne	CallFindRw		;brif id terminator <> '$'
	inc	ax			;fStr = TRUE
CallFindRw:
	push	ax			;pass fStr
	call	FindRw
	jnc	NotResWordId

	;Got a valid reserved word
	cmp	[lastChar],'$'
	je	J1_UpdateSrcPtr		;brif res word not terminated by '$'
	dec	si			;next FetchChar will refetch terminator
J1_UpdateSrcPtr:
	jmp	short UpdateSrcPtr	;save si in ps.PS_bdpSrc.BDP_pbCur and exit

;[tokFlags] = tok.id.flags
NotResWordId:
	mov	al,[firstChar]
	sub	al,'A'			;map A..Z to 0..25, period to 26
	mov	[di.TOK_id_charFirst],al;so parser can set default type
					; Also used for DEFxxx A-X
	mov	ax,[tokFlags]
	mov	[di.TOK_id_vmFlags],ax
	mov	cx,[endOfGO]
	jcxz	NotGoId 		;brif we didn't parse 'go id'
	mov	si,cx			;si -> 1st byte after 'go '
	mov	[cbId],2
	jmp	SHORT GotImplicitId

NotGoId:
	;following code assumes IRW_EtInteger...IRW_<last expl. type char>
	; are contiguous
	mov	al,[charType]
	sub	al,TC_RW + IRW_EtInteger
	cmp	al,CBASETYPEMAX - 1	;cmp al to number of explicit types
	jb	GotExplicitType
GotImplicitId:
	dec	si			;next FetchChar will refetch terminator
	mov	al,ET_IMP		;can't be $,%,&,!, or #
	jmp	SHORT SaveType

GotExplicitType:
	add	al,ET_I2
SaveType:
	sub	ah,ah
	mov	[di.TOK_id_oTyp],ax
	mov	ax,[ps.PS_bdpSrc.BDP_pb]
	mov	[ps.PS_bdpSrc.BDP_pbCur],si ;NOTE: ONamOfPbCb can cause heap to move
					; so save current text pointer
	add	ax,[di.TOK_oSrc]	;ax points to 1st byte of id
					;  (parm to ONamOfPbCb)
	mov	cx,[cbId]		;#bytes in id - also parm to ONamOfPbCb
	test	[ps.PS_flags],PSF_fParseExp
	jne	DontSaveCase		;brif called to parse a Watch Expression
	test	[psFlags],PSIF_NoCaseChg
	jne	DontSaveCase
	cmp	[grs.GRS_fDirect],FALSE
	je	SaveCase		;brif not parsing direct mode stmt
DontSaveCase:
	or	ch,80h			;highbit says don't change case
					; of existing namtbl id, so user
					; can type alpha in the command window
					; and it doesn't change Alpha to
					; alpha in his list window.
SaveCase:
	call	ONamOfPbCb		;nammgr returns ax = oNam, dl = flags
	je	ONamErr			;brif ONamOfPbCb failed
	mov	[di.TOK_id_oNam],ax	;save nammgr's handle for the name
	cmp	[lastChar],'.'
	je	IdPeriod
NotIdPeriod:
	mov	[di.TOK_class],CL_ID
	mov	al,[lastChar]
	mov	[di.TOK_id_termChar],al
	jmp	SHORT ExitFetchToken

;save si in ps.PS_bdpSrc.BDP_pbCur and exit
UpdateSrcPtr:
	mov	[ps.PS_bdpSrc.BDP_pbCur],si
ExitFetchToken:
cEnd	FetchToken

ONamErr:	       			
	call	ParseErrOm		;tell ParseLine to return out-of-mem err
J1_Got_TC_NEWLINE:
	jmp	Got_TC_NEWLINE		;return new-line - ends parsing of line

;ax = oNam of id which was terminated with "."
IdPeriod:
	test	[psFlags],PSIF_fNoPeriod ;don't stop for periods
	jne	NotIdPeriod		;brif scanning id which cannot have .
	test	dl,NM_fAS		;dl still set from ONamOfPbCb call
	jne	NotIdPeriod		;brif id has been seen in "x AS" clause
					; in which case x.y is 3 tokens

	;set PSIF_fLexPeriodOk so IdLoop won't stop for periods
	;set PSIF_fLineHasPeriodId so ParseLine knows line has 'a.b' id in line
	; so it will emit an opNoType at end of line - helps txtmgr
	
	or	[psFlags],PSIF_fLineHasPeriodId + PSIF_fLexPeriodOk
	or	[di.TOK_id_lexFlags],FLX_hasPeriod
	mov	si,[ps.PS_bdpSrc.BDP_pbCur] ;si points to '.' after id
	mov	cx,CB_IDNAM_MAX+2	
	mov	ax,[cbId]
	sub	cx,ax			;cx = num bytes left
	push	di			;save token ptr back on stack
					; so we can jump back into IdLoop
	lea	di,nambuf
	add	di,ax			;di points to destination for next byte
	mov	al,'.'
	push	ds
	pop	es			;es = DGROUP
	jmp	IdLoop0



;=====================================================================
;	   T O K E N   S C A N N I N G	 F U N C T I O N S
;
;	The idea is to maintain a circular queue of tokens for "Look Ahead".
;	There are 4 offsets into this circular queue of particular interest:
;	   pTokLast points to the most recent token scanned by 'FetchToken'.
;	   pTokPeek points to the last token returned by 'Peek1Tok' or
;	      'PeekNextTok'.
;	   pTokScan points to the last token returned by 'ScanTok'.
;		It is the chronologically oldest token we care
;	        about, or the opposite end of the queue from pTokLast.
;	   pTokLastConsumed points to the last token consumed by 'ScanTok'.
;		This token was the previous value of pTokScan. It is useful
;		in some special cases for checking for syntax errors and
;		generating the correct column offset.
;
;======================================================================

;*********************************************************************
; VOID NEAR ScanTok()
;
; Purpose:
;	Advance 'pTokScan' to point to the next non white-space token in
;	the circular token queue.  Note that if PeekNextTok has already
;	fetched the token we're interested in, this function does
;	very little work (i.e. doesn't need to call FetchToken).
; Exit:
;	bx points to current token (same as pTokScan)
;
;*********************************************************************
PUBLIC	ScanTok
ScanTok PROC NEAR
	inc	[cTokScan]		;bump token count so NtParse
					; knows to return PR_GoodSyntax
					; instead of PR_NotFound
	mov	ax,[pStateCur]
	mov	[pStateLastScan],ax	;used for error reporting
	mov	ax,[pTokScan]		;ax points to current 'scan' token
	mov	[pTokLastConsumed],ax	;update pointer to last consumed tok
	mov	bx,ax	 		;bx points to current 'scan' token
	add	ax,CB_TOK		;advance to next token
	cmp	ax,dataOFFSET tLookAhead + (LOOK_AHEAD * CB_TOK)
	jb	ScanNoWrap		;brif if not wrapped around
	mov	ax,dataOFFSET tLookAhead;ax points to 1st token descriptor
ScanNoWrap:
	mov	[pTokScan],ax		;save ptr to new 'scan' token

;bx points to previous token of interest
;ax points to next token of interest
ScanFetch:
	cmp	[pTokLast],bx
	jne	ScanGotOne		;brif token we want has already been
					; fetched by PeekNextToken
					;else circular queue is empty,
					; fetch a new entry
	mov	[pTokLast],ax		;advance pTokLast as well
	push	di
	mov	di,ax			;di points to new token
	call	FetchToken		;fill in token structure di
	mov	bx,di			;return token ptr in bx
	pop	di
ScanGotOne:
	ret
ScanTok ENDP

;Entry:
;	ax = error code
;	bx = token descriptor
;
IdErr	PROC NEAR
	cmp	[ps.PS_errCode],0
	jne	Not1stErr		;brif not 1st error for this line
	call	ParseErr
Not1stErr:
	ret
IdErr	ENDP

;*********************************************************************
; VOID NEAR Peek1Tok()
;
; Purpose:
;	Set 'pTokPeek' pointing to the 1st non white-space token after
;	'pTokScan' for look-ahead.
; Exit:
;	bx points to current token (same as pTokPeek)
;
;*********************************************************************
;*********************************************************************
; VOID NEAR PeekNextTok()
;
; Purpose:
;	Advance 'pTokPeek' to point to the next non white-space token in
;	the circular token queue for look-ahead.
; Exit:
;	bx points to current token (same as pTokPeek)
;
;*********************************************************************
PUBLIC	Peek1Tok
Peek1Tok PROC NEAR
	mov	ax,[pTokScan]		;pTokPeek = pTokScan
	mov	[pTokPeek],ax
	;fall into PeekNextTok
Peek1Tok ENDP
PUBLIC	PeekNextTok
PeekNextTok PROC NEAR
	mov	ax,[pTokPeek]		;ax points to current 'peek' token
	mov	bx,ax			;bx points to current 'peek' token
	add	ax,CB_TOK		;advance to next token
	cmp	ax,dataOFFSET tLookAhead + (LOOK_AHEAD * CB_TOK)
	jb	PeekNoWrap		;brif if not wrapped around
	mov	ax,dataOFFSET tLookAhead;ax points to 1st token descriptor
PeekNoWrap:
	mov	[pTokPeek],ax		;save ptr to new 'peek' token
	jmp	SHORT ScanFetch 	;share code with ScanToken
PeekNextTok	ENDP

;**********************************************************************
; boolean NEAR TestScan_AX
; Purpose:
;	See if current token is a particular reserved word
; Entry:
;	pTokScan points to current token
;	ax = IRW_xxx for reserved word to be tested
; Exit:
;	bx = [pTokScan]
;	psw.z set if current token is desired reserved word
;
;*******************************************************************
PUBLIC	TestScan_AX
TestScan_AX PROC NEAR
	mov	bx,[pTokScan]		;bx points to current token
TestPeek_AX1:
	cmp	[bx.TOK_class],CL_resWord
	jne	TestScan_AXExit 	;if not res word, return NE
	cmp	ax,[bx.TOK_rw_iRw]	;if not expected one, return NE
TestScan_AXExit:
	ret
TestScan_AX ENDP

;**********************************************************************
; boolean NEAR TryScan_AX
; Purpose:
;	See if current token is a particular reserved word and if it
;	is then consume it.
; Entry:
;	pTokScan points to current token
;	ax = IRW_xxx for reserved word to be tested
; Exit:
;	if not matched then psw.z is cleared and bx = [pTokScan]  
;	otherwise psw.z is set
;
;*******************************************************************
PUBLIC	TryScan_AX
TryScan_AX PROC NEAR
	call	TestScan_Ax		;test for the reserved word
	jne	TrySExit		;brif not matched
	call	ScanTok			;consume it
	xor	ax,ax			;set psw.z
TrySExit:
	ret
TryScan_AX ENDP

;**********************************************************************
; boolean NEAR TestPeek_AX
; Purpose:
;	See if current PEEK token is a particular reserved word
; Entry:
;	pTokPeek points to next token
;	ax = IRW_xxx for reserved word to be tested
; Exit:
;	bx = [pTokPeek]
;	psw.z set if token is desired reserved word
;
;******************************************************************
PUBLIC	TestPeek_AX
TestPeek_AX PROC NEAR
	mov	bx,[pTokPeek]		;bx points to next token
	jmp	SHORT TestPeek_AX1
TestPeek_AX ENDP

;**********************************************************************
; boolean NEAR ConsumeRw_AX(ax:iRw)
; Purpose:
;	Make sure that the current token is the reserved word 'iRw'.
; Entry:
;	ax is offset into reserved word table for res word to be consumed
; Exit:
;	If reserved word was not found
;	   a complete error message is generated
;	   al = PR_BadSyntax
;	   carry is set
;	Else
;	   token is consumed
;	   bx = [pTokScan]
;	   carry is clear
;
;*******************************************************************
PUBLIC	ConsumeRw_AX
ConsumeRw_AX PROC NEAR
	mov	bx,[pTokScan]		;bx points to current token
	cmp	[bx.TOK_class],CL_resWord
	jne	ConRwErr		;branch if token isn't reserved word
	cmp	[bx.TOK_rw_iRw],ax	;compare with expected res word
	jne	ConRwErr		;branch if didn't get expected one
	call	ScanTok 		;consume token
	clc				;return SUCCESS
	ret

ConRwErr:
	call	PErrExpRw_AX		;Error "Expected <reserved word>"
					; al = PR_BadSyntax
	stc				;return error flag
	ret
ConsumeRw_AX ENDP

;*********************************************************************
; LexReset
; Purpose:
;	Reset the lexical analyzer's source pointer to the 1st char
;	of pTokScan's token.  This flushes the token peek-ahead queue.
;
;*********************************************************************
PUBLIC	LexReset
LexReset PROC NEAR
	mov	bx,[pTokScan]
	mov	[pTokLast],bx
	mov	ax,[bx.TOK_oSrc]
	add	ax,[ps.PS_bdpSrc.BDP_pb]
	mov	[ps.PS_bdpSrc.BDP_pbCur],ax ;setup to rescan same token
	jmp	ScanTok 		;bx points to current token
					;return to caller
LexReset ENDP

sEnd	CP


sBegin	CODE
assumes CS,CODE

;*********************************************************************
; void ScanLit
;	This function is based on code lifted from BASCOM
; Purpose:
;	Scan a number
; Entry:
;	si points beyond 1st source char in number
;	di points to token descriptor to be filled in on exit
; Exit:
;	si points to char which terminates number
;	token pointed to by di is filled in
;	If an error occurred, a runtime error is generated with
;	   a standard error msg (ER_xxx or MSG_xxx)
;
;*********************************************************************
l_s16inv=	80h		; 16 bit signed value is invalid
l_ind=		80h		; indefinite
l_inv=		40h		; invalid (no digits or syntax error)
l_s32inv=	20h		; 32 bit signed value is invalid
l_u32inv=	10h		; 32 bit unsigned value is invalid
l_long= 	08h		; l_Dexp or more than 7 digits
l_Dexp= 	04h		; explicit 'D' or 'd' seen
l_Rexp= 	02h		; explicit 'E' or 'e' seen
l_inf=		01h		; DP overflow

dbPub	ScanLit
cProc	ScanLit,<FAR>			
cBegin	ScanLit				
	dec	si			;si points to 1st letter of number
	mov	[di.TOK_CLASS],CL_LIT	;token class = Literal
	push	di			;save pointer to token descriptor
	add	di,TOK_lit_value_R8	;di points to number's result field
	cmp	byte ptr [si],'&'	; check for special radix constant
	jne	DNSgetnum		;   no - normal number

;	read special radix numbers as unsigned 16 or 32 bit integers

	inc	si			; skip past 1st &
	lodsb				; get next character (H or O)
	or	al,20h			; map letter to lower case
	mov	bx,(LIT_H4 * 100h) + 10h; assume hex
	cmp	al,'h'
	je	DNSerad 		;   yes

	mov	bx,(LIT_O4 * 100h) + 8	; assume octal
	cmp	al,'o'
	je	DNSerad 		;   yes


	dec	si			; must be octal - move back pointer
DNSerad:
	push	bx			; save bh = LIT_xxx
	xor	bh,bh			; bx = bl = base
	call	DoInput			; [bx:ax] = result if integer/long
	pop	dx			; [dh] = LIT_xxx
	pop	di			; di points to token descriptor

	test	cl,l_u32inv		; check if overflow error
	jnz	DNSovr
	test	cl,l_inv		; check syntax err
	jnz	DNSsyn
	cmp	byte ptr [si],'&'	; long?
	je	EatExpLng		; brif explicitly LONG
	or	bx,bx			; if BX<>0, can't be 16-bit integer
	je	ItsShort		
	cmp	byte ptr [si],'%'	; short?
	jne	DNSradlng		; brif not explicitly SHORT
;Overflow error
DNSovr:
	mov	ax,ER_OV		; input number overflow
	jmp	SHORT DNSerr

EatExpLng:
	inc	si			; consume '&'
	jmp	SHORT DNSradlng

ItsShort:
	sub	dh,3			; map LIT_x4 to LIT_x2
.errnz	LIT_H4 - LIT_H2 - 3		; in case values of constants change
.errnz	LIT_O4 - LIT_O2 - 3		; in case values of constants change
	cmp	byte ptr [si],'%'	; short?
	jne	DNSradint		; brif not explicit SHORT
	inc	si			; consume '&'
;dh = literal type (LIT_xxx)
DNSradint:
	mov	dl,ET_I2		; integer
DNSradintlng:
	mov	[di.TOK_lit_value_I2],ax	; save low part of integer
	xchg	ax,dx			; al = value type, ah = literal type
	jmp	DNSchkdbl		; save type and exit

;dh = literal type (LIT_xxx)
DNSradlng:
	mov	dl,ET_I4		; long integer
	mov	[di.TOK_lit_value_I2+2],bx	; save high part of long integer
	jmp	SHORT DNSradintlng

DNStyp:
	inc	si			; eat invalid type character
DNSsyn:
	mov	ax,MSG_IllegalNumber	; input number syntax error
DNSerr:
	call	RtErrorCODE		; error (ax)

DNSgetnum:
	call	TryI2		;if its an I2, don't call $i8_input (pig slow)
	jc	NotAnI2		;brif not a valid I2
	pop	di		;restore token pointer
	mov	dh,LIT_I2
	jmp	SHORT DNSradint	;brif AX = valid I2

NotAnI2:
	xor	bx,bx		; (BX) = 0 (default radix)
	call	DoInput		; [bx:ax] = result if integer/long
				; [di] = R8 result
	pop	di		; di points to token descriptor

	test	cl,l_inv+l_ind	; check if syntax error
	jnz	DNSsyn
	test	cl,l_inf	; check if d.p. overflow
	jnz	DNSovr

;	check if number could be 16-bit integer

	test	cl,l_s32inv	; check if valid 32-bit integer
	jnz	DNSinv16	;   no - not valid 16-bit either
	cwd
	cmp	bx,dx
	je	DNSchkexp

DNSinv16:
	or	cl,l_s16inv	; mark as invalid 16-bit integer

;	check for explicit E or D

DNSchkexp:
	test	cl,l_Rexp+l_Dexp ; check if explicit E or D
	jz	DNSchktyp	;  no - test for explicit type char
	or	[di.TOK_lit_flags],FLIT_exp
	jmp	SHORT DNSsav	;  explicit types not allowed

;	check if trailing type character

DNSchktyp:
	cmp	byte ptr [si],'#' ; double precision?
	je	DNSdbl		  ;   yes
	cmp	byte ptr [si],'!' ; single precision?
	je	DNSsng		  ;   yes
	cmp	byte ptr [si],'%' ; integer?
	je	DNSint		  ;   yes
	cmp	byte ptr [si],'&' ; long?
	jne	DNSsav		  ;   no

DNSlng:
	test	cl,l_s32inv	; check if valid 32-bit integer
	jnz	DNStyp		;   no - type character error
	or	cl,l_s16inv	; force it long
	jmp	SHORT DNSsavi

DNSint:
	test	cl,l_s16inv	; check if valid 16-bit integer
	jnz	DNStyp		;   no - type character error
	jmp	SHORT DNSsavi

DNSdbl:
	or	cl,l_long+l_s32inv+l_s16inv ; force it double
	jmp	SHORT DNSsavi


DNSsng:
	and	cl,not l_long	; force it single
	or	cl,l_s32inv+l_s16inv

DNSsavi:
	inc	si		; skip over explicit type character

;	have number - now save it

DNSsav:
	mov	dh,LIT_I2
	test	cl,l_s16inv	; check if 16-bit integer
	jz	jDNSradint	;   yes

DNSchklng:
	mov	dh,LIT_I4
	test	cl,l_s32inv	; check if 32-bit integer
	jz	jDNSradlng	;   yes

DNSchksng:
	mov	ax,(LIT_R8 * 100h) + ET_R8	;assume double precision
	test	cl,l_long	; check if double
	jnz	DNSchkdbl	;   yes

	push	di		;save ptr to token descriptor

	add	di,TOK_lit_value_R8	;di -> number's result field
	fld	qword ptr [di]	;pass R8 value
	fstp	dword ptr [di]	;pop R4 value
	pop	di		;di points to token descriptor
	mov	ax,(LIT_R4 * 100h) + ET_R4	;single precision
	fwait			

; al = value type, ah = literal type
DNSchkdbl:
	mov	[di.TOK_lit_type],al	; value type (ET_xxx)
	mov	[di.TOK_lit_litType],ah ; constant type (LIT_xxx)
DNSxit:
cEnd	ScanLit				


jDNSradint:
	jmp	DNSradint

jDNSradlng:
	jmp	DNSradlng

DoInput PROC NEAR
	mov	cx,[ps.PS_bdpSrc.BDP_pb] ;cx = start of bdpSrc buffer
	add	cx,[ps.PS_bdpSrc.BDP_cbLogical] ;cx = end of bdpSrc buffer+1
	sub	cx,si			;cx = remaining number of bytes in
					;     buffer past si
	xor	ax,ax			; no scale factor
	cwd				; (DX) = 0 (FORTRAN garbage)
	mov	[$i8_inpbas],1		; use BASIC sematics for E and D
	push	bp
	extrn	$i8_input:near
	call	$i8_input		; [bx:ax] = long integer
	pop	bp
	mov	[$i8_inpbas],0		; clear BASIC input flag
	ret
DoInput ENDP


;*********************************************************************
;TryI2
;Purpose:
;	Try to parse an I2 so we don't have to call $i8_input (its slow)
;Entry:
;	si points to 1st digit of number
;Exit:
;	if valid I2 was consumed,
;	   carry is clear
;	   si points beyond last digit of number
;	else
;	   carry is set
;	   si points to 1st digit of number
;
;*********************************************************************
dbpub	TryI2
cProc	TryI2,<NEAR>
cBegin
	push	si
	sub	dx,dx			;acc=0
NextDigit:
	lodsb				;al = next digit
	sub	al,'0'			;map '0'..'9' to 0..9
	jc	EndOfDigits		;brif < '0'
	cmp	al,9
	ja	EndOfDigits
	cmp	dx,3275d
	ja	NotI2			;brif result could be > 32759
	add	dx,dx			;acc = acc*2
	mov	cx,dx			;cx  = acc*2
	add	dx,dx			;acc = acc*4
	add	dx,dx			;acc = acc*8
	add	dx,cx			;acc = acc*10
	cbw
	add	dx,ax			;acc = acc*10 + new digit
	jmp	SHORT NextDigit

;al = terminating digit - '0'
EndOfDigits:
	mov	bx,ax			;bl = terminating digit - '.'
	push	di
	push	ds
	pop	es			;es = DGROUP
	lea	di,tbFloat		;si points to table of letters that
					; would mean the number is not an I2
	mov	cx,CB_tbFloat
	repne	scasb			;search for found opcode in table
	pop	di
	je	NotI2			;brif terminated with '.'
	pop	ax			;discard entry si
	dec	si			;si points to terminating digit
	xchg	ax,dx			;return result in dx
	clc
	jmp	SHORT TryI2Exit

NotI2:
	pop	si			;restore ptr to start of digits
	stc
TryI2Exit:
cEnd

sEnd	CODE

end
