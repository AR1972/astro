; File: PSINT.INC
; NOTE: For comments, see psint.h

PSINT_INC = ON			;remember that this file has been included

;**======================================================================**
;**==           Internal Interface to Parser Component                 ==**
;**======================================================================**

;**==================== token descriptor ==================================
;** dtk - (token descriptor) describes tokens returned by the lexical analyzer
;**=======================================================================**
TOK	STRUC
TOK_class	DW	0
TOK_oSrc	DW	0
TOK_dsc		DW	0	;class specific descriptor union
TOK_filler	DW	5 DUP (0) ;fill out to max size of remaining tokens
TOK	ENDS

TOK_rw	STRUC			;12 bytes long
TOK_rw_union	DW	0,0	;TOK_class and TOK_oSrc
TOK_rw_iRw	DW	0
TOK_rw_rwf	DW	0
TOK_rw_iOperator DW	0
TOK_rw_pArgs	DW	0
TOK_rw	ENDS
.erre SIZE TOK_rw LE SIZE TOK


TOK_id	STRUC			;12 bytes long
TOK_id_union	DW	0,0	;TOK_class and TOK_oSrc
TOK_id_oNam	DW	0
TOK_id_oTyp	DW	0
TOK_id_vmFlags	DW	0	;variable mgr flags
	;can have FVIMPLICIT or FVFNNAME bits set
TOK_id_charFirst DB	0
TOK_id_termChar	DB	0	;char which terminated id (not part of id)
TOK_id_lexFlags DB	0	;lexical analyzer flags (see FLX_ below)
TOK_id_filler   DB      0	;pad to even byte length
TOK_id	ENDS
.erre SIZE TOK_id LE SIZE TOK

;TOK_id_lexFlags values:
FLX_hasPeriod	EQU 01h		;TRUE if current token has a '.' in its name
FLX_asSymConst	EQU 02h		;TRUE for x in 'x AS STRING * <symbolic const>

TOK_unknownChar	STRUC		;5 bytes long
TOK_unknownChar_union DW 0,0	;TOK_class and TOK_oSrc
TOK_unknownChar_unknownChar DB	0
TOK_unknownChar	ENDS
.erre SIZE TOK_unknownChar LE SIZE TOK

TOK_lit STRUC			;16 bytes long
TOK_lit_union	DW	0,0	;TOK_class and TOK_oSrc
TOK_lit_errCode DB	0	;0 if no error, else ER_xxx or MSG_xxx
TOK_lit_flags	DB	0	;FLIT_xxx below
TOK_lit_type	DB	0
TOK_lit_litType	DB	0
TOK_lit_value_I2 DW	0
TOK_lit_value_filler DW	0,0,0	;large enough for an R8
TOK_lit ENDS
.erre SIZE TOK_lit LE SIZE TOK

FLIT_exp	EQU	1	;TRUE if E+nnn or D+nnn exponent seen

TOK_lit_value_I4 = TOK_lit_value_I2
TOK_lit_value_R4 = TOK_lit_value_I2
TOK_lit_value_R8 = TOK_lit_value_I2
TOK_lit_value_cbStr = TOK_lit_value_I2

CB_TOK		EQU	SIZE TOK	;max size of token structure
.erre CB_TOK EQ 16 ;if this changes then the declarations in psint.h muse
		  ; be changed so that the size of tLookAhead will be correct

LOOK_AHEAD	EQU	9		;size of token lookahead queue
					;this value must be consistent with
					;same value in psint.h

;Enumerations for TOK_class
;
CL_RESWORD = 0
CL_ID = 2
CL_LIT = 3
CL_UNKNOWNCHAR = 4


; PARSE_RESULT is the type returned by NtParse() and functions which
; recognize non-terminals.  The values are PR_NotFound, PR_GoodSyntax,
; and PR_BadSyntax.
;
PR_BadSyntax	EQU	-1
PR_NotFound	EQU	0
PR_GoodSyntax	EQU	1

;Indecies into table of operators used by NtExp()
IOP_mark	EQU	0
IOP_RParen	EQU	1
IOP_Imp		EQU	2
IOP_Eqv		EQU	3
IOP_Xor		EQU	4
IOP_Or		EQU	5
IOP_And		EQU	6
IOP_Not		EQU	7
IOP_EQ		EQU	8
IOP_LT		EQU	9
IOP_GT		EQU	10
IOP_LE		EQU	11
IOP_GE		EQU	12
IOP_NE		EQU	13
IOP_Add		EQU	14
IOP_Minus	EQU	15
IOP_Mod		EQU	16
IOP_Idiv	EQU	17
IOP_Mult	EQU	18
IOP_Div		EQU	19
IOP_Plus	EQU	20
IOP_UMinus	EQU	21
IOP_Pwr		EQU	22
IOP_LParen	EQU	23
NUM_OPERATORS	EQU	24

;These flags modify the global flag byte 'psFlags' (parser internal flags)
;
PSIF_fBindVars		EQU 01h
   ;set if parser is to call variable mgr to bind variable references
PSIF_fNoPeriod		EQU 02h
   ;set if lexical analyzer is NOT to allow "." in identifiers.
   ;This is true when parsing record element identifiers.
PSIF_fPeriodOk		EQU 04h
   ;set if lexical analyzer is to allow "." in identifiers.
   ;This is true when parsing identifiers like labels and /common/ id's.
   ;If not set, the lexical analyzer treats period as a record separator,
   ;unless the lexical analyzer sets PSF_fLexPeriodOk while scanning the id.
PSIF_fLexPeriodOk	EQU 08h
   ;Flag which is local to lexical analyzer.  Set if id being scanned begins
   ;with "xxx." and xxx has not been seen in an AS clause.  Causes periods
   ;to be ok in such an id.  If xxx had been seen in an AS clause, xxx.yyy
   ;would return 3 tokens:  xxx, ., yyy
   ;This flag is only set if neither PSF_fNoPeriod or PSF_fPeriodOk are set
PSIF_fLineHasPeriodId	EQU 10h
   ;If flag is set, this line contains an identifier other than a record
   ;element with a period in it's name (like A.B).  Causes ParseLine to
   ;emit an opNoTyp opcode at the end of the line, to aid the text manager's
   ;PreScanAsChg() function.
PSIF_fNot1stStmt	EQU 20h
   ;True after we've successfully parsed 1st statement on line
PSIF_NoCaseChg		EQU 40h
   ;True if next token to be scanned should not change case of ids in namtbl
   ;Set for DEFINT..DEFSTR

;Parameters passed to ParseUndoLog():
	PUNDO_oNamAs		equ 1
	PUNDO_oPrsRef		equ 2

;Structure used to pass info from parser terminal recognizers like
;NtIdSubDecl, NtIdFn, etc. to the code generator CgDeclare():
;
PDCL_st STRUC
PDCL_oNam	dw 0	;oNam of procedure being defined/declared
PDCL_oPrs	dw 0	;oPrs of procedure being defined/declared
PDCL_cParms	dw 0	;number of arguments
PDCL_oTyp	db 0	;type returned by func
			;MUST PRECEED PDCL_fDeclare
PDCL_procType	db 0	;PT_SUB, PT_FUNCTION, PT_DEFFN
PDCL_fDeclare	db 0	;non-zero if DECLARE, not SUB/FUNCTION/DEF FN stmt
			;MUST FOLLOW PDCL_procType
PDCL_st ENDS


;------------------------------------------------
;  External DATA accessed by the parser component
;------------------------------------------------


sBegin DATA
	EXTRN	pTokScan:WORD
	EXTRN	pTokPeek:WORD
	EXTRN	pTokLast:WORD
	EXTRN	pTokLastConsumed:WORD
	EXTRN	cIdArgs:WORD
	EXTRN	tpMsgStd:WORD
	EXTRN	cTokScan:BYTE
	EXTRN	tLookAhead:BYTE

ife	PRSID_ASM
	EXTRN	oNamConstPs:WORD ;non-zero if we're parsing a CONST expression
	EXTRN	pdcl:BYTE	 ;PDCL_ST struct - passes info to CgDeclare()
endif	;PRSID_ASM

ife PRSNT_ASM
	EXTRN	pStateLastScan:WORD
	EXTRN	pStateCur:WORD
	EXTRN	pCurStkMark:WORD
	EXTRN	minStkMark:WORD
	EXTRN	maxStkMark:WORD
	EXTRN	pStateLastGood:WORD
endif
	MIN_STK_MARK	EQU	dataOFFSET minStkMark
	MAX_STK_MARK	EQU	dataOFFSET maxStkMark

ife PRSRWT_ASM
	EXTRN	mpIRWtoIOP:BYTE
	EXTRN	mpIRWtoChar:BYTE
endif

ife PRSEXP_ASM
	EXTRN	stkExpInit:BYTE
	EXTRN	pExpTos:WORD
endif

ife PRSMAIN_ASM
	EXTRN	psFlags:BYTE	;see PSIF_xxx above
	EXTRN	stkChkParse:WORD
endif

ife PRSSTATE_ASM
	EXTRN	tState:BYTE
	EXTRN	tIntNtDisp:WORD
	EXTRN	tExtNtHelp:WORD
	EXTRN	tExtNtDisp:WORD
endif

sEnd	DATA


;------------------------------------------------
;  External CODE accessed by the parser component
;------------------------------------------------

sBegin	CP
ife	PRSID_ASM
	EXTRN	NtIdAryElem:near
	EXTRN	NtImpliedLetOrCall:near
	EXTRN	NtConsumeExp:near
	EXTRN	SubRef:near
	EXTRN	PErrExpExpr:near
	EXTRN	BindVar:near
	EXTRN	IdTok:near
	EXTRN	IdTokFn:near
	EXTRN	IdTokPeriodImp:near
	EXTRN	IdTokNoPeriod:near
	EXTRN	IdTokNoPeriodImp:near
	EXTRN	CopyTokScanBx:near
endif	;PRSID_ASM

ife	PRSLEX_ASM
	EXTRN	ScanTok:near
	EXTRN	Peek1Tok:near
	EXTRN	PeekNextTok:near
	EXTRN	TestScan_AX:near
	EXTRN	TestPeek_AX:near
	EXTRN	TryScan_AX:near
	EXTRN	ConsumeRw_AX:near
	EXTRN	FindRw:near
	EXTRN	LexReset:near
endif	;PRSLEX_ASM

ife	PRSMAIN_ASM
	EXTRN	ParseUndoLog:near
	EXTRN	RudeIfErr:near
endif	;PRSMAIN_ASM

ife	PRSUTIL_ASM
	EXTRN	EmitSrcCompress:near
	EXTRN	EmitSrc:near
	EXTRN	Emit16_0:near
	EXTRN	PErrState:near
	EXTRN	PErrExpMsg_AX:near
	EXTRN	PErrMsg_AX:near
	EXTRN	PErrExpRw_AX:near
	EXTRN	PErrPrevTok_Ax:near
	EXTRN	PErrVarMgr:near
	EXTRN	ParseErr:near
	EXTRN	ParseErr0:near
	EXTRN	ParseErrOm:near
	EXTRN	ParseErrTokScan:near
	EXTRN	PErrExpId:near
	EXTRN	PErrExpIdImp:near
endif	;PRSUTIL_ASM

ife	PRSNT1_ASM
	EXTRN	NtEmitRem:near
endif	;PRSNT1_ASM

ife	PRSEXP_ASM
	EXTRN	RelOp:near
	EXTRN	NtExp:near
	EXTRN	NtIntrinsic:near
	EXTRN	NtLit:near
	EXTRN	NtLitI2:near
endif	;PRSEXP_ASM

ife	PRSCTL_ASM
	;These symbols are defined inside prsctl.asm
	EXTRN	TestLn:near
	EXTRN	NtLn:near
	EXTRN	NtLabLn:near
	EXTRN	NtIfStmt:near
	EXTRN	NtCaseRelation:near
endif	;PRSCTL_ASM

ife	PRSSTMT_ASM
	EXTRN	NtEndStatement:near
	EXTRN	NtEndLine:near
	EXTRN	NtStatementList:near
	EXTRN	NtStatementList0:near
endif	;PRSSTMT_ASM

ife	PRSNT_ASM
	;These symbols are defined inside prsnt.asm
	EXTRN	NtParse:near
	EXTRN	NtCommaNoEos:near
endif	;PRSNT_ASM

DbChkPsStk MACRO
	ENDM
sEnd	CP

