; File: LSINT.INC

LSINT_INC = ON			;remember that this file has been included

;**======================================================================**
;**==           Internal Interface to Lister Component                 ==**
;**======================================================================**

;LsNode is a node in a binary tree used to describe the line being listed.
LsNode	STRUC
	LN_sib		DW 0	;offset into bdNodes to sibling node
				; (NULL if none)
	LN_type		DB 0	;identifies what type of text is associated
				; with this node (LNT_xxx defined below)
	LN_val		DB 0	;variable length content which depends
				; on the value of field LN_type
LsNode	ENDS

;Node types:
LNT_CHAR_TOK EQU 0	;value is 1 or 2 ASCII characters associated with
			; this node.
LNT_STR EQU	2	;value is a 16 bit offset followed by 16 bit count
			; which identifies where the string is in
			; the text table.
LNT_ENSTR EQU	4	;value is a 16 bit offset followed by 16 bit count
			; which identifies where the string is in the
			; text table.  The string may have runs of characters
			; compressed.
LNT_CSSTR EQU	6	;value is a 16 bit offset into LIST segment to a
			; str255 struct for a string constant
LNT_LITSTR EQU  8	;same as a LNT_STR node except that the list routine
			; generates two double quotes for each occurance
;The following nodes can 'begin' a logical token (for _CrLf line splitting)
LNT_ONAM EQU	10	;value is a 16 bit name table offset for identifier
			; associated with this node.
LNT_LIST EQU	12	;this node has no atomic ASCII source directly
			; associated with it, i.e. it is a hierarchy node.
LNT_RW EQU	14	;value is a 16 bit reserved word table offset for
			; reserved word or special character associated
			; with this node.
LNT_SPACES EQU	16	;value is 16 bit count of spaces (blanks)
LNT_NUM EQU	18	;value identifies a numeric constant
LNT_COL EQU	20	;value identifies a column to advance to
LNT_CHAR EQU	22	;value is 1 or 2 ASCII characters associated with
			; this node.  Identical to LNT_CHAR_TOK, but this
			; node is guarenteed to begin a lexical token
			; (needed for _CrLf line splitting)
LNT_CURSOR EQU	24

LN_val_list EQU LN_val		;LNT_LIST's offset to list of child nodes
LN_val_oNam EQU LN_val		;LNT_ONAM's arg (offset into name table for id)
LN_val_orw EQU LN_val		;LNT_RW's arg (offset into res word table)
LN_val_char EQU LN_val		;LNT_CHAR's arg (ASCII char)
LN_val_cbStr EQU LN_val		;LNT_STR's arg (# bytes in string literal)
LN_val_oStr EQU LN_val + 2	;LNT_STR's 2nd arg (offset into program text
				;   table to start of string)
LN_val_cbEnStr EQU LN_val	;LNT_ENSTR's arg (# bytes in string literal)
LN_val_oEnStr EQU LN_val + 2	;LNT_ENSTR's 2nd arg (offset into program text
				;   table to start of string)
LN_val_CsStr EQU LN_val		;LNT_CSSTR's arg (16 bit offset into LIST seg
				; to a str255 struct for a string constant
LN_val_cbSpaces EQU LN_val	;LNT_SPACES's arg (# spaces to list)
LN_val_cbNum EQU LN_val		;size of number in bytes (2, 4, 8)
LN_val_clNum EQU LN_val + 1	;class of number (LIT_D2...LIT_R8)
				; see psint.inc for enumerations
LN_val_otxNum EQU LN_val + 2	;16 bit text offset to number's value
LN_val_col EQU LN_val		;16 bit column to advance to (LNT_COL)

CBLNT_LIST EQU	5	;size of LNT_LIST node
CBLNT_ONAM EQU	5	;size of LNT_ONAM node
CBLNT_RW EQU	5	;size of LNT_RW node
CBLNT_CHAR EQU	5	;size of LNT_CHAR node
			; even though this could be 4 bytes, this allows
			; us to convert a LNT_CHAR node to LNT_RW node
			; without changing pointers.
			; It also allows us to store 2 chars in one node.
CBLNT_STR EQU	7	;size of LNT_STR node
CBLNT_ENSTR EQU 7	;size of LNT_ENSTR node
CBLNT_CSSTR EQU	5	;size of LNT_CSSTR node
CBLNT_SPACES EQU 5	;size of LNT_SPACES node
CBLNT_NUM EQU	7	;size of LNT_NUM node
CBLNT_COL EQU 5		;size of LNT_COL node

LIT_LINENUM EQU 0FFH

;------------------------------------------------------------------
; Flags in lsBosFlags which get reset at every beginning-of-line
;------------------------------------------------------------------
FBOL_GotIf		EQU 01H	;non-zero if an IF opcode has been
				;seen on this line
FBOL_Shared		EQU 02H	;non-zero if opShared has been seen
FBOL_AsClause		EQU 04H	;non-zero if next opIdVTRf is to be
				;followed by AS <type> (set by LrAsType)

;------------------------------------------------------------------
; Flags in lsBosFlags which get reset at every beginning-of-stmt
;------------------------------------------------------------------
FBOS_Channel		EQU 01H ;non-zero if #n, needs to be listed
FBOS_StmtRw		EQU 02H	;non-zero if PushStmtRwBos has been
				;called for stmt
FBOS_PrintSemi		EQU 04H	;non-zero if next PRINT item is to be
				;preceded by ";"
FBOS_DoIdCommas		EQU 08H ;non-zero if every opIdVTRef and
				;opAVTRef is to be preceded by a comma
FBOS_NextIdComma	EQU 10H	;used to list commas between 2nd->nth
				;id (if FBOS_DoIdCommas is set)
FBOS_NextStmtComma	EQU 20H	;used to list opcodes which list
				;as a reserved word the first time
				;they occur in a statement, and as
				;a comma for the 2nd-nth occurence
				;in the statement.  See LrStCase,
				;LrStDim for examples of usage.
FBOS_CallLess		EQU 40H	;remember not to emit () around arg list
FBOS_InputPrompt	EQU 80H	;non-zero if LrInputPrompt has already
				;listed "INPUT "

;2nd byte of word which gets cleared at beginning of every statement
FBOS2_1stCoord		EQU 01H	;non-zero if coord pair seen before -(x,y)
FBOS2_DECLARE		EQU 02H	;non-zero if listing DECLARE stmt
FBOS2_Circle1		EQU 04H	;non-zero if got Circle Start argument
FBOS2_Circle2		EQU 08H	;non-zero if got Circle End argument
FBOS2_NonNilExp		EQU 10H	;see PopNilExp in lsrules.asm
FBOS2_CONST		EQU 20H ;non-zero if listing CONST id=expr stmt
FBOS2_CONST_COMMA	EQU 40H	;non-zero if listing 2nd->nth id=expr in stmt
FBOS2_DIM		EQU 80H ;non-zero if listing DIM/AUTO/PUBLIC etc.

;macro to store a constant in ds:di and bump di by 2
stosDsWord MACRO value
	mov	WORD PTR [di],value
	inc	di
	inc	di
	ENDM

;macro to store a constant in ds:di and bump di by 1
stosDsByte MACRO value
	mov	BYTE PTR [di],value
	inc	di
	ENDM

;Macro to define a List-Rule entry point
ListRule MACRO	entry
	PUBLIC	entry
entry	label	NEAR
	ENDM

sBegin LIST
EXTRN	mpOpLister:word
	;table which maps an opcode to the address of the function which
	; lists it.

EXTRN	mpOpLsArg:word
	;table which maps opcodes into offsets into reserved word table

ife	LSMAIN_ASM
	EXTRN	Stage2:near
	EXTRN	Stage2Inc:near
	EXTRN	Stg1Loop:near
	EXTRN	GrowBdNodes:near
endif	;LSMAIN_ASM

ife	LSUTIL_ASM
	EXTRN	NewChar:near
	EXTRN	NewCsStr:near
	EXTRN	NewId:near
	EXTRN	NewLabel:near
	EXTRN	NewLabelONam:near
	EXTRN	NewModLabel:near
	EXTRN	NewNum:near
	EXTRN	NewONam:near
	EXTRN	NewRw:near
	EXTRN	NewSpaces:near
	EXTRN	NewEnStr:near
	EXTRN	NewStr:near
	EXTRN	NewCol:near
	EXTRN	NewCol1:near
	EXTRN	PopRoot:near
	EXTRN	PushCommaArgs:near
	EXTRN	PushList:near
	EXTRN	PushRevList:near
	EXTRN	PushRoot:near
	EXTRN	PushRootChar:near
	EXTRN	PushRootChars:near
	EXTRN	PushRootLParen:near
	EXTRN	PushRootRParen:near
	EXTRN	PushRootSpc:near
	EXTRN	PushRootONam:near
	EXTRN	PushRootLabel:near
	EXTRN	PushRootRw:near
	EXTRN	PushRootRwSpc:near
	EXTRN	PushRootOpRw:near
	EXTRN	PushRootOpRwSpc:near
	EXTRN	PushRootQStr:near
	EXTRN	PushTemp:near
	EXTRN	PushTempChar:near
	EXTRN	PushTempChars:near
	EXTRN	PushTempLParen:near
	EXTRN	PushTempRParen:near
	EXTRN	PushTempSpc:near
	EXTRN	PushTempRw:near
	EXTRN	PushTempRwSpc:near
	EXTRN	PushTempOpRw:near
	EXTRN	PushTempOpRwSpc:near
	EXTRN	PushTempComma:near
	EXTRN	PushTempCommaSpc:near
	EXTRN	PopPushCommaSpc:near
	EXTRN	PushRootCommaSpc:near
	EXTRN	PushTempIdComma:near
	EXTRN	PushTempOpChars:near
	EXTRN	PushRootOpChars:near
	EXTRN	PushTempOpRwOrComma:near
	EXTRN	PushTempRwOrComma:near
	EXTRN	PopRootPushTemp:near
	EXTRN	PushTempCharSpc:near
	EXTRN	PushRootCharSpc:near
	EXTRN	PushStmtRwIfBos:near
	EXTRN	CharToCharTok:near
	EXTRN	ListOffToPtr:near
	EXTRN	ListSibPtr:near
endif	;LSUTIL_ASM

ife	LSID_ASM
	EXTRN	PushTempONam:near
	EXTRN	PushOffId:near
	EXTRN	PushListStg1:near
	EXTRN	PushRevListStg1:near
endif	;LSUTIL_ASM

ife	LSRULES_ASM
	EXTRN	LrRwSpc:near
endif	;LSRULES_ASM

sEnd	LIST

sBegin	DATA
ife	LSMAIN_ASM
	EXTRN	bdNodes:word
	EXTRN	cLsArgs:byte
	EXTRN	fGotBol:byte
	EXTRN	fLsIncluded:byte
	EXTRN	oNodeRoot:word
	EXTRN	oNodeTemp:word
	EXTRN	opList2:word
	EXTRN	opList:word
	EXTRN	lsBolFlags:byte
	EXTRN	lsBosFlagsWord:word
	EXTRN	lsBosFlags:byte
	EXTRN	lsBosFlags2:byte
	EXTRN	otxLsCursorTmp:word
	EXTRN	colLsCursor:word
	EXTRN	ndLsCursor:word
endif	;LSMAIN_ASM
sEnd	DATA

