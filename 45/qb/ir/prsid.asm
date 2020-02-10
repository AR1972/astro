 	TITLE	prsid.asm - Parser ID Related NonTerminal Functions

;==========================================================================
;
;	Module:  prsid.asm - Parser ID Related NonTerminal Functions
;	Subsystem:  Parser
;	System:  Quick BASIC Interpreter
;
;	See Comments at top of prsnt.asm for rules for writing non-terminal
;	functions
;
;
;==========================================================================

	include		version.inc
	PRSID_ASM = ON

	includeOnce	architec
	includeOnce	context
	includeOnce	names
	includeOnce	opcodes
	includeOnce	parser
	includeOnce	pcode
	includeOnce	prstab
	includeOnce	prsirw
	includeOnce	psint
	includeOnce	qbimsgs
	includeOnce	scanner
	includeOnce	txtmgr
	includeOnce	ui
	includeOnce	util
	includeOnce	variable

	MAXARG EQU 60d	;BASCOM shared constant - max args to SUB/FUNC/DEF
	MAXDIM EQU 60d	;BASCOM shared constant - max args to DIM

ET_MAX_NOFIELDS = ET_MaxStr

FV_ARYELEM EQU 0


sBegin	DATA
assumes	ds,DATA
assumes ss,DATA

fLastIdIndexed	DW 1 DUP (?)

PUBLIC		oNamConstPs
oNamConstPs	DW 0	;non-zero if we're parsing a CONST expression

;pdcl is used to pass info from parser terminal recognizers like
;NtIdSubDecl, NtIdFn [QB4], etc. to the code generator CgDeclare():
;
PUBLIC		pdcl
pdcl		DB size PDCL_st DUP(?)


NTEL_ARGS EQU 0010h			;used as input flag to NtExprOrArg

	extrn	fMergeInSub:byte	;non-zero if MERGING into SUB/FUNC
					;only valid when FLoadActive is TRUE
sEnd	DATA

sBegin	CP
assumes	cs,CP

;====================================================================
; ID Syntactic Elements
;
;       Common identifier non-terminals referenced from bnf.prs
;
;	IdNamCom := idNoType
;	      occurs in COMMON /id/ statements	[QB4]
;	IdAry := id [()]
;	      occurs in SHARED statement. [QB4]
;	IdAryI := id [([integer])]
;	      occurs in STATIC, COMMON [QB4], and array in formal arg list for
;	IdAryElem := id [(arg, ... )]
;	      occurs in expressions, which can be array references or function
;	         invocations. [QB4]
;	IdAryElemRef := id [(arg, ... )]
;	      occurs in GET, LINE INPUT, INPUT, MID$, PUT, READ, RSET,
;	      SADD, SWAP, VARPTR, VARPTR$, VARSEG.
;	      Causes a Rf type Id opcode to be emitted.
;	IdArray() := id
;	      occurs in ERASE, LBOUND, UBOUND
;	IdAryGetPut() := id [(exp, ...)]
;	      occurs in GET, PUT, PALETTE USING statements [QB4]
;	IdAryDim := id [(exp [TO exp], ... )]
;	      occurs in DIM statement.
;	IdAryRedim := id (exp [TO exp], ... )
;	      occurs in REDIM statement.
;	IdFor := id
;	      occurs in FOR and NEXT statements
;	IdSubDef := id <no type char>
;	      occurs in SUB statement
;	IdSubDecl := id <no type char>
;	      occurs in DECLARE SUB statement
;	IdFuncDef := id
;	      occurs in FUNCTION statement
;	IdFuncDecl := id
;	      occurs in DECLARE FUNCTION statement
;	IdType := id
;	      occurs in TYPE statement and AS <type> clause
;	IdFn := FNid
;	      occurs in DEF FN statement
;	IdParm := [BYVAL | SEG] variable
;	      occurs in DECLARE, SUB, FUNCTION, DEF FN parm lists
;	NArgsMax3 := [arg [, [arg] [, arg]]]
;	      occurs in CLEAR and COLOR statements
;	NArgsMax4 := [arg [, [arg] [, [arg] [, arg]]]]
;	      occurs in SCREEN statement
;	NArgsMax5 := [arg [, [arg] [, [arg] [, [arg] [, arg]]]]]
;	      occurs in LOCATE statement
;
; ACTIONidCommon [QB4] - sets varmgr COMMON flag for all ids until end-of-stmt
; ACTIONidShared [QB4] - sets varmgr SHARED flag for all ids until end-of-stmt
; ACTIONidAuto [EB] - sets varmgr AUTO flag for all ids until end-of-stmt
; ACTIONidPublic [EB] - sets varmgr PUBLIC flag for all ids until end-of-stmt
; ACTIONidStatic - sets varmgr STATIC flag for all ids until end-of-stmt
;
;	NOTE [QB4] That it is ok for types, elements, labels, subs, and
;	   common block names to begin
;	   with FN.  It is not ok for scalars, FUNCTIONS or arrays to begin
;	   with FN.
;
;=======================================================================

opId_Ld		EQU opIdLd - opIdLd	;value to add to opIdLd
					; to get opIdLd
opId_Rf		EQU opIdLd - opIdLd	;value to add to opIdLd to get 
					; ref type opcode (same as Ld type)
opId_St		EQU opIdSt - opIdLd	;value to add to opIdLd
					; to get opIdSt
opId_VtRf	EQU opVtRf - opIdLd	;value to add to opIdLd
					; to get opIdVtRf
opId_Scalar	EQU opIdLd - opIdLd	;value to add to opIdxx opcode
					; to get scalar type opcode
opId_Array	EQU opAIdLd - opIdLd	;value to add to opIdxx opcode 
					; to get array type opcode

;*********************************************************************
; STATICF(boolean) FElements()
; Purpose:
;	Look ahead and see if we're looking at record seperator
; Entry:
;	pTokScan points to current token
; Exit
;	Returns zero condition codes iff record separator is seen
;	Alters bx, preserves all other registers (callers assume this)
;
;*********************************************************************
PUBLIC	FElements			
FElements PROC NEAR
	mov	bx,[pTokScan]
	cmp	[bx.TOK_class],CL_UNKNOWNCHAR
	jne	FeExit			;brif not "."
	cmp	[bx.TOK_unknownChar_unknownChar],"."
FeExit:
	ret
FElements ENDP

;*********************************************************************
; ushort NEAR BindVar(ax:pTok)
; Purpose:
;	If we're parsing to SS_PARSE, bind the variable identified by
;	the token 'pTok'.
; Entry:
;	ax = pTok.  points to a token descriptor for the id
;	mkVar is setup for a call to MakeVariable
; Exit:
;	ax = oVar
;
;*********************************************************************
PUBLIC	BindVar
BindVar	PROC NEAR
	push	ax			;save pointer to id's token
	xchg	bx,ax			;bx points to token

	or	[ps.PS_flags],PSF_fRef	;so text mgr knows to scan program
					; if in direct mode 
	TESTM	mkVar.MKVAR_flags,FVI_FNNAME	
	jne	GotFn			;brif id begins with FN
GotFnRet:
	mov	ax,[mkVar.MKVAR_oNam]	;potential return value
	test	[psFlags],PSIF_fBindVars
	je	BindExit		;brif parser not binding variables
					;return oNam (in ax) instead of oVar

	;let varmgr bind variable.
	;If parsing direct mode stmt, scan-state is same current text
	;table's, since if module is SS_RUDE, everything in module is
	
	mov	al,[bx.TOK_id_lexFlags]
	and	al,FLX_asSymConst	;0 if not 'x AS STRING * <sym const>
	or	al,al
	jz	BindVar_Cont

	or	[mkVar.MKVAR_flags2],MV_fONamInOTyp
BindVar_Cont:
	call	MakeVariable
	or	ax,ax			;high bit is set for errors
	js	BindErr			;brif error
BindExit:				;return oVar/oNam in ax
	pop	dx			;discard pTok parm
BindExit1:
	ret

;Make sure its not COMMON FNx or SHARED FNx etc.
GotFn:
	TESTM	mkVar.MKVAR_flags,<FVI_COMMON or FVI_STATIC or FVI_SHARED or FVI_ARRAY or FVI_DIM or FVI_ASCLAUSE or FVI_FORMAL>	
	je	GotFnRet		;brif not a declarative reference
	mov	ax,MSG_FNStart		
	;fall into BindErr

;MakeVariable detected some error, pass it to ParseLine
;in ps.errCode so it can return RudeEdit or ReParse.
;low byte has QBI Std Error Code 
;
BindErr:
	pop	bx			;bx points to token of interest (or 0)
	call	PErrVarMgr		;handle variable mgr error
	jmp	SHORT BindExit1
BindVar	ENDP

;*********************************************************************
; STATICF(boolean) EmitVar(pTok, opBase, cArgs, flags)
;
; Emit one of the following opcodes:
;	opAId<Ld|St|Rf>(<cArgs>,<oNam|oVar>)
;	opAVtRf(<cArgs>,<oNam|oVar>)
; The high bits of the opcode are set to give the explicit type if any. [25]
;
; Entry:
;	   pTok->dsc.id.oNam is the name table offset for the var being defined
;	   pTok->dsc.id.oTyp is the explicit type for the variable being defined
;	      (ET_IMP if id has no explicit type)
;	      (RefTyp(oNam) if it was in an AS clause)
;	   pTok->dsc.id.flags has one or more of the following bits set:
;	      FVI_LVAL      if on left side, or in INPUT, READ stmt
;	      FVI_INDEXED   if var followed by "(" - could be an array or Function
;	      FVI_ASCLAUSE  if var type declared via an AS clause
;	      FVI_DIM       if scalar was seen in a DIM stmt, so var mgr can
;		     detect a scalar being DIMed twice (BASCOM compatibility)
;	   opBase = opId_Ld or opId_St or opId_Rf or opId_VtRf
;	   flags.FEM_Ary means we saw an array, not a scalar
;	     cArgs = number of arguments seen within array's parenthesis
;	     cArgs must be set to 0 if called for a scalar
;	     flags.FEM_AryNoArgs means we saw an array with no (),
;	         like ERASE A.  cArgs = 0 in this case.
;	     flags.FEM_AryDim means we saw something like
;	         DIM(x to y,...)
;	         such that the number of args we pass to the scanner as
;	         an opcode argument is 2 * cArgs.  The number we pass
;	         to MakeVariable is cArgs.)
;	   flags.FEM_ElemOk is TRUE if .elem[.elem...] can be scanned
;	   pTokScan points to '.id' if any elements are to be parsed
;	   mkVar.flags has one or more of the following bits set:
;	      FVI_COMMON    if input is from a COMMON statement [QB4]
;	      FVI_STATIC    if input is from a STATIC statement
;	      FVI_SHARED    if SHARED keyword associated with var [QB4]
;	      The setting of any other flags in mkVar.flags are unimportant.
;	         (and any other bits for that matter)
;
;	      FVI_ARRAY will be set by if its a reference array id
;	      as opposed to load/store.  This tells MakeVariable that
;	      it is definately not a function.
;
; Exit:
;	If syntax error
;	   returns Carry Set and al=PR_BadSyntax after emitting error msg
;	else returns Carry Clear
;	If FV_SQL then if there is no error bx = oVar of variable emitted [34]
;
;*********************************************************************
FEM_Ary		EQU 1
FEM_AryNoArgs	EQU 2
FEM_AryDim	EQU 4
FEM_ElemOk	EQU 8
MKVAR_STATIC_FLAGS EQU FVI_COMMON + FVI_STATIC + FVI_SHARED
DbPub	EmitVar

cProc EmitVar,<NEAR>,<si,di>
	parmW pTok			
	parmW opBase
	parmB cArgs
	parmB flags
cBegin
	mov	si,[pTok]
	mov	ax,[si.TOK_id_oNam]
	mov	[mkVar.MKVAR_oNam],ax
	mov	ax,[mkVar.MKVAR_flags]	;ax = default flags
	and	ax,MKVAR_STATIC_FLAGS	;preserve these flags in mkVar.flags
	or	ax,[si.TOK_id_vmFlags]	;set token specific flags
					; this may set one or more of FVI_LVAL,
					; FVI_ASCLAUSE
					;ax = default flags for scalars
	test	[flags],FEM_Ary
	je	NotAry1			;brif we're emitting a scalar

	;FVI_INDEXED can be set for array or function references.
	;If this is a VTREF (declarative/GET/PUT/ERASE) opcode,
	;or an lvalue (assign,INPUT,READ) opcode,
	;tell MakeVariable this is an array and not a function by
	;setting FVI_ARRAY
	
if	FVI_INDEXED AND 0FFH		
	or	al,FVI_INDEXED		;[34] set FVI_INDEXED for arrays
else					
	or	ah,FVI_INDEXED / 100H	;set FVI_INDEXED for arrays
endif					
	TESTM	si.TOK_id_vmFlags,FVI_FNNAME	
	jne	NotAry1			;brif reference to DEF FN
	cmp	[opBase],opId_VtRf
	je	ItsAnArray		;brif we sure its an array
	cmp	[cArgs],0
	je	ItsAnArray		;ref like X() can't be function
	TESTM	mkVar.MKVAR_flags,FVI_LVAL	
	je	NotAry1			;brif it may be a function ref
ItsAnArray:
.errnz	FVI_ARRAY AND 0FFH
	or	ah,FVI_ARRAY / 100H
NotAry1:
	mov	[mkVar.MKVAR_flags],ax	;pass flags to MakeVariable
	mov	al,[cArgs]
	mov	[mkVar.MKVAR_cDimensions],al

	mov	ax,[si.TOK_id_oTyp]
	test	[flags],FEM_ElemOk
	je	EvNoElem		;brif ref cannot have elements
.errnz	ET_IMP
	or	ax,ax			;test for ET_IMP
	jne	EvNoElem		;brif ref is explicitly typed
	call	FElements		;try to parse .elem.elem...
					;ax is preserved as ET_IMP
	jne	EvNoElem		;brif variable not followed by "."
	mov	[mkVar.MKVAR_oTyp],UNDEFINED
					;tell MakeVariable to look for
					; record variable
	jmp	SHORT EvBind

;ax = si.TOK_id_oTyp
EvNoElem:
	mov	[mkVar.MKVAR_oTyp],ax
	and	[flags],NOT FEM_ElemOk	;remember id has no .elem after it
EvBind:
	mov	ax,si			;pass pTok in ax
	call	BindVar
	xchg	di,ax			;di = oVar
	test	[flags],FEM_ElemOk
	je	EvNoElem1		;brif didn't get .element
	mov	ax,opIdLd		
	test	[flags],FEM_Ary
	je	EvEmit			;brif scalar
	mov	ax,opAIdLd		
	jmp	SHORT EvEmit

EvNoElem1:
	TESTM	si.TOK_id_vmFlags,FVI_ASCLAUSE	
	je	EvNoAs
	mov	[si.TOK_id_oTyp],ET_IMP	;so DIM A(5) AS INTEGER won't list
					; like DIM A(5)% AS INTEGER

;if mkVar.oTyp is a USER DEFINED TYPE, we need
;to emit a opAIdLd or opAIdSt with no explicit type.
;EmitOpcode( (pTok->dsc.id.oTyp <= ET_MAX) ?
;               opIdLd + opBase + opId_Array | pTok->dsc.id.oTyp << 10 :
;            (opBase == opId_St) ? opAIdSt :
;            (opBase == opId_VtRf) ? opAVtRf : opAIdLd)
;
EvNoAs:
	mov	bx,CPOFFSET twOpIdMap
	mov	cx,opIdLd		
	test	[flags],FEM_Ary
	je	NotAry3			;brif we're emitting a scalar
	mov	bx,CPOFFSET twOpAIdMap
	mov	cx,opId_Array + opIdLd	
NotAry3:
	mov	ax,[opBase]
	mov	dx,[si.TOK_id_oTyp]
	cmp	dx,ET_MAX
	ja	EvUserTyp		;brif user defined type
	add	ax,cx			;ax = opcode to emit 
.errnz OPCODE_MASK - 3FFh		
	shl	dx,1			
	shl	dx,1			
	or	ah,dl			;ax = opcode with high bits set
					;     to the explicit type if any
	jmp	SHORT EvEmit

EvUserTyp:
	call	MapBaseOp		;ax = opcode for opBase in ax
EvEmit:
	call	Emit16_AX		;Emit16(opcode)
	test	[flags],FEM_Ary
	je	NotAry2			;brif we're emitting a scalar
	mov	ax,8000H		;cArgs for ERASE A and friends
					;high-bit tells lister not to list ()
	test	[flags],FEM_AryNoArgs
	jne	EvNotDim		;brif no args, like ERASE A
	mov	al,[cArgs]
	sub	ah,ah			;ax = al = cArgs



	test	[flags],FEM_AryDim
	je	EvNotDim		;brif not DIM array
	;for DIM x(1 TO 2, 2 TO 3), tell scanner that cArgs = 4
	shl	ax,1			;ax = cArgs * 2



EvNotDim:				;brif not DIM array
	call	Emit16_AX		;emit cArgs
NotAry2:
	push	di			;Emit16(oVar)
	call	Emit16
	test	[flags],FEM_ElemOk
	je	EvNoElem2		;brif didn't get .element
	push	[opBase]		;pass EeElements opBase
	call	EmitElements		;parse and emit .elem...
	jmp	SHORT EvExit		;return ax as result

EvNoElem2:
	clc				;return success
EvExit:
cEnd

;*********************************************************************
; STATICF(boolean) EmitElements(ax:opBase)
;	Many modifications during revision [15]
; Purpose:
;	Scan .id[.id...] and emit pcode for construct
;
; Entry:
;	parm1 = opBase = opId_Ld or opId_St or opId_Rf
;	pTokScan points to token for '.'
;	mkVar.oTyp contains the type of the variable
;
; Exit:
;	If syntax error
;	   returns Carry Set and al=PR_BadSyntax after emitting error msg
;	else
;	   returns Carry Clear
;	   pTokScan points beyond end of construct
;
;***********************************************************************
DbPub	EmitElements
cProc	EmitElements,<NEAR>,<si,di>	
	parmW	opBase
cBegin	EmitElements			
EeLoop:
	or	[psFlags],PSIF_fNoPeriod
					;so ScanTok stops at "."
					;get's reset by IdTokNoPeriod
	call	ScanTok			;skip past "." token
	call	IdTokNoPeriod		;check for id token with no period in it
	stc				;prepare to return error
DJMP	jl	EeExit			;brif PR_BadSyntax, carry set

	mov	si,[bx.TOK_id_oNam]	;si = oNam
	mov	di,[bx.TOK_id_oTyp]	;save oTyp in di
	call	ScanTok			;skip id token
	test	[psFlags],PSIF_fBindVars
	je	EeSavTyp		;brif parser not binding variables


	;ask typmgr to convert oNam to oElem
	
	push	si			
	push	di			;pass oTyp so RefElem can test the
					; explicit type if any
	cCall	RefElem 		;ax = oElem (high bit set if error)
;Fix bug where a random error message gets generated for an undefined
;element reference or element type mismatch. Fix this only in QBJ to
;be sure it has no affect on frozen QB4.5.
;Note however that this is thought to be a very safe bug fix.
	mov	si,ax			;save si = oElem
	or	si,si			
	jns	EeSavTyp		;brif no error

	;Either the variable manager hasn't seen this oNam in the variable's
	;TYPE definition or the explicit type conflicted with the actual
	;type.  Tell ParseLine() to return ReParse.
	;We call this instead of PErrMsg_AX so we can continue
	;checking for bad syntax.
	
	and	ah,7Fh			;mask off sign bit. ax = error code
	call	ParseErrTokScan
EeSavTyp:
.errnz	ET_IMP
	or	di,di			;compare di with ET_IMP
	jne	EeNoElem		;brif id is explicitly typed (i.e. id#)
	call	FElements		;check for more record elements
	jne	EeNoElem		;brif didn't get record separator
	mov	ax,opOffLd		
EeEmit:
	call	Emit16_AX		;emit opcode opOffLd
	push	si			;emit oNam/oElem
	call	Emit16
	test	[psFlags],PSIF_fBindVars
	je	EeLoop			;brif parser not binding variables
	mov	ax,[mkvar.MKVAR_oTyp]	;ax = oTyp of last element
					;     (returned by RefElem)
	cmp	ax,ET_MAX_NOFIELDS	;compare it to largest non-fielded
					; 	type
	jbe	EeIdNoPeriod		;brif if not a user defined type
	or	ax,ax			;test the top bit of the oTyp
	jns	EeLoop1 		;if not set we have a user type
EeIdNoPeriod:
	mov	ax,MSG_BadElemRef	;pass error code to ParseErrTokScan
	call	ParseErrTokScan		;As we do for bad element names
					;  we generate a ReParse and 
					;  continue parsing
EeLoop1:				
	jmp	EeLoop	
	
;done with .a.b.c loop
;if mkVar.oTyp is a USER DEFINED TYPE, we need
;to emit a opOffLd, opOffLd or opIdSt with no explicit type.
;EmitOpcode( (oTyp <= ET_MAX) ? opOffLd + opBase + oTyp :
;            (opBase == opId_St) ? opOffSt : opOffLd)
;
;di = ET_IMP or token's explicit type
EeNoElem:
	mov	ax,[opBase]
DbAssertRel	ax,ne,opId_VtRf,CP,<EmitElements: opbase = opId_VtRf>   
DbAssertRel	di,be,ET_MAX,CP,<EmitElements: oTyp is user type>
	add	ax,opOffLd		;ax = opBase + opOffLd
.errnz OPCODE_MASK - 3FFh		
	xchg	ax,di			;di = oTyp, ax = opcode to Emit
	xchg	ah,al			;ah = oTyp
	shl	ax,1			
	shl	ax,1			
	or	ax,di			;ax = opcode with oTyp in high bits
EeEmit2:
	call	Emit16_AX		;emit opOff<Ld|St><Typ>
	xchg	ax,si			;ax = oNam/oElem
	call	Emit16_AX
	clc				;return success
EeExit:
cEnd	EmitElements			

;Tables used to map from opBase to opcodes
twOpBase LABEL WORD			;opBase search table
	dw	opId_St
	dw	opId_Ld
twOpBaseEnd LABEL WORD
CB_OPBASE = twOpBaseEnd - twOpBase

twOpIdMap LABEL WORD
	dw	opIdSt			;opId_St maps to this for scalars
	dw	opVtRf			;opId_Ld maps to this for scalars
	dw	opIdLd			;opId_Rf and opId_VtRf maps to this

twOpAIdMap LABEL WORD
	dw	opAIdSt			;opId_St maps to this for arrays
	dw	opAVtRf			;opId_Ld maps to this for arrays
	dw	opAIdLd			;opId_Rf and opId_VtRf maps to this

;*********************************************************************
; MapBaseOp
; Purpose:
;	Map an opBase (opId_St etc.) to an opcode.
; Entry:
;	ax = value to search for (opId_St etc.)
;	bx = ptr to table of opcodes which cooresponds to twOpBase
; Exit:
;	ax = opcode
;
; Alters ES
;
;*********************************************************************
MapBaseOp PROC NEAR
	mov	dx,CPOFFSET twOpBase
	mov	cx,CB_OPBASE
	jmp	MapCpW			;ax = bx[find[ax,dx,cx]]
MapBaseOp ENDP

	
;*********************************************************************
; ushort NEAR SubRef(oNam)
; Purpose:
;	Map the oNam for a SUB to its oPrs.
; Entry:
;	ax = oNam of subprogram
; Exit:
;	if successful, ax = oPrs, carry clear on exit
;	else ps.errCode is set with error code, carry set on exit
;
;*********************************************************************
cProc	SubRef,<PUBLIC,NEAR>
cBegin
	push	ax			;pass oNam of sub
	PUSHI	ax,PT_SUB
	sub	ax,ax
	push	ax
	call	PrsRef			;ax = error or oPrs
	or	ax,ax
	jns	SubRefGood		;brif no error

	;Don't set PSERR_fAlert flag, wait until ScanTime to report the error
	; since user may have just deleted =B from A=B, which would make
	; A now look like both a variable and an implied call.
	mov	ah,PSERR_fRude / 100h	;set rude edit flag in result
	call	ParseErr0		;report it to ParseLine's caller
	stc				;return error result
	jmp	SHORT SubRefExit

SubRefGood:
	call	UndoLogPrs		;remember to free prs entry if
					; we turn this line into a reparse
					; ax is preserved as oPrs
	clc				;return success
SubRefExit:
cEnd

;*********************************************************************
; STATICF(PARSE_RESULT) NtConsumeExp()
;
; Purpose:
;	Parse an expression.
;	If successfully parsed, return PR_GoodSyntax.
;	If one is not found, report error and return PR_BadSyntax.
;	If expression had bad syntax, return PR_BadSyntax.
;	In other words, identical to NtExp(), but it won't take
;	   PR_NotFound for an answer.
; Exit:
;	al = PR_GoodSyntax or PR_BadSyntax, condition codes set accordingly
;
;*********************************************************************
PUBLIC	NtConsumeExp
NtConsumeExp PROC NEAR
	call	NtExp
	je	PErrExpExpr		;brif result == PR_NotFound
					; error "Expected expression"
					; al = PR_BadSyntax
	ret
NtConsumeExp ENDP

;*********************************************************************
; PARSE_RESULT NEAR PErrExpExpr()
;
; Purpose:
;	generate error "Expected expression" and return PR_BadSyntax
; Exit:
;	al = PR_BadSyntax
;
;*********************************************************************
PUBLIC	PErrExpExpr
PErrExpExpr PROC NEAR
	mov	ax,MSG_ExpExp
	jmp	PErrExpMsg_AX		;Error "Expected expression"
					; al = PR_BadSyntax
PErrExpExpr ENDP

;*********************************************************************
; NtExprOrArg()
; 
; Purpose:
;	Parse an expression or an arg based on the value in ax
; Entry:
;	ax is tested for the flag
;	    NTEL_ARGS: if set allow SEG, BYVAL and A() args
;			        otherwise only allow a normal expression
; Exit:
;	same as NtArg
;	
;*********************************************************************
NtExprOrArg PROC
	test	ax,NTEL_ARGS
	jnz	NtArg
	call	NtExp
	mov	dx,0
	ret
NtExprOrArg ENDP

;*********************************************************************
; ExpRParenLastToken
; Purpose:
;	generate the error "expected ')'" referring to the last token
;	consumed
; Exit:
;	al = PR_BadSyntax
;*********************************************************************
ExpRParenLastToken PROC NEAR
	mov	ax,[pTokLastConsumed]	
	mov	[pTokScan],ax		;reset pTokScan to point to ","
					; so it will be highlighted
	mov	ax,IRW_RPAREN		
	jmp	PErrExpRw_Ax		;generated "expected ')'"
ExpRParenLastToken ENDP

;*********************************************************************
; PARSE_RESULT NEAR NtIdCallArg()
;
; Purpose:
;	Try to parse an identifier of the form:
;	   "[BYVAL | SEG] id[([exp[,exp...]])]"
;	This can occur in the following statements:
;	   tkCALL IdSub [tkLParen IdCallArg {tkComma IdCallArg} tkRParen]
;	Tests to ensure that we haven't yet reached the maximum number
;	of args before branching into 
;
;*********************************************************************
PUBLIC	NtIdCallArg
NtIdCallArg PROC NEAR
	cmp	[cIdArgs],MAXARG
	jae	ExpRParenLastToken	;BASCOM can't handle more than 60
					; args, so we shouldn't either
; fall into NtArg
NtIdCallArg ENDP

;*********************************************************************
; STATICF(PARSE_RESULT) NtArg()
;
; Purpose:
;	Parse and generate code for:
;	   [BYVAL | SEG] expression or
;	   array reference of the form x()
;	Emit the following pcode:
;	   [opByval | opSeg] <expression's pcode>
;	   opAIdRfxx(oVar,0)
;	NOTE: BYVAL x() is illegal because it makes no sense
;	      SEG x() is illegal because we don't want to document
;	              the format of array descriptors to outside world.
; Exit:
;	Returns al = PR_NotFound, PR_GoodSyntax or PR_BadSyntax
;	If result is PR_GoodSyntax, bumps cIdArgs by 1, no matter what
;	   recursion takes place and returns
;	   dx = NTEL_ARGS if SEG, BYVAL, or array ref of form x() seen
;		0 otherwise
;
;*********************************************************************
cProc	NtArg,<NEAR,PUBLIC>,<di>
cBegin
	push	[cIdArgs]
	mov	ax,IRW_Byval
	call	TestScan_AX		;see if current token is BYVAL
					;bx points to current token
	.erre	opByval
	mov	di,opByval
	je	GotByvalSeg		;brif got BYVAL keyword
	sub	di,di			;assume no SEG
	mov	ax,IRW_Seg
	call	TestScan_AX		;see if current token is SEG
					;bx points to current token
	jne	NoByvalSeg		;brif didn't get SEG keyword
	mov	di,opSeg
	.erre	opSeg

GotByvalSeg:
	call	ScanTok			;skip BYVAL or SEG token
					;bx points to current token

;di = 0 for no SEG or BYVAL, opByval for BYVAL, opSeg for SEG
;bx points to current token
NoByvalSeg:
	or	di,di
	jne	NotAryArg		;can't have arg of form 'BYVAL A()'
					; or 'SEG A()'
	cmp	[bx.TOK_class],CL_ID
	jne	NotAryArg		;brif token isn't an id
	call	Peek1Tok		;see if its an array ref of the form x()
	mov	ax,IRW_LParen
	call	TestPeek_AX
	jne	NotAryArg		;brif not '('
	call	PeekNextTok
	mov	ax,IRW_RParen
	call	TestPeek_AX
	jne	NotAryArg		;brif not ')'

	;we did get an array reference of the form x()
	;if (!EmitVar(pTokScan, opId_VtRf, 0, FALSE)) return PR_BadSyntax
	
	push	[pTokScan]		;pass pointer to id's token
	PUSHI	ax,opId_Ld		;make opcode a Ld variant
	PUSHI	ax,0			;pass cArgs == 0
	PUSHI	ax,FEM_Ary		;let EmitVar know its an array ref
	call	EmitVar
	jc	NtArgExit		;brif unsuccessful (al = PR_BadSyntax)
	call	ScanTok			;skip id
	call	ScanTok			;skip '('
	call	ScanTok			;skip ')'
	jmp	SHORT ItsAnArg		

NotAryArg:
	or	di,di
	je	MaybeExp		;brif no tokens consumed yet
	call	NtConsumeExp		;error if can't consume an expression
	jle	NtArgExit		;brif result != PR_GoodSyntax
EmitByvalSeg:
	or	di,di
	je	NtArgGood		;brif no BYVAL or SEG parm
	push	di
	call	Emit16			;emit opByval or opSeg
ItsAnArg:
NtArgGood:
	mov	al,PR_GoodSyntax	;return PR_GoodSyntax
	jmp	SHORT NtArgExit

MaybeExp:
	call	NtExp			;consume expression, al = result
NtArgExit:
	pop	dx			;dx = caller's cIdArgs
	inc	dx
	or	al,al			;set condition codes for caller
	jle	NtArgExit1		;brif result != PR_GoodSyntax
	mov	[cIdArgs],dx		;bump cIdArgs
NtArgExit1:
cEnd

;*********************************************************************
; NtExprList
;	Completely rewritten during revision [15]
; Purpose:
;	Parse and generate code for 
;		(expr1, expr2, ..., exprN)
;	Each of these expressions can be:
;	-  An expression
;	-  A scalar or array element
;
;	Called by NtImpliedLetOrCall and EmitElements.
;
;
; Exit:
;	Returns ax = PR_NotFound, PR_GoodSyntax or PR_BadSyntax
;	condition codes set based on value in [al]
;	cx = number of args scanned. 0 if no "(" found.
;if NOT FV_QB4LANG
;	NtELFlags contains output flags
;		NTEL_ARGS is set if an arg of the form x(), or
;				SEG x, or BYVAL x is seen
;if     FV_ARYELEM
;		NTEL_STOPONTO is set if TO was encountered
;endif ;FV_ARYELEM
;endif ;NOT FV_QB4LANG
;
;*********************************************************************
DbPub	NtExprList
cProc	NtExprList,<NEAR>,<di>
cBegin
	xor	di,di			;initialize arg count to 0
	mov	ax,IRW_LPAREN
	call	TryScan_AX		;consume "(" if present
	mov	ax,PR_NotFound		;assume "(" not found
	jne	NoArrayArg		;brif "(" not found

NtELLoop:
	call	NtExp			
	jg	NtELGotArg		;brif result is PR_GoodSyntax
	jl	NtELBadSyn		;brif result is PR_BadSyntax
	call	PErrExpExpr		;error "Expected expression"
	jmp	SHORT NtELBadSyn
NtELGotArg:
	inc	di			;bump arg count
.erre	MAXARG EQ MAXDIM		;since NtExprList used for parsing
					; both arg list Call-Less call w/
					; parens and array on lhs of assgn
	cmp	di,MAXARG		
	jae	ConsumeRParen		
	mov	ax,IRW_Comma
	call	TryScan_AX		;try to consume a ","
	je	NtELLoop		;if "," found then repeat loop


ConsumeRParen:
	mov	ax,IRW_RPAREN
	call	ConsumeRw_AX		;consume ")" if present
	jc	NtELBadSyn		;brif not found al = PR_BadSyntax
	mov	al,PR_GoodSyntax	;found it --- return PR_GoodSyntax
	
NoArrayArg:
	mov	cx,di			;return cx = arg count
NtELBadSyn:
	or	al,al			;set condition codes for caller
cEnd

;*********************************************************************
; EnsRude
; Purpose:
;	This is called by parser non-terminals that require current
;	text table to be in SS_RUDE state.  If current table isn't
;	an error is generated that will force ParseLine's caller
;	to descan to SS_RUDE.
;	The reason we need to retry the call to ParseLine after
;	calling AskRudeEdit is because the pcode already emitted for this
;	line could have been emitted in SS_PARSE, and the pcode for
;	the rest of the line would be emitted in SS_RUDE.
; Exit:
;	if already in SS_RUDE
;	   Condition codes = Z
;	else
;	   Condition codes = NZ
;	   Either PSF_UndoEdit or PSF_fRetry bits are set in ps.flags
;	
;*********************************************************************
EnsRude PROC NEAR
	cmp	[txdCur.TXD_scanState],SS_RUDE
	je	AlreadyRude		;brif scan-state = SS_RUDE

	call	ParseUndo		;We must call this before ModuleRudeEdit
					; or else we will try to free some
					; DEF FN prs's which no-longer exist
	call	AskRudeEdit		;see if user wants to back out of edit
	mov	al,PSF_UndoEdit
	je	ErBackOut		;brif user wants to back out
	mov	al,PSF_fRetry		;tell caller to call ParseLine again
ErBackOut:
	or	[ps.PS_flags],al
	mov	ax,ER_IER
	call	ParseErr0		;stop's subsequent calls to MakeVariable
	or	al,al			;set nz condition codes
AlreadyRude:
	ret
EnsRude	ENDP


;*=========================================================================
;*    I D    R E L A T E D    N O N - T E R M I N A L    F U N C T I O N S
;*
;*    NOTE:  These functions are arranged alphabetically
;*
;*=========================================================================

;*********************************************************************
; PARSE_RESULT NEAR NtACTIONidCommon()
; Purpose:
;	Remember that subsequent ids in this statement have this attribute.
;	This occurs in the following statement:
;	   COMMON [/id/] ACTIONidCommon ...
;
;*********************************************************************
PUBLIC NtACTIONidCommon
NtACTIONidCommon PROC NEAR
	call	EnsRude
	mov	ax,FVI_COMMON
	jmp	SHORT SetIdMask
NTACTIONidCommon ENDP

;*********************************************************************
; PARSE_RESULT NEAR NtACTIONidShared()
; Purpose:
;	Remember that subsequent ids in this statement have this attribute.
;	This occurs in the following statement:
;	   SHARED ACTIONidShared IdAry {tkComma IdAry}
;
;*********************************************************************
PUBLIC NtACTIONidShared
NtACTIONidShared PROC NEAR
	mov	ax,FVI_SHARED
	jmp	SHORT SetIdMask
NTACTIONidShared ENDP



;*********************************************************************
; PARSE_RESULT NEAR NtACTIONidStatic()
; Purpose:
;	Remember that subsequent ids in this statement have this attribute.
;	This occurs in the following statement:
;	   STATIC ACTIONidStatic IdAryI {tkComma IdAryI}
;
;*********************************************************************

;*********************************************************************
; STATICF(PARSE_RESULT) SetIdMask(mask)
;
; Purpose:
;	Set one or more of the following flags into mkVar.flags:
;	   FVI_COMMON    if input is from a COMMON statement [QB4]
;	   FVI_STATIC    if input is from a STATIC statement
;	   FVI_SHARED    if SHARED keyword associated with var [QB4]
;
; NOTE:  NtExp() preserves the value of mkVar.flags and
;	      sets it to 0 for all id's encountered within the expression
;
;*********************************************************************
PUBLIC NtACTIONidStatic
NtACTIONidStatic PROC NEAR
	mov	ax,FVI_STATIC
NTACTIONidStatic ENDP
	;fall into SetIdMask
SetIdMask PROC NEAR
	or	[mkVar.MKVAR_flags],ax
RetGoodSyntax:
	mov	al,PR_GoodSyntax
	ret
SetIdMask ENDP


;*********************************************************************
; PARSE_RESULT NEAR NtAsClause(al=flags, bx=pTokId)
; Purpose:
;	Parse AS (ANY | INTEGER | LONG | SINGLE | DOUBLE | STRING * n | id)
;
; Entry:
;	al = flags, set as follows:
;	     FAS_fNoVarLenStr if no var length STRING syntax is allowed
;		(this flag is ignored if FV_FARSTR is true)
;	     FAS_fNoFixLenStr if no fixed length STRING/TEXT syntax is allowed
;	     FAS_fAllowAny if AS ANY or AS FIELD (EB) is allowed
;	     FAS_fDontBind if we're not to call RefType(x) for
;	       id as x  (set if AS clause is seen inside TYPE/END TYPE block)
;	     FAS_fDontSetONam if we're not to set FOO's NM_fAs [QB4]
;	       bit if we see FOO AS BAR.  Setting this bit prevents any
;	       variables/constants/procedures named FOO.xxx
;	     FAS_fNoUserType if AS <userType> is not allowed and
;				FORMs not allowed (EB).
;	     FAS_fField if As Field allowed (EB)
;	bx = pTokId points to token for variable (i.e. to FOO for case
;	     FOO AS BAR)
;	pTokScan points to AS token
;	If RefTyp is not to be called for this variable, pTokId = NULL
;
; Exit:
;	If good syntax, emits the following pcode and returns PR_GoodSyntax:
;	  AS ANY ==> opAsTypeExp(ET_IMP,column)
;	  AS INTEGER ==> opAsTypeExp(ET_I2,column)
;	  AS LONG ==> opAsTypeExp(ET_I4,column)
;	  AS SINGLE ==> opAsTypeExp(ET_R4,column)
;	  AS DOUBLE ==> opAsTypeExp(ET_R8,column)
;	  AS STRING ==> opAsTypeExp(ET_SD,column)
;	  AS STRING * nn ==> opAsTypeFixed(0x8000 + ET_SD, nn, column)
;	     (only valid if fInType is TRUE)
;	  AS STRING * <symbolic const> ==> 
;			opAsTypeExp(0x8000 + ET_SD,oNam,column)
;	     (only valid if fInType is TRUE)
;	  AS CURRENCY (if FV_CURRENCY)  ==> opAsTypeExp(ET_Cy,column)
;	  AS TEXT (if EB) ==> opAsTypeExp(ET_Tx,column)
;	  AS FORM <command equivalent> (if EB) ==> opCmdAsType(iCe , column)
;	  AS <id> ==> opAsType(<oNam for id>,column)
;	  if pTokId is not NULL, pTokId->id.oTyp = oTyp from AS clause
;
;	Else returns PR_NotFound or PR_BadSyntax
;	Condition codes set based on result in al
;
;*********************************************************************
FAS_fNoVarLenStr	EQU 01h
FAS_fAllowAny		EQU 02h
FAS_fDontBind		EQU 04h
FAS_fDontSetONam	EQU 08h
FAS_fNoFixLenStr	EQU 10h
FAS_fNoUserType		EQU 20h

PUBLIC	NtAsClause	;for debugging
cProc	NtAsClause,<NEAR>,<si,di>
	localB	flags
	localW	columnAs		;source column where AS occurred
	;register si = pTokId
	;register di = oTyp
cBegin
	mov	[flags],al
	mov	si,bx			;si = pTokId
	mov	ax,IRW_AS
	call	TestScan_AX
	mov	al,PR_NotFound		;prepare to return NotFound
	jne	J1_NtAsExit		;brif current token isn't 'AS'
	mov	ax,[bx.TOK_oSrc]	;ax = column for AS
	mov	[columnAs],ax
	call	ScanTok			;consume 'AS' token


	mov	ax,STI_AsClausePrim + OFFSET CP:tState
	test	[flags],FAS_fNoUserType
	jne	CallParse		;brif AS <userType> not allowed
	mov	ax,STI_AsClauseAny + OFFSET CP:tState ;parse AS <type>
	test	[flags],FAS_fAllowAny
	jne	CallParse		;brif AS ANY not allowed
	mov	ax,STI_AsClause + OFFSET CP:tState ;parse AS <type | userType>
CallParse:
	mov	[pStateLastScan],ax	;setup for call to PErrState below
	call	NtParse			;al = parse result for AS <clause>
	jg	GoodAs			;brif good syntax
	jl	J1_NtAsExit		;brif bad syntax -- only occurs if 
					; user defined types are allowed and
					; an invalid identifier was encountered
	call	PErrState		;output 'expected INTEGER, SINGLE, ...
					; based on pStateLastScan above
					;al = PR_BadSyntax
J1_NtAsExit:
	jmp	NtAsExit		;brif NtParse=PR_BadSyntax/PR_NotFound

;Pcode emitted by NtParse(AsClause) is:
; opAsType(oTyp)
; opAsTypeExp(ET_I2)
;   :
; opAsTypeExp(ET_SD)
;
GoodAs:
	call	RudeIfErr		;if this line gets any kind of error,
					; we will descan to ss-rude.  First,
					; ask user if he wants to back out of
					; edit for Edit & Continue.
	cmp	[si.TOK_id_oTyp],ET_IMP
	je	NotAsExplicitId		;brif not implicit id
; Got As Explicit Id
	mov	bx,[pTokScan]
	mov	ax,[si.TOK_oSrc]	;ax = column id began in
	mov	[bx.TOK_oSrc],ax	;set field used by PErrMsg
	mov	ax,MSG_IdImp		;error: id can't end with $!#&%
	jmp	NtAsErrMsg

NotAsExplicitId:
	mov	bx,[ps.PS_bdpDst.BDP_pbCur]
	mov	di,[bx-2]		;di = oTyp or oNam from AS clause
	cmp	WORD PTR [bx-4],opAsType 
	jne	NtAsNotUser		;brif didn't get id AS <user type>
					; may have been id AS INTEGER
					; or id AS ANY

;Got user type like FOO AS BAR
;di = oNam for BAR, not oTyp
;Set namtbl bit NM_fAS for x, so lexer knows all future references to A.B
; are 3 tokens.
;
	test	[flags],FAS_fDontSetONam
	jne	DontSetAsBit
	mov	al,MSG_BadElemRef	;identifier cannot have "."
	test	[si.TOK_id_lexFlags],FLX_hasPeriod
	jne	NtAsErrSiAl		;brif id.anything AS <usertype>

	mov	al,PUNDO_oNamAs		;pass entry type in al
	mov	dx,[si.TOK_id_oNam]	;pass oNam in dx
	call	ParseUndoLog		;remember to undo SetONamMask if
					; we turn this line into a reparse

	push	[si.TOK_id_oNam]	;pass oNam
	PUSHI	ax,NM_fAS		;pass mask for bit to be set
	call	SetONamMask		;set flag, dl=old value of flag
	test	dl,NM_fAS
	jne	DontSetAsBit		;brif bit was already set, no change
	or	[mrsCur.MRS_flags],FM_asChg
					;causes PreScanAsChg before scanning
					;to convert any A.B id references
					;into record elements
DontSetAsBit:
	test	[flags],FAS_fDontBind
	jne	J1_NtAsEnd		;brif no need to call RefTyp
	test	[psFlags],PSIF_fBindVars
	je	J1_NtAsEnd		;brif parser not binding variables

	mov	ax,[ps.PS_otxLine]	;pass source offset of reference
	mov	dx,UNDEFINED
	;can't test [txdCur.TXD_flags],FTX_mrs because prs's text table
	; isn't created until after parsing "SUB id(x AS foo)" line
	
	cmp	[grs.GRS_oPrsCur],dx
	je	NotInSubOrFunc		;brif ref isn't in SUB/FUNCTION
	cmp	[prsCur.PRS_procType],PT_DEFFN
	je	NotInSubOrFunc		;brif ref is in DEF FN
	xchg	ax,dx			;ax = UNDEFINED, because all
					; TYPEs are defined at module level,
					; so are available to any SUB/FUNCTION
NotInSubOrFunc:
	push	di			;pass oNam to RefType
	push	ax			;pass otx to RefType
	call	RefTyp			;ask type mgr for oTyp of AS id
	mov	di,ax			;di = ax = oTyp
	or	ax,ax
	jns	J1_NtAsEnd		;brif no error
;si = id token ptr, al = error code
NtAsErrSiAl:
	sub	ah,ah			;low byte has QBI Std Error Code
	mov	bx,si
	call	ParseErr		;ParseErr(ax,bx)

J1_NtAsEnd:
	jmp	SHORT NtAsEnd	

;Got explicit type like AS INTEGER or AS ANY
NtAsNotUser:
	cmp	di,ET_SD
	jne	NtAsEnd			;brif not STRING in TYPE stmt
NtAsTestFlags:
	test	[flags],FAS_fNoFixLenStr
	jne	NtAsEnd			;brif can't accept fixed len string
	test	[flags],FAS_fNoVarLenStr
	jne	GetFixed		;brif can't accept var len string

	;We can accept either fixed length, or variable length string syntax
	mov	ax,IRW_Mult		;Consume "* const" clause
	call	TestScan_AX
	jne	NtAsEnd			;brif didn't get * (fixed len string)
GetFixed:
	mov	ax,IRW_Mult		;Consume "*"
	call	ConsumeRw_AX
	jc	NtAsExit		;brif syntax error (al = PR_BadSyntax)
	add	di,ET_FS-ET_SD		;Convert oTyp to fixed variant
	call	IdTok			;bx points to current token
	jne	NotSymConst		;brif its not an id token
	push	[bx.TOK_id_oNam]	;preserve oNam
	call	ScanTok			;consume symbolic constant's token
	pop	ax			;ax = oNam of symbolic constant
	mov	bx,[ps.PS_bdpDst.BDP_pbCur]
	mov	dx,di			;dx = oTyp
	or	dh,80h			;set high bit of oTyp to store
					;  in pcode
	;set flag so we can tell MakeVariable that fsLength is oNam
	; of symbolic constant.
	
	or	[si.TOK_id_lexFlags],FLX_asSymConst

	jmp	SHORT UpdatePcode
NotSymConst:
	call	NtLitI2NoCode		;consume integer (if any)
					;bx points to I2 value in pcode buf
	jl	NtAsExit		;brif PR_BadSyntax
	je	NtAsSnErr		;brif PR_NotFound
	mov	ax,[bx]			;ax = string length
	or	ax,ax			
	jnz	GotGoodLen		;brif string length is > zero
	mov	ax,MSG_IllegalNumber	
	call	PErrPrevTok_Ax		
	jmp	SHORT NtAsExit
GotGoodLen:
	mov	dx,di			;dx = oTyp
UpdatePcode:
	mov	[bx-2],dx		;replace the old oTyp operand in pcode
	mov	dx,opAsTypeFixed	;change opcode from opAsTypeExp to
	mov	[bx-4],dx		;  opAsType2

.errnz MKVAR_fsLength - MKVAR_oNamForm	
NtAsEmitExtraOperand:
	mov	[mkvar.MKVAR_fsLength],ax ;oNam/cb passed to makevariable
	call	Emit16_Ax		;emit number of bytes or oNam
					; of constant which gives # of bytes
NtAsEnd:
	mov	ax,[columnAs]		
	call	Emit16_AX		;emit column operand
	mov	[si.TOK_id_oTyp],di	;set the variable's oTyp
	or	[si.TOK_id_vmFlags],FVI_ASCLAUSE
	mov	al,PR_GoodSyntax	;return PR_GoodSyntax
NtAsExit:
	or	al,al			;set condition codes for caller
cEnd

NtAsSnErr:
	mov	ax,ER_SN		;Syntax Error
NtAsErrMsg:
	call	PErrMsg_AX		;al = PR_BadSyntax
	jmp	SHORT NtAsExit		;return PR_BadSyntax


;*********************************************************************
; NtLitI2NoCode
; Purpose:
;	Parse a signed integer
; Exit:
;	If successfully parsed,
;	   no pcode is emitted, but value is stored beyond end
;	      of pcode buffer,
;	   bx points to value,
;	   al = PR_GoodSyntax
;	else
;	   al = PR_NotFound or PR_BadSyntax
;	Condition codes are set based on value in al
;
;*********************************************************************
NtLitI2NoCode PROC NEAR
	call	NtLitI2			;consume integer, emit 2 byte value
	jle	NoI2			;brif no integer found, or snerr
	sub	[ps.PS_bdpDst.BDP_cbLogical],2 ;eliminate pcode emitted by NtLitI2
	mov	bx,[ps.PS_bdpDst.BDP_pbCur] ;bx points beyond emitted value
	dec	bx			;pbCur -= 2
	dec	bx
	mov	[ps.PS_bdpDst.BDP_pbCur],bx
	or	al,al			;set condition codes for caller
NoI2:
	ret
NtLitI2NoCode ENDP

;*********************************************************************
; PARSE_RESULT NEAR NtIdAry()
;
; Purpose:
;	Try to parse a scalar or array id of the form: id [()] [AS <type>]
;	This can occur in the following statements:
;	   SHARED IdAry {tkComma IdAry}
;
; Exit:
;	Returns PR_NotFound, PR_GoodSyntax or PR_BadSyntax
;
;*********************************************************************
PUBLIC	NtIdAry
NtIdAry	PROC NEAR
	mov	ax,IDM_INDEXED OR IDM_VTREF OR IDM_AS
	jmp	SHORT NtId
NtIdAry	ENDP

;*********************************************************************
; PARSE_RESULT NEAR NtIdAryElem()
;
; Purpose:
;	Try to parse a scalar or array element of the form:
;	   id[<type>] [(exp, ... )] [.id[.id...]]
;	This can occur in the following statements:
;	   FIELD #n, 15 AS IdAryElem, 20 as IdAryElem [QB4]
;	   MID$ (IdAryElem, 5, 3) = exp
;	   LSET IdAryElem = exp
;	   RSET IdAryElem = exp
;	   NtExp() calls this for any id's encountered within expressions
;
; Exit:
;	Returns PR_NotFound, PR_GoodSyntax or PR_BadSyntax
;
;*********************************************************************
PUBLIC	NtIdAryElem
NtIdAryElem PROC NEAR
	mov	ax,IDM_INDEXED OR IDM_EXP OR IDM_ARGS OR IDM_ELEM
	jmp	SHORT NtId
NtIdAryElem ENDP

;*********************************************************************
; PARSE_RESULT NEAR NtIdAryElemRef()
;
; Purpose:
;	Try to parse a scalar or array element of the form:
;	   id[<type>] [(exp, ... )]
;	and emit a Rf Id opcode.
;	This can occur in the following statements:
;	   INPUT IdAryElem
;	   LINE INPUT IdAryElem
;	   READ IdAryElem
;	   y = VARPTR(IdAryElem)
;
; Exit:
;	Returns PR_NotFound, PR_GoodSyntax or PR_BadSyntax
;
;*********************************************************************
PUBLIC	NtIdAryElemRef
NtIdAryElemRef PROC NEAR
	mov	ax,IDM_INDEXED OR IDM_EXP OR IDM_REF OR IDM_ELEM
	jmp	SHORT NtId
NtIdAryElemRef ENDP

;*********************************************************************
; PARSE_RESULT NEAR NtIdAryGetPut()
;
; Purpose:
;	Try to parse a scalar or array element of the form:
;	   id[<type>] [(exp, ... )]
;	This can occur in the following statements:
;	   GET (10,10)-(20,20),IdAryGetPut
;	   PUT (10,10),IdAryGetPut
;	   PALETTE USING IdAryGetPut
;
; Exit:
;	Returns PR_NotFound, PR_GoodSyntax or PR_BadSyntax
;
;*********************************************************************
PUBLIC	NtIdAryGetPut
NtIdAryGetPut PROC NEAR
	mov	ax,IDM_INDEXED OR IDM_EXP OR IDM_ARRAY	
	jmp	SHORT NtId
NtIdAryGetPut ENDP

;*********************************************************************
; PARSE_RESULT NEAR NtIdAryI()
;
; Purpose:
;	Try to parse a scalar or array id of the form:
;	   id [([<integer>])] [AS <type>]
;	This can occur in the following statement:
;	   STATIC IdAryI {tkComma IdAryI}
;	   COMMON [SHARED] [/id/] IdAryI {tkComma IdAryI} [QB4]
;
; Exit:
;	Returns PR_NotFound, PR_GoodSyntax or PR_BadSyntax
;
;*********************************************************************
PUBLIC	NtIdAryDim2
NtIdAryDim2	LABEL	NEAR



PUBLIC	NtIdAryI
NtIdAryI PROC NEAR
	mov	ax,IDM_INDEXED OR IDM_CONST OR IDM_VTREF OR IDM_AS
	jmp	SHORT NtId
NtIdAryI ENDP

;*********************************************************************
; PARSE_RESULT NEAR NtIdArray()
;	Parse an identifier of the form:   "id"
;	This can occur in the following statements:
;	   ERASE IdArray {tkComma IdArray}
;	   PALETTE [USING IdArray]...
;
;*********************************************************************
PUBLIC	NtIdArray
NtIdArray PROC NEAR
	mov	ax,IDM_ARRAY		
	jmp	SHORT NtId
NtIdArray ENDP

;*********************************************************************
; PARSE_RESULT NEAR NtIdFor()
;
; Purpose:
;	Try to parse an identifier of the form:  id [<type>]
;	This can occur in the following statements:
;	   FOR IdFor ...
;	   NEXT IdFor ...
;
; Exit:
;	Returns PR_NotFound, PR_GoodSyntax or PR_BadSyntax
;
;*********************************************************************
PUBLIC	NtIdFor
NtIdFor PROC NEAR
	mov	ax,IDM_REF
	jmp	SHORT NtId
NtIdFor ENDP

;*********************************************************************
; PARSE_RESULT NEAR NtId(ax:mask)
;
; Purpose:
;	Try to parse an id.  The legal format of the ID depends on the bits
;	set in the input parameter 'mask'.
;
; Entry:
;	If the static variable [oNamConstPs] is non-zero, variables which
;	   are not CONSTANT are not allowed
;	ax:mask's bits have the following meanings:
;	   If no bits in mask are set, the only syntax allowed is ID
;	   IDM_INDEXED: ID can be (but need not be) followed by ()
;	      IDM_CONST: (<integer constant>) can (but need not) follow ID
;	      IDM_EXP: exp [,...] must follow ID( if ( is seen
;	      IDM_OEXP: exp [,...] can (but need not) follow ID( if ( is seen
;	         IDM_DIM: each exp can be followed by [TO exp]
;	         IDM_ARGS: each exp can be preceded by BYVAL or SEG
;	            and each exp can be of the form id() or a normal expression
;	   IDM_ARRAY: an array id opcode is emitted even if no
;	      indecies are seen
;	   IDM_AS: id can be followed by [AS id]
;	   IDM_ELEM: id can be followed by [.id[.id...]]
;	   By default, either opIdLd... or opAIdSt... is emitted
;	   IDM_REF : causes opIdRf... or opAIdRf... to be emitted
;	   IDM_VTREF : causes opIdVtRf... or opAIdVtRf... to be emitted
;
;	For example:
;	   SHARED IdAry : IDM_INDEXED | IDM_AS [QB4]
;	   STATIC IdAryI : IDM_INDEXED | IDM_CONST | IDM_AS
;	   ERASE IdArray : IDM_ARRAY
;	   DIM IdAryDim : IDM_INDEXED | IDM_EXP | IDM_DIM | IDM_VTREF | IDM_AS
;	   PUT (10,20),IdAryGetPut : IDM_INDEXED | IDM_EXP | IDM_ARRAY
;	   FOR IdFor : IDM_REF
;	   INPUT IdAryElemRef : IDM_INDEXED | IDM_EXP | IDM_REF | IDM_ELEM
;	   NtExp() calls NtIdAryElem, which calls NtId with :
;	      IDM_INDEXED | IDM_EXP | IDM_ARGS
;
; Exit:
;	If id was indexed, fLastIdIndexed is TRUE, else it is FALSE
;	Returns PR_NotFound, PR_GoodSyntax or PR_BadSyntax in al.
;	Condition codes set based on value in al
;	If result is PR_GoodSyntax, bumps cIdArgs by 1, no matter what
;	   recursion takes place
;	For FV_SQL bx = oVar of emitted variable if variable ref was emitted.
;
;*********************************************************************

;word masks for 'flags'
IDM_INDEXED  EQU 0001H
IDM_CONST    EQU 0002H
IDM_EXP      EQU 0004H
IDM_OEXP     EQU 0008H
IDM_ARGS     EQU 0010H
IDM_DIM      EQU 0020H
IDM_ARRAY    EQU 0040H
IDM_REF      EQU 0080H
IDM_VTREF    EQU 0100H
IDM_ELEM     EQU 0200H
IDM_AS       EQU 0400H
IDM_NOSCALAR EQU 0800H	

;low-byte masks
IDM1_INDEXED  EQU 01H
IDM1_CONST    EQU 02H
IDM1_EXP      EQU 04H
IDM1_OEXP     EQU 08H
IDM1_ARGS     EQU 10H
IDM1_DIM      EQU 20H
IDM1_ARRAY    EQU 40H
IDM1_REF      EQU 80H

;high-byte masks
IDM2_VTREF    EQU 01H
IDM2_ELEM     EQU 02H
IDM2_AS       EQU 04H
IDM2_NOSCALAR EQU 08H	

cProc	NtId,<PUBLIC,NEAR>,<si,di>
	localW	maskW
	maskLO  EQU BYTE PTR (maskW)
	maskHI  EQU BYTE PTR (maskW+1)
	localV	tokId,%(size TOK)
	localW	opBase
	localW	flags
	flagsLO  EQU BYTE PTR (flags)
;Register usage:  di = oDstExp, si = cArgs
cBegin
	push	[cIdArgs]		;save caller's cIdArgs
	mov	[maskW],ax
	sub	al,al			;prepare to return PR_NotFound
	call	IdTok			;bx points to current token
	je	GotId			;brif its an id token
	jmp	NtIdExit		;brif not an id
GotId:
.erre opId_Ld EQ 0
	xor	ax,ax			;ax = opId_Ld
	test	[maskLO],IDM1_REF
	je	NotRefId		;brif not a REFERENCE id
	mov	ax,opId_Rf
	jmp	SHORT SetOpBase
NotRefId:
	test	[maskHI],IDM2_VTREF
	je	SetOpBase		;brif not a VarTab REFERENCE id
	mov	ax,opId_VtRf

;ax = opId_xxx (which class of opId to emit)
SetOpBase:
	mov	[opBase],ax
	;save important information about this token id.
	lea	bx,tokId
	call	CopyTokScanBx		;copy token [pTokScan] to [bx], ScanTok
					; using NtACTIONidDim
	test	[maskLO],IDM1_DIM
	je	NotDim			;brif id is not in DIM stmt
	;var mgr reports duplicate defn for DIM A: DIM A or DIM FNA
	or	[tokId.TOK_id_vmFlags],FVI_DIM
NotDim:
	sub	dx,dx			;default value for flags
	mov	al,[maskHI]
	and	al,IDM2_ELEM		;al = non-zero if .elem can follow id
	je	NoElem
	or	dl,FEM_ElemOk		;tell EmitVar that .elem is ok
NoElem:
	mov	[flags],dx
	test	[maskLO],IDM1_INDEXED
	je	NotIndexed		;brif (...) cannot follow id
	mov	ax,IRW_LParen
	call	TestScan_AX
	jne	TestNoScalar		
DJMP	jmp	SHORT IndexedId		; brif got an indexed id
TestNoScalar:				
	test	[maskHI],IDM2_NOSCALAR	
	jz	NotIndexed		
	mov	ax,IRW_LParen		
	call	PErrExpRw_Ax		
	jmp	SHORT J1_NtIdSnErr	
NotIndexed:
	test	[maskLO],IDM1_ARRAY
	je	NotUnindexedArray

	;got unindexed array reference like ERASE A
	lea	ax,[tokId]
	push	ax			;pass pointer to id's token
	push	[opBase]		;load/store/ref/vtref indicator
	PUSHI	ax,0			;arg count
	or	[flagsLO],FEM_Ary OR FEM_AryNoArgs
	push	[flags]
	call	EmitVar			;emit array id opcode
	jmp	SHORT ChkResult

;got scalar id reference
NotUnindexedArray:
	test	[maskHI],IDM2_AS
	je	NotAsScalar		;brif AS <type> is not allowed here
	sub	ax,ax
	lea	bx,[tokId]
	call	NtAsClause		;parse "AS <type>" clause
	jl	J1_NtIdSnErr		;brif result == PR_BadSyntax
NotAsScalar:
	mov	ax,[tokId.TOK_id_oNam]
	cmp	ax,[oNamConstPs]
	je	BadConst		;brif self-referencial CONST x=x
					; this is needed in case we're in
					; a procedure, and there is a global
					; level CONST x=n stmt.
	lea	ax,[tokId]
	push	ax			;pass ptr to token's id
	push	[opBase]		;load/store/ref indicator
	PUSHI	ax,0			;cArgs = 0
	push	[flags]
	call	EmitVar			;emit a scalar id opcode

;condition codes = result of calling EmitVar
ChkResult:
	jc	J1_NtIdSnErr		;brif bad syntax
	mov	[fLastIdIndexed],FALSE	;tell caller last id was not indexed
	cmp	[oNamConstPs],0
	je	J1_NtIdEnd		;brif not in CONST a=<expression> stmt
	test	[psFlags],PSIF_fBindVars
	je	J1_NtIdEnd		;brif parser not binding variables
					;rude scanner will check for this error
	TESTM	mkVar.MKVAR_flags2,MV_fConstFound	
	jne	J1_NtIdEnd		;brif CONST id = constant
					;else got CONST id = variable
BadConst:
	mov	ax,MSG_InvConst		;Error: Invalid Constant
	call	PErrMsg_AX		; al = PR_BadSyntax
J1_NtIdSnErr:
	jmp	SHORT J2_NtIdSnErr

J1_NtIdEnd:
	jmp	NtIdEnd

;got an indexed id
IndexedId:
	cmp	[oNamConstPs],0
	jne	BadConst		;brif in CONST a=<expression> stmt
					;can't have CONST a=z(i)
	or	[flagsLO],FEM_Ary	;tell EmitVar this is an array/func
	sub	si,si			;cArgs = 0
	call	ScanTok			;skip past left paren

	;Now that we've consumed at least 1 token, we can only return
	;PR_GoodSyntax or PR_BadSyntax
	
	test	[maskLO],IDM1_CONST
	je	NoOptConst		;brif not looking for X(literal) syntax
					; like COMMON x(3) [QB5] or STATIC x(2)
	call	NtLitI2NoCode		;scan optional integer (ignore value)
	jl	J2_NtIdSnErr		;brif result == PR_BadSyntax
NtIdNoArg:
DJMP	jmp	SHORT NtIdGetRParen	

NoOptConst:
	TESTM	maskW,<IDM_EXP OR IDM_OEXP>	
	je	NtIdNoArg
;scan arguments/indicies, can have a number of arguments inside ()
NtIdArgLoop:
	mov	di,[ps.PS_bdpDst.BDP_cbLogical] ;di = oDstExp
.erre NTEL_ARGS EQ IDM_ARGS
	mov	ax,[maskW]		
	call	NtExprOrArg		
	jl	J2_NtIdSnErr		;brif result == PR_BadSyntax
	je	NtIdEndArgs		;brif result == PR_NotFound
BumpCArgs:
	inc	si			;bump cArgs
	test	[maskLO],IDM1_DIM
	je	GetComma

	;each index to DIM represents 2 scanner arguments, but 1
	;arg to MakeVariable.  We communicate this to
	;EmitVar() by passing FEM_AryDim in [flags]
	
	or	[flagsLO],FEM_AryDim
	mov	ax,IRW_TO
	call	TestScan_AX
	jne	NoToClause		;brif didn't get TO
	call	ScanTok			;skip TO
	call	NtConsumeExp		;consume high index
	jge	GetComma		;brif result != PR_BadSyntax
J2_NtIdSnErr:
	jmp	short NtIdSnErr		;brif syntax error

;default lower bound
NoToClause:
	PUSHI	ax,<DATAOFFSET ps.PS_bdpDst>
	push	di			;pass oDstExp
	PUSHI	ax,2
	call	BdShiftRight
	or	ax,ax
	jne	StoreOpBase
	call	ParseErrOm		;Error "Out of memory"
	jmp	SHORT GetComma

;store opDimOptionBase opcode in pcode buffer
StoreOpBase:
	add	[ps.PS_bdpDst.BDP_pbCur],2
	mov	bx,[ps.PS_bdpDst.BDP_pb]
	add	bx,di			;add in oDstExp
	mov	WORD PTR [bx],opDimOptionBase
GetComma:
	mov	ax,IRW_Comma
	call	TestScan_AX
	jne	NtIdEndArgs		;brif current token is not comma
	cmp	si,MAXDIM
	jae	NtIdEndArgs		;BASCOM can't handle more than 60
					; args, so we shouldn't either
	call	ScanTok			;skip comma
	jmp	NtIdArgLoop		;get next arg

NtIdEndArgs:
	or	si,si			;test cArgs
	jne	NtIdGetRParen		;brif got more than 1 index
	test	[maskLO],IDM1_EXP
	je	NtIdGetRParen		;brif we don't need any expressions
	call	PErrExpExpr		;error "Expected expression"
	jmp	SHORT NtIdExit		;return PR_BadSyntax

NtIdGetRParen:
	mov	ax,IRW_RParen
	call	ConsumeRw_AX		;consume ')'
	jc	NtIdSnErr		;brif syntax error
	test	WORD PTR [maskHI],IDM2_AS
	je	NotAryAs		;brif not expecting AS clause

	;check for AS clause
	sub	ax,ax
	lea	bx,[tokId]
	call	NtAsClause		;parse AS <type>
	jge	NotAryAs		;brif result != PR_BadSyntax
NtIdSnErr:
	mov	al,PR_BadSyntax
	jmp	SHORT NtIdExit

NotAryAs:
	lea	ax,[tokId]
	push	ax
	push	[opBase]
	push	si			;pass cArgs
	push	[flags]
	call	EmitVar			;emit pcode for array ref
	jc	NtIdSnErr		;brif syntax error
	mov	[fLastIdIndexed],TRUE
NtIdEnd:
	;this is (and must remain) the only exit point for PR_GoodSyntax
	mov	al,PR_GoodSyntax
NtIdExit:
	;Can't just bump cIdArgs, recursive calls to NtExp could have
	;bumped it, making it useless.
	
	pop	dx			;dx = caller's cIdArgs
	inc	dx
	or	al,al			;set condition codes for caller
	jle	NtIdExit1		;brif result != PR_GoodSyntax
	mov	[cIdArgs],dx		;bump cIdArgs
NtIdExit1:
cEnd


;*********************************************************************
; CopyTokScanBx
; Purpose:
;	Copy important fields from one token descriptor to another.
;	Then call ScanTok to advance to next token.
; Entry:
;	[pTokScan] points to source token
;	bx points to destination token
;
;*********************************************************************
PUBLIC	CopyTokScanBx
CopyTokScanBx PROC NEAR
	push	[pTokScan]		;pass pbSrc
	push	bx			;pass pbDst
	PUSHI	ax,<size TOK>		;pass byte count
	call	CopyBlk			;copy the token descriptor
	jmp	ScanTok			;get next token, then return to caller
CopyTokScanBx ENDP

;*********************************************************************
; PARSE_RESULT NEAR NtIdAryDim()
;
; Purpose:
;	Try to parse a scalar or array element of the form:
;	   id[<type>] [(exp [TO exp], ... )] [AS <type>]
;	This can occur in the statement  DIM IdAryDim ...
;
; Exit:
;	Returns PR_NotFound, PR_GoodSyntax or PR_BadSyntax
;
;*********************************************************************
PUBLIC	NtIdAryDim
NtIdAryDim PROC NEAR
	mov	ax,IDM_INDEXED OR IDM_EXP OR IDM_DIM OR IDM_VTREF OR IDM_AS
	jmp	NtId			

NtIdAryDim ENDP

;*********************************************************************
; PARSE_RESULT NEAR NtIdAryRedim()
;
; Purpose:
;	Try to parse a scalar or array element of the form:
;	   id[<type>] [(exp [TO exp], ... )] [AS <type>]
;	This can occur in the statement  REDIM IdAryDim ...
;
; Exit:
;	Returns PR_NotFound, PR_GoodSyntax or PR_BadSyntax
;
;*********************************************************************
PUBLIC	NtIdAryRedim
NtIdAryRedim PROC NEAR
	mov ax,IDM_INDEXED OR IDM_EXP OR IDM_DIM OR IDM_AS OR IDM_NOSCALAR
	call	NtId
	jle	AryDimExit		;brif result != PR_GoodSyntax
	cmp	[fLastIdIndexed],FALSE
	je	AryDimExit		; Brif NtId emitted a scalar
	mov	ax,opStRedimTo
	call	Emit16_AX
	mov	al,PR_GoodSyntax
AryDimExit:
	or	al,al			; Set flags for caller
	ret	
NtIdAryRedim ENDP

;*********************************************************************
; PARSE_RESULT NEAR NtImpliedLetOrCall(fCantBeCall)
;
; Purpose:
;	Parse implied LET or CALL or TYPE/AS statement
;	   id = exp
;	   id [argList]
;	   id AS (INTEGER | LONG | SINGLE | DOUBLE | STRING * const | id)
;	A complicated example is:
;	   a(x).b=c(y).d which produces:
;	opIdLd(y) opAIdRf(1,c) opOffLd(d) opIdLd(x) opAIdRf(1,a) opOffSt(b)
;
; Entry:
;	It is assumed that pTokScan points to an id token
;	fCantBeCall is TRUE if it MUST be a LET statement
;
; Exit:
;	Never returns PR_NotFound, only PR_GoodSyntax or PR_BadSyntax
;	Condition codes set based on value in al
;
;*********************************************************************
cProc	NtImpliedLetOrCall,<PUBLIC,NEAR>,<si,di>
	parmW	fCantBeCall		;can't be parmB because some caller's
					; do a push sp to pass TRUE
	localB	fGotScalar
	localW	cArgs
	;reg si = oDstLvalStart
	localW	oDstLvalEnd
	localV	tokId,%(size TOK)
	localW	cbShift
cBegin
	call	Peek1Tok		;look at token past id
	mov	ax,IRW_AS
	call	TestPeek_AX
	jne	NotAsType1		;brif id not followed by AS

	;got  id AS type  statement, presumably within TYPE/END TYPE block
	call	IdTokNoPeriodImp	;parse id token with no period in it
					;error if not implicit
	jl	J1_LetExit		;brif PR_BadSyntax
	lea	bx,tokId
	call	CopyTokScanBx		;copy [pTokScan] to [bx], ScanTok
	mov	ax,opElemRef
PrsAsClause:
	call	Emit16_AX
	mov	ax,[tokId.TOK_id_oNam]	;ax = oNam of variable in as clause
	call	Emit16_AX

	;now parse 'AS <type>' and emit opAsType(oNam)
; if FV_FARSTR is TRUE but that would be nearly unreadable
	mov	al,FAS_fNoVarLenStr + FAS_fDontBind + FAS_fDontSetONam
					;AS STRING is not allowed within
					; a TYPE stmt.  AS STRING * N is
					;Don't call RefType(x) for id as x
	lea	bx,tokId
	call	NtAsClause		;parse "AS <type>" clause
J1_LetExit:
	jmp	LetExit			

NotAsType1:
	lea	bx,tokId
	call	CopyTokScanBx		;copy [pTokScan] to [bx], ScanTok
	mov	si,[ps.PS_bdpDst.BDP_cbLogical]
					;si = oDstLvalStart]
	mov	[fGotScalar],TRUE
	call	NtExprList		;try to parse "(exp, exp, ..., exp)"
	mov	[cArgs],cx		;record number of args found
	je	LetNotAry		;brif not found
	jl	J1_LetExit		;brif syntax error
LetGotAry:
	mov	[fGotScalar],FALSE
	
LetNotAry:
	call	FElements		;see if cur token is record separator
	je	ItsALet			;brif we got .elem (it can't be CALL)
	cmp	cx,1			
	ja	ItsALet			;brif got x(y,...)... (can't be CALL)
	mov	ax,IRW_EQ
	call	TestScan_AX
	je	ItsALet			;brif got '=' (can't be CALL)
	jmp	ImpliedCall		;brif didn't get an implied LET stmt

;we're looking at an implied LET statement, not an implied CALLstatement
ItsALet:
	or	[tokId.TOK_id_vmFlags],FVI_LVAL
	cmp	[fGotScalar],FALSE
	je	LetAry
	lea	ax,[tokId]
	push	ax			;pass ptr to id's token
	PUSHI	ax,opId_St		;emit a Store id variant
	PUSHI	ax,0			;cArgs = 0
	PUSHI	ax,FEM_ElemOk		;.elem may follow variable
	jmp	SHORT LetEmitVar

;lvalue is an array element
LetAry:
LetAryGood:
	lea	ax,[tokId]
	push	ax
	PUSHI	ax,opId_St		;emit a Store id variant
	push	[cArgs]			;arg count
	PUSHI	ax,<FEM_Ary OR FEM_ElemOk> ;.elem may follow variable
LetEmitVar:
	call	EmitVar			;emit an array store opcode
	jc	LetSnErr		;brif EmitVar got error
	mov	ax,[ps.PS_bdpDst.BDP_cbLogical]
	mov	[oDstLvalEnd],ax	;save offset to end of lval's pcode
	mov	ax,IRW_EQ
	call	ConsumeRw_AX		;parse '='
	jc	LetSnErr		;brif error
	call	NtConsumeExp		;parse an expression
	jl	LetSnErr		;brif result == PR_BadSyntax
	mov	ax,[ps.PS_bdpDst.BDP_cbLogical]
	sub	ax,[oDstLvalEnd]
	mov	[cbShift],ax

	;for assignments like A(X,Y)=Z, the pcode buffer contains:
	;           <id(X)> <id(Y)> <id(A)> <id(Z)>
	;         1^                      2^      3^
	; where 1^=oDstLvalStart, 2^=oDstLvalEnd, 3^=cbLogical
	; The next few lines swap argument and expression pcode to be:
	;             <id(Z)> <id(X)> <id(Y)> <id(A)>
	
	PUSHI	ax,<DATAOFFSET ps.PS_bdpDst>
	push	si
	push	[cbShift]
	call	BdShiftRight
	or	ax,ax
	jne	RightOk
	call	ParseErrOm		;Error "Out of memory"
	jmp	SHORT J1_LetGoodSyntax

RightOk:
	mov	ax,[oDstLvalEnd]
	add	ax,[cbShift]
	add	ax,[ps.PS_bdpDst.BDP_pb] ;ax points to source
	push	ax
	xchg	ax,si			;ax = oDstLvalStart
	add	ax,[ps.PS_bdpDst.BDP_pb] ;ax points to destination
	push	ax
	push	[cbShift]		;pass byte count
	call	CopyBlk
	mov	ax,[cbShift]
	sub	[ps.PS_bdpDst.BDP_cbLogical],ax
J1_LetGoodSyntax:
	jmp	SHORT LetGoodSyntax


InvalidCall:
	mov	ax,MSG_ExpAssignment	;Error "Expected var=expression"

LetExpErr:
	call	PErrExpMsg_AX
	;tell user-interface what column error really occured in
	mov	ax,[tokId.TOK_oSrc]
	mov	[ps.PS_oSrcErr],ax
LetSnErr:
	mov	al,PR_BadSyntax
	jmp	SHORT LetExit		


;we're looking at an implied CALL statement, not an implied LET statement
;If [cArgs] == 1, we've already consumed the 1st argument, because we
;   weren't sure if it was an array lval.
;   idProc ( expression ) [, arg2 [, arg3 ...]]
;          current token ^
;If [cArgs] == 0, we've just consumed the idProc
;            idProc [arg1 [, arg2 ...]]
;    current token ^
;
ImpliedCall:
	cmp	[fCantBeCall],FALSE
	jne	InvalidCall		;brif implied CALL is invalid here
	cmp	[tokId.TOK_id_oTyp],ET_IMP
	jne	InvalidCall		;brif sub id is explicitly typed
	cmp	[cArgs],0
	je	CallArg1		;brif no args consumed yet

; We've got IdProc (arg) [, ...] --- 
GotPassByVal:
	mov	ax,opLParen		;already consumed 1st arg,
	call	Emit16_AX		;it was a parend expression
	jmp	SHORT CallArg2		;get 2nd CALL arg (if any)

CallArgLoop:
	call	ScanTok			;skip ','
	call	NtIdCallArg		;consume 1st arg (if any)
	jg	CallArgNext		;brif result is PR_GoodSyntax
	jl	J4_LetSnErr		;brif result == PR_BadSyntax
	call	PErrExpExpr		;error "Expected expression"
J4_LetSnErr:
	jmp	LetSnErr		;return al = PR_BadSyntax

;so far, we've just seen idProc.  Try to parse 1st arg.
;
CallArg1:
	call	NtIdCallArg		;consume 1st arg (if any)
	je	CallArgEnd		;brif no args
CallArgNext:
	inc	[cArgs]			;bump arg count
	cmp	[cArgs],MAXARG		
	jae	CallArgEnd		;don't parse more args if got max
					; this will generate "expected EOL"
CallArg2:
	mov	ax,IRW_Comma
	call	TestScan_AX
	je	CallArgLoop		;brif got a comma
CallArgEnd:
	mov	ax,opStCallLess
CallEmitOp:
	call	Emit16_AX		;emit opcode
	mov	ax,[cArgs]
	call	Emit16_AX		;emit arg count
	mov	ax,[tokId.TOK_id_oNam]	;ax = oNam for sub
	call	SubRef			;ax = oPrs for sub
	jc	LetGoodSyntax		;brif couldn't define prs
					;ps.errCode set, so pcode won't be
					;emitted - line will be opReParse

	call	Emit16_AX		;emit oPrs for sub (parm pushed above)
LetGoodSyntax:
	mov	al,PR_GoodSyntax
LetExit:
	or	al,al			;set condition codes for caller
cEnd

;*********************************************************************
; PARSE_RESULT NEAR NtIdNamCom()
;
; Purpose:
;	Try to parse an identifier of the form:  id <with no explicit type>
;	The id can have a period, regardless of 'x AS' elsewhere in module.
;	This can occur in the following statements:
;	   COMMON [/IdNamCom/] ...
;
; Exit:
;	If good syntax, Emits id's 16-bit oNam and returns PR_GoodSyntax
;	Otherwise, an error is generated and PR_BadSyntax is returned.
;	Never returns PR_NotFound because no callers have other options
;
;*********************************************************************
PUBLIC	NtIdNamCom
NtIdNamCom PROC NEAR
	call	IdTokPeriodImp1
	jmp	SHORT NtIdImp1
NtIdNamCom ENDP

;*********************************************************************
; PARSE_RESULT NEAR NtIdType()
;
; Purpose:
;	Try to parse an identifier of the form:  id <with no explicit type>
;	The id can have no periods.
;	This can occur in the following statements:
;	   DIM a AS IdType
;	   TYPE IdType
;	     Id AS IdType  'element definition in TYPE block
;
; Exit:
;	If good syntax, Emits id's 16-bit oNam and returns PR_GoodSyntax
;	If token is not an id, returns PR_NotFound
;	Else PR_BadSyntax after generating error
;
;*********************************************************************
PUBLIC	NtIdType
NtIdType PROC NEAR
	call	IdTok
	mov	al,PR_NotFound
	jne	NtIdImpExit		;brif not an id token
	call	IdTokNoPeriodImp	;parse id token with no period in it
					;error if not implicit
NtIdImp1:
	jl	NtIdImpExit		;brif PR_BadSyntax
;Jumped to from other procedures
NtIdImp2:
	mov	ax,[bx.TOK_id_oNam]
	call	Emit16_AX		;emit id's oNam
	call	ScanTok			;skip id
	mov	al,PR_GoodSyntax
NtIdImpExit:
	or	al,al			;set condition codes for caller
	ret	
NtIdType ENDP



;*========================================================================
;	               Procedure related ID nonterminals
;=========================================================================

;*********************************************************************
;UndoLogPrs
;Purpose:
;	Remember to free prs entry if we turn this line into a reparse
;	We only log it if the prs in question has no other references.
;	If we called UndefPrs for a prs that had a reference, then
;	a DECLARE that followed a SUB could end up looking like a stronger
;	reference than the SUB, which would cause problems.
;Entry:
;	ax = oPrs in question
;Exit:
;	ax is preserved as oPrs
;
;*********************************************************************
UndoLogPrs PROC NEAR
	push	ax			;save caller's ax
	call	PPrsOPrs		;bx = ptr to prs, ax still=oPrs
	xchg	dx,ax			;dx = oPrs (for ParseUndoLog)
	mov	ax,PTRRS[bx.PRS_otxDef] 
	inc	ax			;test for UNDEFINED
	jne	AlreadyRefed		;brif this prs already has a ref
	mov	al,PUNDO_oPrsRef	;pass entry type in al
	call	ParseUndoLog		;remember to free prs entry if
					; we turn this line into a reparse
AlreadyRefed:
	pop	ax			;restore caller's ax
	ret
UndoLogPrs ENDP


;*********************************************************************
; PARSE_RESULT NEAR NtIdSubRef
;
; Purpose:
;	Try to parse an identifier of the form:  id <with no explicit type>
;	This can occur in the following statements:
;	   CALL IdSubRef ...,  CALLS IdSubRef ...,
;
; Exit:
;	If good syntax, Emits id's 16-bit oNam and returns PR_GoodSyntax
;	Otherwise, an error is generated and PR_BadSyntax is returned.
;	Never returns PR_NotFound because no callers have other options
;
;*********************************************************************
PUBLIC	NtIdSubRef
NtIdSubRef PROC NEAR
	call	IdSub
	jge	NtIdImp2		;brif not PR_BadSyntax, emit pcode
NtRet1:
	ret
NtIdSubRef ENDP

;*********************************************************************
; NtIdFuncDef, NtIdFuncDecl, NtIdFn [QB4]
;
; Purpose:
;	Try to parse a procedure id of the form:
;	   [FN]id[type]
;	This can occur in the following statements:
;	   DECLARE FUNCTION IdFuncDecl ...
;	   FUNCTION IdFuncDef ...
;	   DEF IdFn ... [QB4]
;
; Exit:
;	If this is found, the token is consumed, the parm's oNam and
;	   16 bit procedure-attribute-mask are emitted, and the return
;	   value is PR_GoodSyntax.
;	Otherwise, an error is generated and PR_BadSyntax is returned.
;	Never returns PR_NotFound because no callers have other options
;
;********************************************************************
PUBLIC	NtIdFuncDecl
NtIdFuncDecl PROC NEAR
	sub	ax,ax			;id can't begin with FN
	call	IdTokFn
	jl	NtRet1			;brif bad syntax
	mov	ax,0FF00h + PT_FUNCTION
	jmp	SHORT MakeProcFunc
NtIdFuncDecl ENDP

PUBLIC	NtIdFuncDef
NtIdFuncDef PROC NEAR
	sub	ax,ax			;id can't begin with FN
	call	IdTokFn
	jl	NtRet1			;brif bad syntax
	mov	ax,PT_FUNCTION
MakeProcFunc:
	mov	dx,FVI_FUNCTION
	jmp	SHORT MakeProc
NtIdFuncDef ENDP

; Invoked for DEF FNx statement
PUBLIC	NtIdFn
NtIdFn PROC NEAR
	mov	ax,FVI_FNNAME		;id needs to begin with FN
	call	IdTokFn			;bx=token, dl=oTyp
	jl	NtRet1			;brif bad syntax
	mov	ax,PT_DEFFN
	sub	dx,dx
	jmp	SHORT MakeProc
NtIdFn ENDP

;*********************************************************************
; PARSE_RESULT NEAR NtIdSubDef, NtIdSubDecl
;
; Purpose:
;	Try to parse an identifier of the form:  id <with no explicit type>
;	This can occur in the following statements:
;	   SUB IdSubDef ...
;	   Implied CALL gets fixed up in NtImpliedLetOrCall
;	   DECLARE SUB IdSubDecl ...
;
; Exit:
;	If good syntax, Emits id's 16-bit oNam and returns PR_GoodSyntax
;	Otherwise, an error is generated and PR_BadSyntax is returned.
;	Never returns PR_NotFound because no callers have other options
;
;*********************************************************************
PUBLIC	NtIdSubDef
NtIdSubDef PROC NEAR
	call	IdSub
	jl	NtRet1			;return if bad syntax
	mov	ax,PT_SUB
	jmp	SHORT MakeProcSub
NtIdSubDef ENDP

PUBLIC	NtIdSubDecl
NtIdSubDecl PROC NEAR
	call	IdSub
	jl	NtRet1			;return if bad syntax
	mov	ax,0FF00h + PT_SUB
MakeProcSub:
	sub	dx,dx
NtIdSubDecl ENDP
	;fall into MakeProc
;*********************************************************************
; MakeProc
; Purpose:
;	Calls PrsDefine which will create a Prs for the procedure if one does
;	not yet exist.
;	If it is called for a function then it sets up a variable in the
;	module's variable table for the function.
;	Ensures table is in RUDE state.
; Entry:
;	al = procType
;	ah = non-zero if DECLARE
;	dx = mkVar.flags value for procedure (FVI_FUNCTION/0)
;	pTokScan points to id token for [DECLARE] SUB/FUNCTION/DEF id
; Exit:
;	token is consumed
;	al = PR_GoodSyntax or PR_BadSyntax if DECLARE or DEF FN stmt 
;		is being added into a procedure or out of memory, etc.
;	[pdcl.PDCL_cParms] = 0
;	[pdcl.PDCL_oPrs] = oPrs of procedure
;	If called for other than a DECLARE stmt then PrsCur and MrsCur
;	hold the Prs/Mrs of the procedure.
;
;*********************************************************************
cProc	MakeProc,<NEAR>
	localW	oMrsSave
	localW	oPrsSave
cBegin
	mov	WORD PTR ([pdcl.PDCL_procType]),ax
					;save procType, fDeclare flags
	mov	[mkVar.MKVAR_flags],dx	;setup for call to BindVar
	push	ax
	call	EnsRude			;make sure table is in SS_RUDE state
	pop	ax
	jnz	J1_MpExit		;exit if backout or retry
	test	[mrsCur.MRS_flags2],FM2_Include
	jne	ItsAnIncl		;brif we're looking at an INCLUDE file
	cmp	[cInclNest],0
	je	NotInInclude		;brif not loading an $INCLUDE file
	.errnz	PT_SUB - 1
	.errnz	PT_FUNCTION - 2
	.errnz	PT_DEFFN - 3
ItsAnIncl:
	cmp	ax,PT_FUNCTION
	ja	NotInInclude		;brif DECLARE or DEF FN
	mov	ax,MSG_InvIncl		;Error Stmt illegal within $INCLUDE file
	jmp	SHORT AlertErr

NotInInclude:
	mov	[pdcl.PDCL_cParms],0	;init parm count

	;Check for illegal SUB/FUNCTION/DEF FN/DECLARE Scoping:
	;if (opcode is opStDefFn or opStDeclare)
	;   If (we're within a SUB or FUNCTION)
	;      Issue  Illegal in SUB/FUNCTION error
	;else /* opcode is opStSub or opStFunction */
	;   if ( we're within a SUB or FUNCTION and SUB/FUNCTION is not defined)
	;      PrsDeactivate	      /* so new prs will be created */
	
	cmp	ax,PT_SUB
	je	GotSubFunc		;brif SUB stmt (not DECLARE SUB)
	cmp	ax,PT_FUNCTION
	je	GotSubFunc		;brif FUNCTION stmt

; user is entering a DECLARE, or DEF FN statement
NotSubFunc:
	test	[txdCur.TXD_flags],FTX_mrs
	jne	NoScopeErr		;brif not currently in a SUB/FUNCTION

;  user is attempting to enter a  DEF FN or DECLARE stmt in SUB/FUNC
;
InvProcErr:
	mov	ax,MSG_InvProc		;Error: stmt illegal in SUB/FUNCTION

;ax = standard qbi error code
;pTokScan points to id token
;
AlertErr:
	or	ah,PSERR_fAlert / 100h	;set parser's Alert flag
	call	ParseErrTokScan
J1_MpExit:
	jmp	MpExit

;user is attempting to define a SUB or FUNCTION
GotSubFunc:
	; We are in a SUB or function, and there is no current
	; definition for the SUB, then call PrsDeactivate so that a
	; new SUB/FUNCTION will be created.  Otherwise, this will be
	; treated as a rename of a SUB/FUNCTION.
	
	test	[txdCur.TXD_flags],FTX_mrs
	jne	NoScopeErr		;brif not currently in a SUB/FUNCTION
					; i.e. user is attempting to enter a
					; SUB or FUNCTION stmt in main prog
;Check to see if we are currently merging into a sub. If we
;are then generate an error. 
	fLoadActive			
	jz	NotLoading		;brif not loading
	cmp	[fMergeInSub],0		
	jnz	InvProcErr		;brif merging into a Sub
NotLoading:				
	test	[prsCur.PRS_flags],FP_DEFINED
	je	NoScopeErr		;brif SUB/FUNCTION is not already
					; defined. Rename the SUB/FUNCTION

	; We are in a SUB/FUNCTION with a definition.  Call PrsDeactivate
	; to force the creation of a NEW SUB/FUNCTION by PrsDefine

	call	PrsDeactivate		;get back to main program's text table

;PrsDefine(oNam, procType, oTyp, fDeclare)
NoScopeErr:
	mov	bx,[pTokScan]
	mov	ax,[bx.TOK_id_oNam]
	mov	[pdcl.PDCL_oNam],ax
	mov	[mkVar.MKVAR_oNam],ax	;setup for call BindVar
	mov	ax,[bx.TOK_id_oTyp]	;ax = explicit oTyp
	mov	dx,ax			;dx = ax = oTyp
	or	dl,DCLA_Explicit
	mov	cx,WORD PTR([pdcl.PDCL_procType]) ;cl = procType, ch=fDeclare
	cmp	cl,PT_SUB
	je	ImplicitType		;brif [DECLARE] SUB
	or	ax,ax			;test for ET_IMP
	.errnz	ET_IMP
	jne	ExplicitType
	mov	al,[bx.TOK_id_charFirst]
	sub	ah,ah			;ax = 0..25 for A..Z
	push	bx
	mov	bx,ax			;can't use xchg, need ah=0
	mov	al,[ps.PS_tEtCur+bx]	;ax = default oTyp
	pop	bx			;bx points to id token
ImplicitType:
	sub	dx,dx			;dl = ET_IMP
	.errnz	ET_IMP
ExplicitType:
	mov	[mkVar.MKVAR_oTyp],ax	;oTyp to pass to BindVar (never ET_IMP)
	mov	[pdcl.PDCL_oTyp],dl	;oTyp to emit in pcode

	cmp	cl,PT_DEFFN
	jne	NotDefFn		;brif not DEF FN stmt
	cmp	[txdCur.TXD_scanState],SS_RUDE
	jne	NotDefFn
	mov	[pdcl.PDCL_oPrs],UNDEFINED
	DJMP	jmp SHORT MpExit	;bind DEF FNs to oNam in SS_RUDE
					; to oPrs in other scan states
NotDefFn:

;bx = ptr to token, cl = procType, al = oTypFn
	push	[bx.TOK_id_oNam]	;pass oNam
	push	cx			;pass procType (byte parm)
	push	ax			;pass oTyp (byte parm)
	push	WORD PTR([pdcl.PDCL_fDeclare])	;pass fDeclare (byte parm)
	mov	ax,[grs.GRS_oMrsCur]	;save current mrs & prs
	mov	[oMrsSave],ax		; so we can restore context on exit
	mov	ax,[grs.GRS_oPrsCur]	; for DECLARE
	mov	[oPrsSave],ax
	call	PrsDefine
	or	ax,ax			
	je	PrsDefined		;brif no error
	jmp	AlertErr

PrsDefined:
	;procedure has been defined or declared and made active
	
	mov	ax,[grs.GRS_oPrsCur]	;dx = oPrs for proc
	mov	[pdcl.PDCL_oPrs],ax
	call	UndoLogPrs		;remember to free prs entry if
					; we turn this line into a reparse
					; ax is preserved as oPrs

	;Even if oPrsSave == oPrsNew, we call WnReAssign in case user changed
	;from a SUB to a FUNCTION, so if DrawDebugScr will be called to
	;change all EXIT/END SUB statements to EXIT/END FUNCTION.
	;Otherwise, what is in pcode is inconsistent with what is on the screen.
	;(calling DrawDebugScr is cheap enough to do it sometimes when we
	; don't really need to)
	;;cmp	[oPrsSave],ax
	;;je	NoRename

	cmp	[oPrsSave],UNDEFINED
	je	NoRename
	mov	ax,WORD PTR([pdcl.PDCL_procType]) ;al=procType, ah=fDeclare
	cmp	ax,PT_SUB
	je	RenameProc		;brif SUB stmt
	cmp	ax,PT_FUNCTION
	jne	NoRename
RenameProc:
	;--------------------------------------------------------
	;user is editing SUB/FUNCTION Foo, and has changed its
	;name to Bar.  PrsDefine created a new prs for this case,
	;and moved the text table over to the new prs.
	;We must now find a new definition for prs Foo (i.e.
	;set its prsCur.otx field to another DECLARE,
	;or PrsFree Foo's prs entry if no references exist for it.
	;--------------------------------------------------------
	;tell user interface to change title and oPrs of procedure's window
	mov	ax,[oPrsSave]
	or	ah,80H			;tells WnReAssign this is oPrs, not oMrs
	push	ax			;pass oRsSave
	push	[grs.GRS_oRsCur]	;pass oRs for new prs
	push	sp			;just renaming an oRs-don't move cursor
	call	WnReAssign

	mov	ax,[oPrsSave]
	call	UndefPrs		;tell txtmgr we deleted "defining" ref
;call ChkAllUndefPrs here because if we wait until we return to TxtChange,
;the Prs for "sub2" in the stmt "Sub sub1 : call sub2" will get discarded
;after parsing since its only reference is in the parser buffer.
	call	ChkAllUndefPrsSaveRs	;find new ref to the old Prs or
					; discard it if there aren't any
NoRename:
	cmp	[pdcl.PDCL_procType],PT_SUB
	je	DeclSub			;brif [DECLARE] SUB stmt

DbAssertRel mkVar.MKVAR_oTyp,be,ET_MAX,CP,<invalid mkVar.oTyp in CgDeclare>
; Let variable manager know about this DEF/FUNCTION
; Note that BindVar (and MakeVariable) don't care about the settings of
; FVI_INDEXED or mkVar.cDimensions if FVI_FUNCTION is set or the id begins with FN.
;
NoParms:
DeclSub:
	cmp	[pdcl.PDCL_fDeclare],FALSE
	je	MpExit			;brif not DECLARE stmt
	push	[oMrsSave]		;restore old mrs/prs
	call	MrsActivateCP
	push	[oPrsSave]
	call	PrsActivateCP
MpExit:
	call	ScanTok			;consume id token
	mov	al,PR_GoodSyntax
cEnd

;*********************************************************************
; NtIdParm()
;
; Purpose:
;	Try to parse a formal parameter of the form:
;	   [BYVAL | SEG] id [([constant]) [AS <typeId>]]
;	This can occur in the DECLARE, DEF FN, FUNCTION, and SUB
;	   statement's formal parameter lists.
; Entry:
;	[pdcl.PDCL_procType], [pdcl.PDCL_fDeclare] and [pdcl.PDCL_cParms]
;	  have been setup by NtIdFuncDecl, NtIdSubDecl, NtIdFn [QB4],
;	  NtIdFuncDef or NtIdSubDef.
; Exit:
;	If this is found, the tokens are consumed, the parm's oNam,
;	   16 bit formal-parm-attribute-mask, and oTyp are emitted,
;	   and the return value is PR_GoodSyntax.
;	Otherwise, the return value is PR_NotFound or PR_BadSyntax.
;
;********************************************************************
cProc	NtIdParm,<PUBLIC,NEAR>,<si,di>
	localW	pTokId
	localW	oNamParm
	;reg si oTypOrONam
	;reg di parmAtrMask
cBegin
	;following constants from different header files must be same
	DbAssertRel FVI_ARRAY,e,PATR_array,CP,<FVI_ARRAY != PATR_array>
	DbAssertRel FVI_ASCLAUSE,e,PATR_asClause,CP,<FVI_ASCLAUSE != PATR_asClause>

	cmp	[pdcl.PDCL_cParms],MAXARG 
	jb	NtIdOK			
	call	ExpRParenLastToken	;"expected ')'" on prev token
	jmp	SHORT J1_NtParmExit	
NtIdOK:
	sub	di,di			;parmAtrMask = 0
	cmp	[pdcl.PDCL_fDeclare],0
	je	NotSegParm		;brif SEG/BYVAL not allowed -not DECLARE
	mov	ax,IRW_ByVal
	call	TestScan_AX
	jne	@F			;brif token isn't BYVAL
	mov	di,PATR_BYVAL		;remember that BYVAL was seen

	jmp	SHORT GotSegByVal
@@:
	mov	ax,IRW_SEG
	call	TestScan_AX
	jne	@F			;brif token isn't SEG
	mov	di,PATR_SEG		;remember that SEG was seen
GotSegByVal:
	call	ScanTok			;consume SEG/BYVAL token
@@:
NotSegParm:
	call	IdTok			;bx points to current token
	je	GotIdParm		;brif its an id token
NIPError:
	sub	ax,ax			;prepare to return PR_NotFound
	or	di,di			;test parmAtrMask
	je	J1_NtParmExit		;brif no tokens have been consumed
	;we've already consumed BYVAL or SEG, it is a syntax error
	call	PErrExpId		;Expected id, al = PR_BadSyntax
J1_NtParmExit:
	jmp	NtParmExit

GotIdParm:
	test	[bx.TOK_id_vmFlags],FVI_FNNAME 
	jnz	NIPError		;brif parameter is FNxxx

	mov	[pTokId],bx		;pTokId = pTokScan
	mov	ax,[bx.TOK_id_oNam]	;ax = parm's oNam
	mov	[oNamParm],ax
	mov	si,[bx.TOK_id_oTyp]	;si = parm's oTyp
.errnz	ET_IMP
	or	si,si
	je	ImplicitParm		;brif no explicit type
	or	di,PATR_explicit	;remember parm has explicit type
	jmp	SHORT EatId

ImplicitParm:
	cCall	OTypOfONamDefault,<ax>	; ax = default oTyp for oNam in ax
	xchg	si,ax			;si = oTyp
EatId:
	call	ScanTok			;skip past parm's id token
	mov	al,FAS_fNoUserType + FAS_fNoFixLenStr
	cmp	[pdcl.PDCL_procType],PT_DEFFN
	je	TryAsClause1		;DEF FNs can't have array parms
	test	di,PATR_BYVAL OR PATR_SEG
	jne	TryAsClause		;arrays can't be passed by value or seg
	mov	ax,IRW_LParen
	call	TestScan_AX
	jne	TryAsClause		;brif no '('
	call	ScanTok			;skip past '('
	call	NtLitI2NoCode		;scan optional integer (ignore value)
	jl	JL1_NtParmExit		;brif PR_BadSyntax
	mov	ax,IRW_RParen
	call	ConsumeRw_AX		;consume ')' token
	jc	J1_NtParmExit		;return PR_BadSyntax if not found
	or	di,PATR_array		;remember parm is an array
TryAsClause:
	mov	al,FAS_fNoFixLenStr
TryAsClause1:
	cmp	[pdcl.PDCL_fDeclare],0
	je	NotDeclare		;brif not DECLARE parm
	or	al,FAS_fAllowAny	;parm AS ANY is ok in DECLARE stmt
NotDeclare:
	mov	bx,[pTokId]
	call	NtAsClause		;parse "AS <type>" clause
JL1_NtParmExit:
	jl	NtParmExit		;brif result == PR_BadSyntax
	je	NotAsParm		;brif result == PR_NotFound

	;NtAsClause set pTokId's oTyp to type indicated by AS clause
	mov	bx,[pTokId]
	mov	si,[bx.TOK_id_oTyp]	;si = oTyp (set by NtAsClause)
	or	di,PATR_asClause

	; discard pcode emitted by NtAsClause
	sub	[ps.PS_bdpDst.BDP_cbLogical],6
	sub	[ps.PS_bdpDst.BDP_pbCur],6

;Now bind the parameter to an oVar if we're not in a DECLARE stmt
NotAsParm:
	mov	ax,[oNamParm]
	cmp	[pdcl.PDCL_fDeclare],0
	jne	EmitONam		;brif DECLARE parm
	mov	[mkVar.MKVAR_oNam],ax

	mov	ax,FVI_FORMAL
	test	di,PATR_array
	je	NotArray
	or	ax,FVI_INDEXED OR FVI_ARRAY
NotArray:
	test	di,PATR_asClause
	je	NotAsType2
	or	ax,FVI_ASCLAUSE
NotAsType2:
	mov	[mkVar.MKVAR_flags],ax
	mov	[mkVar.MKVAR_cDimensions],0
	mov	[mkVar.MKVAR_oTyp],si
	mov	ax,[pTokId]		;if error, report at this column
	call	BindVar			;call var mgr to bind variable
					;ax = oVar to emit
EmitONam:
	call	Emit16_AX		;Emit oNam or oVar
	xchg	ax,di			;Emit parmAtrMask
	call	Emit16_AX
	xchg	ax,si			;Emit16 oTypOrONam
	call	Emit16_AX
	inc	[pdcl.PDCL_cParms]	;bump parm count
	mov	al,PR_GoodSyntax
NtParmExit:
cEnd

;*********************************************************************
; PARSE_RESULT NEAR NtNArgs()
;
; Purpose:
;	Invoked to parse the arguments to statements like CLEAR, COLOR,
;	LOCATE, SCREEN where each positional parameter can is optional.   
;	Each expression is followed by opLit1 (to indicate to the executor that
;	this parameter was specified).  Each omitted expression results in
;	an opNull (to tell the executor that this parm was omitted).
;	Syntax error if last item is a comma and not expression.
;	0 or more arguments are allowed.
;	Expected syntax:  [[exp], [[exp] , ... ]]
;	The Finite State Machine for this syntax is:
;	State0:  comma -> state1
;	         expression -> state2
;	         end-of-stmt -> accept
;	State1:  comma -> state1
;	         expression -> state2
;	State2:  comma -> state1
;	         end-of-stmt -> accept
;
; Entry:
;	pTokScan points to 1st expression or comma
;	cIdArgs is assumed to be 0 (it gets set by NtStatement())
;
; Exit:
;	The return value is PR_GoodSyntax or PR_BadSyntax
;
;********************************************************************
PUBLIC	NtNArgsMax3, NtNArgsMax4, NtNArgsMax5
NtNArgsMax3 PROC NEAR
	mov	al,3
	SKIP2_PSW
NtNArgsMax3 ENDP
NtNArgsMax4 PROC NEAR
	mov	al,4
	SKIP2_PSW
NtNArgsMax4 ENDP
NtNArgsMax5 PROC NEAR
	mov	al,5
NtNArgsMax5 ENDP
NtNArgs PROC NEAR
;At this point, we can accept and end-of-stmt, expression, or comma
	push	si
	cbw
	xchg	si,ax			;si = max args allowed
	call	NtEndStatement
	mov	al,PR_GoodSyntax	;prepare to return PR_GoodSyntax
	jne	ArgExit			;brif end-of-stmt
ArgLoop:
	inc	[cIdArgs]		;bump cnt for opNull or opUndef
	mov	ax,IRW_Comma
	call	TestScan_AX
	jne	ArgExp			;brif current token is not a comma
	mov	ax,opNull		;let executor know arg was defaulted
	call	Emit16_AX
	jmp	SHORT ArgComma

;We need an expression at this point
ArgExp:
	mov	ax,opUndef		;let executor know arg wasn't  defaulted
	call	Emit16_AX
	call	NtConsumeExp		;try to parse an expression
					;NtExp bumps cIdArgs
					;returns PR_GoodSyntax or PR_BadSyntax
	jl	ArgExit			;brif result == PR_BadSyntax
	call	NtEndStatement
	mov	al,PR_GoodSyntax	;prepare to return PR_GoodSyntax
	jne	ArgExit			;brif end-of-stmt

;We need a comma at this point
ArgComma:
	dec	si
	je	ArgExpEos		;brif beyond last legal arg
	mov	ax,IRW_Comma		;Consume ","
	call	ConsumeRw_AX
	jnc	ArgLoop			;brif no syntax error
					; else al = PR_BadSyntax
ArgExit:
	pop	si
	ret	

ArgExpEos:
	mov	ax,MSG_eos		;Expected End of statement
	call	PErrExpMsg_AX		;generate error, al=PR_BadSyntax
	jmp	SHORT ArgExit
NtNArgs	ENDP

;********************************************************************
; NtConstAssign
; Purpose:
;	Recognize the nonterminal 'id = <expression>'
;	Where <expression> can contain no intrinsic functions
;
;********************************************************************
PUBLIC	NtConstAssign
cProc	NtConstAssign,<PUBLIC,NEAR>,<si,di>
	localW	oSrcTok
cBegin
	call	EnsRude			;make sure table is in SS_RUDE state
	call	IdTok			;bx points to current token
	je	GotConstId		;brif its an id token
ConstBadName:
	call	PErrExpId		;Expected id, al = PR_BadSyntax
	jmp	SHORT ConstExit

GotConstId:
	TESTM	bx.TOK_id_vmFlags,FVI_FNNAME	
	jne	ConstBadName		;brif id begins with FN
	mov	si,[bx.TOK_id_oNam]
	mov	di,[bx.TOK_id_oTyp]
	mov	ax,[bx.TOK_oSrc]
	mov	[oSrcTok],ax		;remember token's column for error
					; reporting
	call	ScanTok			;skip id
	mov	ax,IRW_EQ
	call	ConsumeRw_AX		;parse '='
	jc	ConstExit		;brif syntax error (return PR_BadSyntax)

	push	[ps.PS_bdpDst.BDP_cbLogical];save cur #bytes pcode generated
	DbAssertRel si,ne,0,CP,<NtConstAssign err1>
	mov	[oNamConstPs],si	;set static flag so NtExp does not
					; allow intrinsic functions or variables
	call	NtConsumeExp		;parse an expression
	mov	[oNamConstPs],0
	pop	dx			;dx = offset to 'id=expr' pcode
	jl	ConstExit		;brif result == PR_BadSyntax

.errnz OPCODE_MASK - 3FFh		
	xchg	ax,di			;ax = oTyp
	xchg	ah,al			
	shl	ax,1			
	shl	ax,1			
	add	ax,opIdSt		;ax = opIdSt with oTyp in top 6 bits
	call	Emit16_AX		;emit opcode
	xchg	ax,si			;pass oNam in ax
	call	Emit16_AX		;emit oNam
ConstGoodExit:
	mov	al,PR_GoodSyntax
ConstExit:
cEnd



			;-----------------------------
			; Primitive ID token scanners 
			;-----------------------------

;*********************************************************************
; IdTokPeriodImp1
; Purpose:
;	Same as IdTokPeriodImp, but this generates errors if desired
;	token is not found, instead of returning PR_NotFound
;
;*********************************************************************
IdTokPeriodImp1	PROC NEAR
	call	IdTok			;bx points to current token
	jne	J1_PErrExpId		;brif not an id token
					; error: Expected id, al = PR_BadSyntax
	call	IdTokPeriodImp
	je	J1_PErrExpIdImp		;brif not implicit id
					; id can't have %&!#$, al=PR_BadSyntax
	ret
IdTokPeriodImp1	ENDP

;*********************************************************************
; IdTokPeriodImp
; Purpose:
;	See if current token is an id token
;	Allow current token to have "." within id
;	Id cannot have explicit type char
; Entry:
;	[pTokScan] points to current token
; Exit:
;	bx points to current token
;	if token is an id token with no explicit type char:
;	   al = PR_GoodSyntax, non-zero condition codes
;	else
;	   al = PR_NotFound, zero condition codes
;
;*********************************************************************
PUBLIC	IdTokPeriodImp
IdTokPeriodImp PROC NEAR
	call	IdTok			;bx points to current token
	jne	IdTpNotFound		;brif not an id token
	;see if token needs to be rescanned
	cmp	[bx.TOK_id_termChar],'.'
	jne	NoRescan		;brif id not terminated by "."
	or	[psFlags],PSIF_fPeriodOk	;this time, don't stop at "."
	call	LexReset		;rescan pTokScan, don't stop at "."
	and	[psFlags],NOT PSIF_fPeriodOk
NoRescan:
	cmp	[bx.TOK_id_oTyp],ET_IMP
	je	IdGoodExit		;brif id has no explicit type char
IdTpNotFound:
	mov	al,PR_NotFound
IdTpExit:
	or	al,al			;set condition codes for caller
	ret
IdTokPeriodImp ENDP

;*********************************************************************
; IdTokNoPeriodImp
; Purpose:
;	Parse an id token, where the id cannot have a period in it.
;	Id must have no explicit type char.
;	Since callers of this function demand an id token, i.e.
;	no other token is possible, this function never returns
;	PR_NotFound, but generates the error instead.
; Entry:
;	[pTokScan] points to current token
; Exit:
;	If token is not an id token:
;	   "Expected id" error message is generated
;	   al = PR_BadSyntax
;	else if token has period:
;	   "id can't have '.'" error message is generated
;	   al = PR_BadSyntax
;	else if token has explicit type char:
;	   "id can't have %&!#$" error message is generated
;	   al = PR_BadSyntax
;	else
;	   al = PR_GoodSyntax
;	   bx points to current token
;	Never returns PR_NotFound because no callers have other options
;	condition codes set based on al
;
;*********************************************************************
PUBLIC	IdTokNoPeriodImp
IdTokNoPeriodImp PROC NEAR
	call	IdTokNoPeriod		;parse id token with no period in it
	jl	IdTokExit		;brif PR_BadSyntax
	cmp	[bx.TOK_id_termChar],'.'
	je	J1_PErrExpIdNoPer	;brif token terminated by '.'
	cmp	[bx.TOK_id_oTyp],ET_IMP
	jne	J1_PErrExpIdImp		;brif token has explicit type char
IdGoodExit:
	mov	al,PR_GoodSyntax	;prepare for successful return
IdTokExit:
	or	al,al			;set condition codes for caller
	ret
IdTokNoPeriodImp ENDP

J1_PErrExpIdNoPer:
	mov	ax,MSG_BadElemRef	;identifier can not have "."
J1_PErrMsg_AX:
	jmp	PErrMsg_AX

;*********************************************************************
; IdTokNoPeriod
; Purpose:
;	Parse an id token, where the id cannot have a period in it.
;	Since callers of this function demand an id token, i.e.
;	no other token is possible, this function never returns
;	PR_NotFound, but generates the error instead.
; Entry:
;	[pTokScan] points to current token
; Exit:
;	If token is not an id token:
;	   "Expected id" error message is generated
;	   al = PR_BadSyntax
;	else
;	   al = PR_GoodSyntax
;	   bx points to current token
;	Never returns PR_NotFound because no callers have other options
;	condition codes set based on al
;
;*********************************************************************


PUBLIC	IdTokNoPeriod
IdTokNoPeriod PROC NEAR
	or	[psFlags],PSIF_fNoPeriod
	call	LexReset		;rescan pTokScan if necessary
					; "." terminates id token
	and	[psFlags],NOT PSIF_fNoPeriod
	call	IdTok			;bx points to current token
	je	IdGoodExit		;brif its an id token
J1_PErrExpId:
	jmp	PErrExpId		;error: "Expected id", al=PR_BadSyntax
					; return to caller
IdTokNoPeriod ENDP

;*********************************************************************
;IdSub
;Purpose:
;	Scan an id token and give an error if:
;	  - it begins with FN [QB5]
;	  - it has an explicit type char
;Exit:
;	If not valid id token
;	   generates "Expected id" message and returns PR_BadSyntax
;	Else if id begins with FN and ax was 0 on entry
;	   generates "Can't begin with 'FN'" message and
;	   returns PR_BadSyntax
;	Else if id ends with explicit type char,
;	   generates "Can't end with '%&!#$'" message and
;	   returns PR_BadSyntax
;	Else returns PR_GoodSyntax with bx pointing to token descriptor
;	Never returns PR_NotFound because no callers have other options
;	condition codes set based on value in al
;
;*********************************************************************
IdSub	PROC NEAR
	sub	ax,ax
	sub	ax,ax			;id can't begin with FN
	call	IdTokFn
	jl	IdTokExit		;brif PR_BadSyntax
	cmp	[bx.TOK_id_oTyp],ET_IMP
	je	IdGoodExit		;brif no explicit type char
J1_PErrExpIdImp:
	jmp	PErrExpIdImp		;id can't have %&!#$
					;al = PR_BadSyntax, return to caller
IdSub	ENDP

;*********************************************************************
;IdTokFn
;Purpose:
;	Scan an id token and give an error if:
;	  - it begins with FN, and FN was not expected - or
;	  - it doesn't begins with FN, and FN was expected
;Entry:
;	ax = 0 if FN is not expected, FVI_FNNAME if it is expected
;Exit:
;	If not an id token
;	   generates "Expected id" message and returns PR_BadSyntax
;	Else if id begins with FN and ax was 0 on entry
;	   generates "Can't begin with 'FN'" message and
;	   returns PR_BadSyntax
;	Else if id does not begin with FN and ax was FVI_FNNAME on entry
;	   returns PR_NotFound
;	Else returns PR_GoodSyntax with bx pointing to token descriptor
;	Never returns PR_NotFound because no callers have other options
;	condition codes set based on value in al
;
;*********************************************************************
PUBLIC	IdTokFn
IdTokFn PROC NEAR
	push	ax			;save fExpectingFn
	call	IdTok			;bx points to current token
	pop	cx			;cx = 0 if id can't begin with FN
	je	IdFnGotId		;brif got id token
	jmp	J1_PErrExpId		;error "Expected Id", al = PR_BadSyntax
					;return to caller
IdFnGotId:
	mov	dx,[bx.TOK_id_vmFlags]
	and	dx,FVI_FNNAME		;ax = non-zero if id begins with FN
	cmp	dx,cx
	je	IdGoodExit		;brif FN state is as expected
	mov	ax,MSG_FNstart		;error "Can't start with 'FN'"
	jcxz	J1_PErrMsg_AX		;brif FN not expected
	mov	ax,MSG_ExpFNid
	jmp	PErrExpMsg_AX		;generate error, return al=PR_BadSyntax
IdTokFn	ENDP

;*********************************************************************
; IdTok
; Purpose:
;	See if current token is an id token
; Entry:
;	[pTokScan] points to current token
; Exit:
;	bx points to current token
;	Zero flag is set if token is an id token
;	NOTE: CALLERS ASSUME AL IS PRESERVED
;
;*********************************************************************
PUBLIC	IdTok
IdTok	PROC NEAR
	mov	bx,[pTokScan]
	cmp	[bx.TOK_class],CL_ID
	ret
IdTok	ENDP

sEnd	CP

end
