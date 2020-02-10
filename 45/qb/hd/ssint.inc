;*** 
;ssint.inc
;
;	Copyright <C> 1988 Microsoft Corporation
;
;
;*******************************************************************************

;SSINT	- Scanner internal interface definitions
SSINT_INC = ON

	IncludeOnce	architec
	IncludeOnce	scanner

;=============================================================
;Constants
;

LOWUND		EQU LOW UNDEFINED

RB_fSizeOp	EQU 80h 	;Rule table constant - indicates cbTyp operand

LineUpdate	=	64	;Number of lines scanned per screen update
				;MUST be power of two!!!

ET_FormRec	equ	ET_MAX

;=============================================================
;Scanner Stack Entry Definitions
;

;Scan stack frame ids - major types
STYP_If 	EQU 100h	;IF stack entry label
STYP_Else	EQU 200h	;opStElse that matches an If
STYP_For	EQU 400h	;FOR block identifier
STYP_DefFn	EQU 800h	;DEF FN block identifier
STYP_Do 	EQU 1000h	;DO block identifier
STYP_While	EQU 2000h	;WHILE block identifier
STYP_Case	EQU 4000h	;SELECT CASE block identifier
STYP_StackEnd	EQU 0		;End of stack token

;Scan stack frame ids - modifiers
STYP_Block	EQU  1		;TRUE for block varients of IF and ELSE
STYP_Lab	EQU  2		;Label varient of IF

STYP_Step	EQU  4h 	;STEP clause in FOR

STYP_DoWhile	EQU 10h 	;DO While varient of DO
STYP_DoUntil	EQU 20h 	;DO Until varient of DO
STYP_CaseTo	EQU 40h 	;CASE IS <const> TO <const> varient of SELECT
STYP_CaseRel	EQU 80h 	;CASE [IS <relop>] <const> varient of SELECT

;Scan stack frame ids - common combinations
STYP_IfLab	EQU  STYP_If OR STYP_Lab
				;Label varient of IF
				; Frame contains the oTx of the IF oTx field
STYP_IfBlock	EQU  STYP_If OR STYP_Block
				;Block IF stack entry label
				; Frame contains the oTx of the IF oTx field
STYP_ElseBlock	EQU  STYP_Else OR STYP_Block
				;opStElse that matches an IfBlock
STYP_ElseNop	EQU  STYP_Else OR STYP_Lab
				;opStElseNop which matches a preceding opStIfLab

;The following stack frame identifiers are used on the debug stack only.
STYP_Exp	EQU -5		;Present in debug stack only.
STYP_VtRf	EQU -4		;Present in debug stack only.
STYP_AVtRf	EQU -3		;Present in debug stack only.
STYP_ArgCnt	EQU -2		;Present in debug stack only.

;===========================================================================
;Scan stack Structure Definitions
;
;The following structures are present on the scan stack.

;FOR structure is used for all varients of FOR (but not EXIT FOR).
FFOR	STRUC
FFOR_Id 	DW 0
FFOR_oTx	DW 0
FFOR_oTxExit	DW 0 ;Must align with FDO_oTxExit
FFOR_oTxIdRf	DW 0
FFOR_oTyp	DW 0
FFOR	ENDS

;IF structure is used for all varients of IF/THEN/ELSEIF

FIF	STRUC
FIF_Id		DW 0
FIF_oTx 	DW 0
FIF_oTxBranch	DW 0
FIF	ENDS

;DO structure is used for all looping constructs DO/LOOP, and WHILE/WEND

FDO	STRUC
FDO_Id		DW 0
FDO_oTxLoop	DW 0
FDO_oTxExit	DW 0 ;Must align with FFOR_oTxExit
FDO	ENDS

;SELECT CASE structure is used for all varients of SELECT CASE / END SELECT

FCASE	STRUC
FCASE_Id	DW 0
FCASE_oTyp	DW 0
FCASE_oTxBranch DW 0
FCASE_oTxTrue	DW 0
FCASE_oTxFalse	DW 0
FCASE	ENDS

;Expressions

FEXP	STRUC
FEXP_oTyp	DW 0
FEXP_oTx	DW 0
FEXP	ENDS

;===========================================================================
;oType contains two fields.
;	The low 13 bits is either a key value for native types or is an
;	offset into the type table to a user defined type definition.
;	The high 3 bits are used by the scanner to identify the origin
;	of the argument to handle several special cases. (ST=scanner type)

ET_RC	equ	0		;Scan stack only ET type for records

;Flag bits in high byte of scan stack oTyp entry
ST_Form	=	1
ST_Seg	=	8
ST_ByVal=	10H

ST_Typ_Mask =	00ffh		;oTyp stored in low byte. Records use ET_RC.

ST_Flags=	not ST_Typ_Mask ;Mask for flag field
ST_Bits =	2000h		;First available bit

;		0*ST_Bits	;All expressions not covered by below
ST_Marker=	1*ST_Bits	;Marker for non-oType stack entries
ST_Lit	=	2*ST_Bits	;Argument is literal without operand
				;  (Replace executor to coerce to R4)
ST_LitX =	3*ST_Bits	;Argument is literal with operand or with unary minus
				;  (OK for static array in DIM index)

;For these types, Ld executors get replaced with Rf's on procedure calls
ST_ArrVar=	4*ST_Bits	;Argument is from opAIdLd
ST_SimpVar=	5*ST_Bits	;Argument is from opIdLd
ST_RecArVar=	6*ST_Bits	;Argument is from opOffLd off array
ST_RecScVar=	7*ST_Bits	;Argument is from opOffLd off scalar

;Derived bit patterns to optimize certain tests
;WARNING!! Some scanner code is very sensitive to bit assignments!
ST_Lit? =	ST_Lit and ST_LitX		;non-zero if literal
ST_RecVar=	ST_RecArVar and ST_RecScVar
ST_Var? =	ST_SimpVar and ST_ArrVar and ST_RecVar
ST_Array?=	ST_Flags and not ST_ArrVar	;Test zero if array var.
ST_Record?=	ST_RecVar and not ST_Var?	;Test non-zero if record
ST_ArrayBit=	ST_ArrVar xor ST_SimpVar	;Bit set if not array

;======================================================================
;SsRefarg flags
;Passed in dh to indicate details of required reference argument
;These must be the same as certain bit assignments in SsProc.
;Verification is made there.
Lvalue	=	3			;Indicates copy back needed
FarArg	=	28H			;Make sure argument is FAR
FScb	=	10H			;Far ref w/length to be used for FS

;======================================================================
;SsBosFlags definitions
;	These flags show state that lasts during a statement.
;	SsBosFlags is set to zero at each BOS.
;
SSBOSF_StCommon EQU	1	;COMMON statement detected
SSBOSF_StShared EQU	2	;SHARED statement detected
SSBOSF_StStatic EQU	4	;STATIC statement detected
SSBOSF_Const	EQU	8	;Scanning CONST statement
SSBOSF_Inserted EQU	10H	;Insertion occured in this statement
SSBOSF_PcUpdate EQU	20H	;PC update pcode exists in this statement
SSBOSF_StCase	EQU	40H	;CASE syntax processing required for this line

;General-purpose scanner flags, kept in SsFlags:
SSF_ScanAndExec EQU	1	;ScanAndExec is processing CONST statement
SSF_HaveDimmed	EQU	2	;Have done a DIM: OPTION BASE illegal
SSF_If		EQU	4	;IF syntax processing required for this line
SSF_StSelect	EQU	8	;SELECT CASE <stmt lst> CASE checking is
				;required
SSF_InType	EQU	10H	;Inside a TYPE declaration
SSF_CantCont	EQU	20H	;Call CantCont at end of scan

;=======================================================================
;Text table link list maintenance structure.  This structure is allocated
;as a local variable on the scanner stack.
;

TXLNK		STRUC
TXLNK_LabDefNext	DW 0	;oTx of next link in label definition list
TXLNK_LabDefLast	DW 0	;oTx of last link in label definition list
TXLNK_Data		DW 0	;oTx of last link in DATA list
TXLNK_DefType		DW 0	;oTx of last link in DEFtyp list
TXLNK_Type		DW 0	;oTx of last link in TYPE/END TYPE list
TXLNK_DefFn		DW 0	;oTx of last link in DEF FN/END DEF list
TXLNK		ENDS

;SSL		STRUC
;SSL_oTxLabDefNext	DW 0	;offset of next label definition
				;(unrelocated for expansion.)
;SSL_oTxLabDefLast	DW 0	;offset of last bound label definition
;SSL		ENDS

;=======================================================================
;Data segment locations
;


sBegin	DATA
ife	SSDATA_ASM
extrn	SsErrOTx:word		;oTx of pcode which was replaced by opEot
extrn	SsErr:word		;Error code to be returned by scanner.
extrn	SsErrOpcode:word	;Opcode which was replaced by opEot
extrn	SsDelayErr:word		;Error code of delayed error
extrn	SsDelayLoc:word		;oTx of delayed error
extrn	SsDelayCnt:word		;Count of pending delayed errors
extrn	SsStackSave:word	;Scanner sp at start of scan loop

extrn	f_Static:byte		;TRUE if current array type default is $STATIC

extrn	f_StaticCalc:byte	;TRUE if current array would be $STATIC if this
				; were the first reference.

extrn	SsExec:word		;SsExecFlag and SsExecTmp together
extrn	SsExecFlag:byte		;OPA_fExecute ORed in if can't allow COMMON
extrn	SsExecTmp:byte		;Temporary hold for SsExecFlag during CONST
extrn	ScanRet:word		;Contains address of scanner main loop
extrn	SsOTxBOS:word		;Offset of last encountered BOS
extrn	SsOTxPatchBos:word	;Address of executor to be patch with Bos
extrn	SsOTxStart:word 	;Address of point were stack was clear.  This
				;is used to find the bounds for Dim as well as
				;the insertion point for CDecl calls.
extrn	ScannerFlags:word	;SsBosFlags and SsFlags together
extrn	SsBosFlags:byte		;Scanner begin of statement flags
extrn	SsFlags:byte		;General scanner flags
extrn	SsCbTxExpand:word	;CB of text expansion during scanning.
extrn	SsLinkCtl:word		;Label Control Structure
extrn	SsCbFrameTemp:word	;Temp space for current statement

;Data for update of return addresses on stack
extrn	SsNextOTx:word		;oTx of return address
extrn	SsReturnBp:word		;offset in stack of return frame

;ScanAndExec data
extrn	SsScanExStart:word	;Starting address of execution
extrn	SsScanExSrc:word	;oTx of original source

	;Data used by ssproc to count param bytes for CDECL calls

extrn	SsCbParmCur:word	;cb of all params so far
extrn	SsParmCnt:word		;Number of parameters

extrn	SsOtxHeapMove:word	;oTx of last possible heap movement
extrn	SsBosStack:word		;SP-2 at start of statement

endif			;SSDATA_ASM
sEnd	DATA


;=======================================================================
;Entrypoints defined in SsScan.asm
;

ife	SSSCAN_ASM
	extrn	ScanExExit:far	;jmp here to terminate execution from scanner
endif				;ife SSSCAN_ASM

sBegin	SCAN			

ife	SSSCAN_ASM
extrn	ScanExit:near		;jmp here when scan is complete
extrn	SsScanExExit:near	;jmp here when scan of ScanAndExec is done
extrn	ExecuteFromScan:near	;Fire up execution from scanner

extrn	SsError:near		;Scanner error handler
extrn	SsErrorBx:near		;Scanner error handler--emit oTx bx
extrn	SsFrameType:near	;Report nesting error
extrn	DescanTerm:near 	;Descan loop termination address
extrn	DescanX:near		;Rude descan loop termination address
endif				;ife SSSCAN_ASM

;======================================================================
;Entrypoints defined in ssid.asm
;
ife	SSID_ASM
extrn	SsGetISFC:near		;Id opcode to executor scope resolution
extrn	GetCxISFC:near
endif				;ife SSID_ASM

;======================================================================
;Entrypoints defined in ssaid.asm
;
ife	SSAID_ASM
extrn	SsIndexType:near	;Index into executor map based on oType
extrn	SsIndexTypeCx:near	;Index into executor map based on oTyp in cx
extrn	SsIndexISFC:near	;Index into executor map based on ISFC
extrn	SsEmitExecutor:near	;Get executor from map and emit with operand

extrn	MakeArrayRef:near	; Convert AIdLd to AdRf.
endif				;ife SSAID_ASM

;======================================================================
;Entrypoints defined in sstxutil.asm
;
ife	SSTXUTIL_ASM
extrn	SsMakeGap:near		;Create initial gap between source and emit pcode
extrn	SsEnsureGap:near	;Ensure a gap between source and emit pcode
endif				;ife SSTXUTIL_ASM

;======================================================================
;Labels in exconv.asm
ife	EXCONV_ASM
extrn	tImpCo:word	;Table of implicit coercion executors
endif				;EXCONV_ASM

;========================================================================
;Tables and entrypoints defined in SsRules.asm
;
ife	SSRULES_ASM
extrn	EnsureArgType:near	;Ensure top stack argument is of correct type
extrn	SsCoerceReg:near	;Coercion handler (args in registers)
extrn	SsCoerceN:Near		;Coercion handler for n stack entries
extrn	TMError:near		;Type Mismatch Error
extrn	TMErrorNZ:near		;Type Mismatch Error if ZF not set
extrn	tRuleByte:byte		;Rule byte - usually an emitted type
extrn	tRuleWord:word		;Rule word - usually a coercion table address
extrn	mpOpRule:byte		;Opcode to rule table
extrn	Insert:near		;Insert into text
extrn	InsertCx:near		;Insert into text
extrn	InsertBranchBos:near	;Insert branch after opBos
extrn	InsertBranch:near	;Insert into text
extrn	Insert1Op:near		;Insert into text
endif			;ife SSRULES_ASM

;======================================================================
;SSBOS resident routines
;
ife	SSBOS_ASM
extrn	EmitExCopyOps:near	;Emit executor and copy operands
extrn	CopyOperands:near	;Copy operands for opcode bx from si to di
extrn	LabelBindCur:near	;Bind a label reference to current scope
extrn	SsBosStack:word		;SP-2 at start of statement
endif				;SSBOS_ASM

;========================================================================
;Scanner specific tables built by OPTABLER
;
extrn	mpOpExe:word		;Opcode to executor (or executor map) table
extrn	mpOpAtr:byte		;Opcode to atribute table
extrn	mpOpScanDisp:word	;Opcode to scanner dispatch table


;======================================================================
;Scanner internal entrypoint declarations for ssdescan.asm
;
ife	SSDESCAN_ASM
extrn	SsLabelRefDebind:near
extrn	SsD_NOps:near
endif				;SSDESCAN_ASM
sEnd	SCAN			

;======================================================================
;Scanner internal entrypoint declarations for ssif.asm
;
ife	SSIF_ASM
extrn	MapOpToExeNumeric:near
endif				;SSIF_ASM

;======================================================================
;Scanner internal entrypoint declarations for ssoperat.asm
;
ife	SSOPERAT_ASM
extrn	MSdFs:near
endif				;SSOPERAT_ASM

;======================================================================
;Scanner internal entrypoint declarations for ssmisc.asm
;
ife	SSMISC_ASM
extrn	GetRuleInfo:near
extrn	FindFrame:near
extrn	CbFrameSTYPCx:near
extrn	SsAllocOFrame:near
extrn	BindExit:near
extrn	BindExitCur:near
extrn	CbTypOTypSCAN:near	
extrn	CbTypOTypOMrsSCAN:near	
endif				;SSMISC_ASM

;=============================================================================
;Scanner internal entrypoint declarations for ssproc.asm
;
ife	ssproc_asm
extrn	ByValMarker:abs		;Rule table entries for BYVAL
extrn	SegMarker:abs		;   and SEG

extrn	SsCallFunc:near		;Entry point to SSPROC from SSID and SSAID
				;   for function calls
extrn	SsReLinkDecl:near	;Adjust PRS_otxDef and copy operands
extrn	SsReLinkNoCopy:near	;Adjust PRS_otxDef
extrn	SsFindOpNoList1:near	;Find NoList pcodes
extrn	CopyOps:near		;Copy operands and dispatch
endif				;SSPROC_ASM

;=============================================================================
;Scanner internal entrypoint declarations for ssrec.asm
;
ife	ssrec_asm
extrn	MakeRef:near
endif				;SSREC_ASM

;=============================================================================
;Scanner internal entrypoint declarations for ssrefarg.asm
;
ife	ssrefarg_asm
extrn	AllocTemp:near
extrn	SsRefArg:near		;Handle all reference argument problems
endif				;SSREFARG_ASM


;=============================================================================
;Scanner internal entrypoint declarations for ssdeclare.asm
;
ife	ssdeclare_asm
extrn	SetArrayType:near
endif				;SSDECLARE_ASM

;=============================================================================
;Scanner Macros and equates
;

;Scan Routine entry point definition

SsProcRude	EQU -4		;Rude mode scan and descan dispatch
SsProcParse	EQU -2		;Parse mode descan dispatch

	;Scan to SS_EXECUTE mode dispatch declaration macro

SsProc	MACRO	ScanNam,Rude,Local
ifnb	<&Rude>
extrn	SsV_&ScanNam:near
	DW	scanOFFSET SsV_&ScanNam
endif
public	Ss_&ScanNam
ifb	<&Local>
extrn	SsD_&ScanNam:near
endif
	DW	scanOFFSET SsD_&ScanNam
Ss_&ScanNam&:
	ENDM

	;Rude mode dispatch point declaration macro


SsVProc MACRO	ScanNam
	public	SsV_&ScanNam
SsV_&ScanNam&:
	ENDM


	;Descan to SS_PARSE mode dispatch point declaration macro


SsDProc MACRO	ScanNam
	public	SsD_&ScanNam
SsD_&ScanNam&:
	ENDM


	;Pcode location macros

PTRTX	equ	<word ptr es:>

LODSWTX equ	<lods word ptr es:[si]>
STOSWTX equ	<stos word ptr es:[si]>
MOVSWTX equ	<movs word ptr es:[si],word ptr es:[di]>

TEXTSEG equ	<db 26h>


sBegin	CODE	;DWEXT needs codeOFFSET defined, and this does it
sEnd	CODE


DWEXT	MACRO	pex,fDefine
	ifb	<fDefine>		;; fDefine specified?
	    extrn   pex:far		;; No, generate offset
	    dw	    codeOFFSET pex
	elseif	    fDefine		;; Yes, use specified fDefine
	    extrn   pex:far
	    dw	    codeOFFSET pex
	endif
	ENDM


DWFILL	MACRO	fDefine
	ifb	<fDefine>		;; fDefine specified?
	    dw	    ?			;; No, Assume true
	elseif	fDefine 		;; Yes, use specified fDefine
	    dw	    ?
	endif
	ENDM



DWIF	MACRO	wData,fDefine		;
	if	fDefine 		;
	    dw	    wData		;
	endif				;
	ENDM				;


DBIF	MACRO	bData,fDefine		;
	if	fDefine 		;
	    db	    bData		;
	endif				;
	ENDM				;


;***
;PUSH_ES, POP_ES, REFRESH_ES macros
;
;Purpose:
;	There are a number of places in the scanner where we can simply
;	push the text segment prior to a far call and pop it on return in
;	non-windows versions (i.e., the called routine promises to cause
;	no heap movement).  In windows versions, however, the far call could
;	cause a thunk and thus heap movement.  PUSH_ES/POP_ES allow us
;	to generate the minimal required code for either case.
;
;	REFRESH_ES is similar, but for the case where the called routine
;	itself preserves ES.
;
;	Added as part of revision [3].
;****


PUSH_ES     equ     <push es>
POP_ES	    equ     <pop es>
REFRESH_ES  equ     <>



;===========================================================================
;Scanner Debug Macros and equates
;


SsDbStackIni	equ	<>
SsDbStackEmpty	equ	<>

SsDbStackPush	MACRO	type
		ENDM

SsDbStackPop	MACRO	type
		ENDM
