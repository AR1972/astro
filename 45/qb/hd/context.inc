;*** 
;context.inc
;
;	Copyright <C> 1985, 1986, 1987 Microsoft Corporation
;
; NOTE: When making changes to this file, be sure to make equivalent
;       changes to file CONTEXT.H
; NOTE: For comments, see file CONTEXT.H
;
;
;*******************************************************************************
CONTEXT_INC = -1                ;remember that this file has been included
	IncludeOnce	heap	
	IncludeOnce	pointers
	IncludeOnce	sb	
;**======================================================================**
;**==           External Interface to Context Mgr Component            ==**
;**======================================================================**


;**==================== text descriptor (TXD_) ============================
;
;Definition of text table descriptor.  Both modules and procedures have
;text tables.  All text-offset (otx...) fields in this descriptor are
;maintained by the text manager during edit operations, and by the scanner
;during pcode expansion/contraction.
;For all linked lists through the pcode, (otx...Link), the link is always
;a text offset to a pcode's 1st operand, NOT to the 1st byte of the pcode.
;This speeds up traversal of the list.
;
;=======================================================================*

TXD			STRUC	;offset 	;offset
TXD_bdlText_cbLogical	DW 0	;00		;00
TXD_bdlText_seg 	DW 0	;02		;02
TXD_bdlText_dummy	DW 0	;04
TXD_bdlText_cPhysical	DW 0	;06		;04
TXD_otxLabLink		DW 0	;08		;06
TXD_otxDefTypeLink	DW 0	;0a		;08
TXD_otxTypeLink 	DW 0	;0c		;0a
TXD_otxReParseLink	DW 0	;0e		;0c
TXD_cLines		DW 0	;10		;0e
TXD_cLinesIncl		DW 0	;12		;10
TXD_lnCursor		DW 0	;14		;12
TXD_scanState		DB 0	;16		;14
TXD_flags		DB 0	;17		;15
TXD			ENDS	;18		;16

TXD_bdlText     EQU     TXD_bdlText_cbLogical
;Define place to keep address of compiled code
TXD_oCompiled   EQU     TXD_otxLabLink
TXD_segCompiled EQU     TXD_otxDefTypeLink
TXD_bdlText_status	equ	TXD_bdlText_seg 

FTX_mrs         EQU 01H ;TRUE if text table is mrs, not prs
FTX_asChg       EQU 02H ;TRUE if PreScanAsChg needs to be called
                        ; before we scan this text table again
;The following FTX_Tmp... flags are used temporarily within the scope of
;SaveDeclares().  If another function needs to make temporary use of a flag,
;it can use these bits as well, as long as it assumes they contain garbage
;on entry and exit.
;
FTX_TmpDecl     EQU 4   ;Used by SaveDeclares in text mgr.
FTX_TmpRef      EQU 8   ;Used by SaveDeclares in text mgr.



;**==================== Procedure Register Set (PRS) =======================
;The there is a procedure register set for each SUB, DEF and FUNCTION in the 
;program. 
;
;For SUB's and FUNCTION's, there is only one procedure per name, regardless
;of FUNCTION type or module of definition.
;There is at most one DEF FN per module per type per name.
;FUNCTIONs and SUBs are global to the program.  DEF FNs are local to a module.
;
;Prs's must exist whenever there is pcode (even if no variable tables
;exist), because pcode's contain references to entries.
;Prs's are used at runtime for procedure invocations for identifying
;the new module.  Prs's are built only at the request of the parser/
;user-interface.
;
;While a procedure is loaded into prsCur, its entry in the Rs table
;should not be referenced or updated.
;==========================================================================

;Flag constants used with the PRS_flags byte, below
FP_STATIC       EQU     80h
FP_CDECL        EQU     40h
FP_DEFINED      EQU     20h
FP_ENDPROC      EQU     10h
FP_DECLARED     EQU     08h
FP_DEFSCANNED	EQU	04H

; Flag constants used with the PRS_flags2 byte, below
; NOTE: only flags 08H - 80H are available, as flags2 is overlaid over oType
FP_RefDeleted 	EQU	80h			
FP_AUTO 	EQU	40h			
FP_ExeTree	EQU	20h			

PRS                     STRUC
;Begin of zero fill
PRS_cbFrameVars         DW 0    ;00	     
PRS_cbFrameTemp         DW 0    ;02	     
PRS_ogNam		DW 0	;04	;	     			
PRS_procType            DB 0    ;08	     
PRS_oType               DB 0    ;09	     
PRS_flags               DB 0    ;0a	     
PRS_cwParams            DB 0    ;0b	     
;Begin of UNDEFINED fill	
PRS_oVarHash            DW 0    ;0c	     
PRS_oMrs                DW 0    ;0e	     
PRS_oRsDef              DW 0    ;10	     
PRS_otxDef              DW 0    ;12	     
PRS_oPrsNext		DW 0	;14	;	     			
PRS_txd                 DB SIZE TXD DUP(0)   ;16
PRS			ENDS

PRS_flags2	EQU	PRS_oType	; share this byte - - - low nibble
					; is for oTyp, high nibble is flags
M_PT_OTYPE	EQU	07H		;[20] mask to allow access to PRS_oType

;enumerations for PRS_procType:
PT_SUB          EQU     1
PT_FUNCTION     EQU     2
PT_DEFFN        EQU     3
PT_NOT_PROC	EQU	4

; eb api constants


;**==================== Module Register Set (MRS) ============================
;The Module Register Set either directly contains or is the owner of all
;information about a particular module.
;
;The global Rs table contains both prs's and mrs's, each linked together.
;Entries are not compressed, due to the number of structures that have offsets
;into this table.
;
;While a module is loaded into mrsCur, its entry in the global Rs table
;should not be referenced or updated.
;============================================================================**

OGNAM_UNNAMED		EQU	0	; ogNam for an unnamed mrs
OGNAM_GMRS		EQU	1	; an invalid ogNam to be used as
					;  the ogNam of the global mrs
OGNAM_CLIPBOARD		EQU	2	; ogNam for clipboard mrs
OGNAM_IMMEDIATE		EQU	3	; ognam for Immediate window
OGNAM_PSEUDO_MAX	EQU	3	; max psuedo ogNam value

OMRS_GLOBAL		EQU	0	

;Flag constants used with the MRS_flags byte, below
FM_OptionBase1  EQU     01h
FM_VARNEW       EQU     02h
FM_asChg        EQU     04h
FM_TEMPORARY    EQU     08h
FM_AllSsRude    EQU     10h
FM_LinksOk 	EQU 	20H 		
FM_MustVisit	EQU	40H		
FM_Visited	EQU	80H		
;NOTE: all flags except FM_asChg get set to 0 by BinaryLoad().
;      When adding flags, make sure this is desired

;Flag constants used with the MRS_flags2 byte, below.  These are file related
FM2_Modified    EQU     01h
FM2_NewFile     EQU     02h
FM2_Include     EQU     04h
FM2_NoPcode     EQU     08h
FM2_File        EQU     10h
FM2_AsciiLoaded EQU     20h
FM2_ReInclude	EQU	40h
FM2_EntabSource EQU     80h
;NOTE: all flags except FM2_EntabSource get set to 0 by BinaryLoad().
;      When adding flags, make sure this is desired

;Flag constants used with the MRS_flags3 byte, below.
FM3_NotFound    EQU     01h
FM3_Translated	EQU	04h		
;NOTE: This flags are NOT saved by BinarySave/Load.

MRS                     STRUC
;Begin of zero fill
;------------------
MRS_cbFrameVars         DW 0
MRS_cbFrameTemp         DW 0
MRS_ogNam		DW 0
MRS_bdVar	DB SIZE BD DUP(0)			
MRS_flags		DB 0
MRS_flags2		DB 0
MRS_flags3		DB 0	
MRS_dummy1		DB 0	
;Begin of UNDEFINED fill
MRS_oMrsNext		DW 0
MRS_otxHandler		DW 0
MRS_bdlNam_cbLogical	DW 0
MRS_bdlNam_seg		DW 0
MRS_bdlNam_dummy	DW 0
MRS_bdlNam_cPhysical	DW 0
MRS_txd 		DB SIZE TXD DUP(0)

MRS_otxDefFnLink	DW 0
MRS_oPastLastVar	DW 0
MRS_data_otxFirst	DW 0
MRS_data_otxCur 	DW 0
MRS_data_oLineCur	DW 0
MRS			ENDS

MRS_bdlNam      EQU     MRS_bdlNam_cbLogical
MRS_bdlNam_status	equ	MRS_bdlNam_seg	

;**==================== Global Register Set (GRS) ========================
;The global register set owns heap entries which define the current context.
;
;Only one module table entry and procedure table entry may be active at any
;one time.  A module table entry MUST be active, and a procedure entry MAY
;be active. These tables are managed by swapping a new mrs into mrsCur or
;swapping a new procedure into prsCur.
;
;While a module or procedure is loaded into mrsCur or prsCur, its entry
;in the global Rs table should not be referenced or updated.
;
;=======================================================================**

GRSTYPE STRUC
;Begin of zero fill			;QB5		EB
GRS_fScan                       DB 0    ;00		00
GRS_fDirect			DB 0	;01		01
GRS_bdRs	DB SIZE BD DUP(0)				
GRS_bdtComBlk	DB SIZE BD DUP(0)				
GRS_flags			DB 0	;0e		0c
GRS_filler			DB 0	;1f		0d
;Begin of UNDEFINED fill		
GRS_oMrsMain			DW 0	;10		0e
GRS_oRsCur			DW 0	;12		10
GRS_pMrsCur			DW 0
GRS_pRsCur			DW 0
GRS_offsetTxdSeg		DW 0	;14		12
GRS_oMrsCur			DW 0	;16		14
GRS_oPrsCur			DW 0	;18		16
GRS_otxCur			DW 0	;1a		18
GRS_oRsCONT			DW 0	;1c		1a
GRS_otxCONT			DW 0	;1e		1c
GRS_oRsContTxtTbl		DW 0	;20		1e
GRS_bdlDirect_cbLogical 	DW 0	;22		20
GRS_bdlDirect_seg		DW 0	;24		22
GRS_bdlDirect_dummy		DW 0
GRS_bdlDirect_cPhysical 	DW 0	;26		24
GRS_flagsDir			DB 0	;28		2a
GRSTYPE ENDS

GRS_bdlDirect           EQU     GRS_bdlDirect_cbLogical
GRS_bdlDirect_status	equ	GRS_bdlDirect_seg	

;GRS_flags enumerations
FG_WatchActive          EQU     01H     ;set if watch pcode is active
FG_RetDir               EQU     02H     ;1 return address to direct-mode buffer
                                        ;exists on the stack.  Entry of direct-
                                        ;mode stmts when this is true causes
                                        ;FG_RetDirBad to be set.
FG_AllSsExecute         EQU     04H     ;set if all text tables are
                                        ;in SS_EXECUTE
FG_OtxInDir		EQU	08H	; Set if the mose recently scanned
					; direct mode statement contained
					; an otx reference.	If this flag
					; is set at the same time as
					; FG_RetDir, then any edit is a
					; CantCont.
;GRS_flagsDir enumerations
FDIR_cleared            EQU     01H
FDIR_new                EQU     02H

;bits for mask passed to ForEach[CP]:
;
FE_PcodeMrs     EQU 01h ;TRUE if mrs's containing pcode are to be visited
FE_TextMrs      EQU 02h ;TRUE if FM_TEXT and FM_TEXTFILE mrs's are to be
                        ; visited (i.e. command window's mrs, docuemnt files)
FE_CallMrs      EQU 04h ;TRUE if pFunc is to be called for mrs's text table
                        ; BEFORE it is called for prs's text tables.
FE_PcodePrs     EQU 08h ;TRUE if prs's with text tables (SUBs/FUNCTIONs)
                        ; are to be visited
FE_NoPcodePrs   EQU 10h ;TRUE if DEF FN and DECLARE prs's are to be visited
FE_FarCall      EQU 20h ;TRUE if function to be called is FAR
FE_SaveRs       EQU 40h ;TRUE if ForEach is to restore caller's oRsCur on exit
FE_CallMrsAfter EQU 80h ;TRUE if pFunc is to be called for mrs's text table
                        ; AFTER it is called for prs's text tables.

;***
;GETRS_SEG, RS_BASE, SETSEG_EQ_SS, [B]PTRRS
;
;Purpose:
;	Macros to allow the global Rs table to be a near or far table
;	for different products built off QB shared sources.
;	Added as revision [6].
;	GETRS_SEG:
;		Same as GETBDL_SEG, but optimized to fetch the segment of
;		this particular table. If the Rs table is near, does nothing.
;	RS_BASE:
;		Used to perform some primitive operation (add, sub, mov)
;		using the base pointer of the Rs table; if the Rs table is
;		far, does nothing (as the offset to start of table is always
;		zero in that case).
;			If the Rs table is far and FV_LMEM == TRUE, then this
;			macro must zero the arg if the operation is 'mov'.
;			(or else "RS_BASE mov,..." does nothing when it is
;			expected to get the Rs table base.)
;
;	SETSEG_EQ_SS:
;		Used to set a given seg address equal to SS, only in the
;		case where the global Rs table is far; does nothing if
;		the table is near.
;	[B]PTRRS:
;		Text macros, similar to [B]PTRTX and [B]PTRVAR, used to
;		provide an es seg override for versions with the Rs table far.
;**************************************************************************
GETRS_SEG   MACRO   reg,temp,opt		;
	ENDM					;

RS_BASE  MACRO	 op,arg,reg			;
	op	arg,grs.GRS_bdRs.BD_pb		;
	ENDM					;

SETSEG_EQ_SS MACRO  reg 			;
	ENDM					;

PTRRS	EQU word ptr				;
BPTRRS	EQU byte ptr				;




sBegin  DATA

;**======================================================================**
;**     External references to static context structures
;**
;** Since all of these are defined in context.c, we don't have to put
;** them under any switch
;**======================================================================**
        EXTRN   grs:byte        ;global register set
        EXTRN   mrsCur:byte     ;module resister set for the active module
        EXTRN   prsCur:byte     ;procedure resister set for the active proc
        EXTRN   txdCur:byte     ;current text table (from prs or mrs)

;**======================================================================**
;**     Fundamental global state variables
;**======================================================================**
        EXTRN   b$mainframe:word        ;initial value of BP after runtime
                                        ; inititalization. Only changes when
                                        ; the stack moves; used to reset the
                                        ; stack
        EXTRN   b$curframe:word ;always equal to BP, except in runtime
                                        ; support code or in U.I. code. BP is
                                        ; modified by runtime entry points and
                                        ; by non-BASIC procedures, so this is
                                        ; used to restore BP after runtime error
        EXTRN   b$pend:word             ;points to the last (bottom) word in
                                        ; the stack. Modified only by the
                                        ; runtime, on initialization & when the
                                        ; stack moves. Used by CLEAR to reset
                                        ; the stack.
        EXTRN   b$pendchk:word          ;Set to b$pend plus a fudge factor
                                        ; used to check to see if sufficient
                                        ; stack space remains at CALL time,
                                        ; GOSUB time, others that push a
                                        ; frame, and by U.I. recursive routines.
                                        ; Reset by the runtime when the stack
                                        ; moves. 
        EXTRN   fNonQBI_Active:word     ;Set TRUE whenever non-QBI code is
                                        ; active. Used to determine whether
                                        ; we can CONTinue, and for special
                                        ; code to blast stack back to last
                                        ; QBI frame when we STOP or ctl-Break
                                        ; with this flag set. 
                                        ; Note that when 'set' this flag serves
                                        ; double duty as it contains the value
					; of BP for the most recent QBI frame.
        EXTRN   bcurlevel_QBI:word      ;Only meaningful when fNonQBI_Active
                                        ; is non-zero. Contains the value of
                                        ; b$curlevel which must be restored
                                        ; whenever we blast the stack back to
                                        ; the most recent QBI frame.
        EXTRN   b$cNonQBIFrames:word    ;Set 0 by [re]init code. Incremented
                                        ; whenever non-QBI code is made active,
                                        ; decremented control is returned from
                                        ; such code to QBI. Used because we
                                        ; currently don't allow the user to
                                        ; Edit and then Continue when non-QBI
					; frames are on the stack.
        EXTRN   b$curlevel:word ;A count of procedure call invocations
                                        ; used to determine which string
                                        ; temps should be released when a
                                        ; runtime error occurs.  Reset by
                                        ; runtime for CLEAR/NEW/runtime
                                        ; initialization.
        EXTRN   pGosubLast:word         ;Pointer to last GOSUB frame on the
                                        ; stack. 0 if no GOSUB's are active.
                                        ; Note that when non-zero, should
                                        ; always be the same as SP at BOS.
                                        ; Reset to 0 whenever the stack is
                                        ; reset; modified by GOSUB & RETURN
                                        ; executors.

ife     CONTEXT_ASM
        EXTRN   oRsDupErr:word          ;Error indicator set by PrsMake and
                                        ; MrsMake for LoadFile
endif   ;CONTEXT_ASM


sEnd    DATA

;**======================================================================**
;**==           External Interface to Context Mgr Component            ==**
;**======================================================================**

ife     CONTEXT_ASM     ;don't include these in context.asm
sBegin  RARE
        EXTRN InitContext:NEAR		
sEnd    RARE

sBegin  CODE
        EXTRN RsActivateCODE:NEAR
sEnd    CODE
endif   ;ife CONTEXT_ASM

sBegin  CP

ife     CONTEXT_ASM     ;don't include these in context.asm
        EXTRN MrsMake:FAR
        EXTRN MrsDeActivate:NEAR
        EXTRN RsActivate:FAR
        EXTRN RsActivateCP:NEAR
        EXTRN MrsActivate:FAR
        EXTRN MrsActivateCP:NEAR
        EXTRN MrsDiscard:FAR
        EXTRN MrsFind:FAR
        EXTRN PrsFind:FAR
        EXTRN PrsRef:NEAR
        EXTRN PrsMake:FAR
	EXTRN PrsDefine:FAR		
        EXTRN PrsDeActivate:NEAR
        EXTRN PrsDeActivateFar:FAR
        EXTRN PrsActivate:FAR
        EXTRN PrsActivateCP:NEAR
        EXTRN NextPrsInMrs:FAR
        EXTRN NextTextPrsInMrs:FAR
        EXTRN GetNextPrsInMrs:NEAR
	EXTRN GetNextPrs:NEAR		
        EXTRN NextMrsFile:FAR
	EXTRN NextMrsFile_All:FAR	
        EXTRN PrsDiscard:FAR
        EXTRN PrsDiscard1:NEAR
        EXTRN PrsFree:FAR
        EXTRN FreeAllUnrefdPrs:FAR
        EXTRN FieldsOfPrs:NEAR
        EXTRN FieldsOfPrsFar:FAR
        EXTRN SetPrsField:NEAR
        EXTRN OMrsORs:NEAR
        EXTRN PPrsOPrs:NEAR
        EXTRN ForEach:FAR
        EXTRN ForEachCP:NEAR
        EXTRN ForEachTxtTbl:NEAR
        EXTRN ForEachTxtTblInMrs:NEAR
        EXTRN ForEachPrsInPlaceCP:NEAR
        EXTRN ForEachPrsInPlaceCPSav:NEAR
	EXTRN OTypeOfTypeCharFar:FAR	
        EXTRN ValidORs:NEAR
endif   ;ife CONTEXT_ASM

ife     CONMISC_ASM     ;don't include these in conmisc.asm
        EXTRN NewStmt:FAR
        EXTRN RunInit:FAR
        EXTRN ContReinit:FAR
        EXTRN ContReinitStat:FAR
        EXTRN ClearStmt:FAR
        EXTRN CantCont:FAR
        EXTRN VarDealloc:NEAR
        EXTRN AdjustITable:FAR
        EXTRN ResetData:NEAR
        EXTRN NextQBI_Frame:FAR
	EXTRN ActiveORs_Frame:FAR
	EXTRN FindORsFrame:NEAR
endif

ife     RSALPHA_ASM
	EXTRN AlphaBuildORs:FAR		
        EXTRN ORsOfAlpha:FAR
        EXTRN NextAlphaPrs:FAR
        EXTRN AlphaORsFree:FAR
endif   ;RSALPHA_ASM



sEnd    CP

sBegin	SCAN			
ife	CONTEXT_ASM		; don't include these in context.asm
	EXTRN PPrsOPrsSCAN:NEAR 
endif				; ife CONTEXT_ASM

ife	CONMISC_ASM		; don't include these in conmisc.asm
	EXTRN FCanCont:NEAR	
endif				; ife CONMISC_ASM

sEnd	SCAN			

;**======================================================================**
;**==           Non-RELEASE and DEBUG routines, in condebug.c          ==**
;**======================================================================**
