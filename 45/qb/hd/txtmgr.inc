; File: TXTMGR.INC
; NOTE: When making changes to this file, be sure to make equivalent
;	changes to file TXTMGR.H
;	See file TXTMGR.H for comments.

TXTMGR_INC = ON


DbTxtFind	MACRO	pTbl
		ENDM
DbChkTxdCur	MACRO
		ENDM


;
; GetSegAddr CODE returns the address of the code segment in ax
; GetSegAddrSCAN CODE does the same, but is used from the SCAN segment
;

GetSegAddr	MACRO	SegName
	EXTRN	Seg_&SegName:abs
	mov	ax,SEG Seg_&SegName
	ENDM

GetSegAddrSCAN	MACRO	SegName
	EXTRN	Seg_&SegName:abs
	mov	ax,SEG Seg_&SegName
	ENDM


SetfDirect MACRO reg,value
ifb	<value>
	mov	[grs.GRS_fDirect],reg
else
	mov	[grs.GRS_fDirect],value
endif
	ENDM

PopfDirect MACRO Reg
	local	RegLet
RegLet	substr	<Reg>,1,1		;; First letter of register
	pop	Reg
%	mov	[grs.GRS_fDirect],RegLet&&l
	ENDM

; macro to set es=seg addr of current text table
; fDirect must be FALSE
GetSegTxtTblCur	MACRO
	GETSEG	es,[txdCur.TXD_bdlText_seg]
	ENDM

; macro to set es=seg addr of current text table
; 
GetSegTxtCur	MACRO
      % call	TxtSegCur&&?segname
	ENDM



THDR_ST			STRUC
THDR_otxLabLink		DW 0
THDR_otxDefTypeLink	DW 0
THDR_otxTypeLink	DW 0
THDR_otxReParseLink	DW 0
THDR_ST			ENDS
THDR_SIZE		EQU size THDR_ST
; THDR_SIZE is the constant offset to 1st byte of pcode in text table

;The global static txtErr contains return values from TxtDirect
TXER_ST		STRUC
TXER_errCode	DW 0	;offset into the QBI Message Table (MSG_xxx) or,
			;if UNDEFINED, ps.bdpError contains the parser-built
			;ASCII error message
TXER_oRs	DW 0	;identifies the text table with the error (only valid
			;if fDirect is FALSE)
TXER_otx	DW 0	;offset into text table where error occurred
TXER_oSrc	DW 0	;UNDEFINED if the error was caught by the scanner.
			;The user interface is responsible for invoking
			;the lister to map the txtErr.otx to a line and
			;column which can be displayed to the user.
			;else The error was caught by the parser.
			;oSrc identifies the column within the source
			;line where the error occurred.
TXER_fDirect	DB 0	;TRUE if error was in direct mode buffer,
			;FALSE if it was somewhere in loaded program
TXER_ST		ENDS


;Macros used to build tables of opcodes used by TxtFind[Next]Op
;See TxtFindNextOp for a description of the format of these tables
;
opTabStart MACRO	tableId
	DW	tableId&_opEot+1	;num entries in table
	opTabCur = 0
	endm

;Highbit set in 1st word of table tells TxtFind... to search the table
;for each opcode, even if opcode's OPA_TxtFind bit is not set.
;This form is used for tables which are not searched at speed sensitive
;times.
;
opTabStartAll MACRO	tableId
	DW	8000h+tableId&_opEot+1	;num entries in table
	opTabCur = 0
	endm

opTabEntry MACRO	tableId,opId
	tableId&_&opId EQU opTabCur+0	;+0 means value, not alias for opTabCur
	opTabCur = opTabCur + 1
	DW opId
	endm

FLoadActive MACRO
	cmp	[cChansOpen],0
	ENDM

;Enumerated parm values for TxtBindPrsS
;
TBIND_Load	EQU 0	;map pcode oNam->oPrs, call PrsDefine
TBIND_Save1	EQU 1	;map pcode oPrs->oNam - called before writing to file
TBIND_Save2	EQU 2	;map pcode oNam->oPrs - called after writing to file

;special lnInsert values for LoadFile(psdFilename, lnInsert)
;
LF_NewProg	EQU 0FFFFH	;do a NEW before the load
LF_NewModule	EQU 0FFFEH	;don't do a NEW before the load
LF_ViewIncl	EQU 0FFFDH	;load $INCLUDE file for editing
LF_NewDoc	EQU 0FFFCH	;load a document into a window

sBegin	DATA
ife TXTMGR_ASM
	EXTRN	flagsTm:byte	;text mgr flags - FTM_xxx
endif
	FTM_reInclude EQU 1	;if set, re-include all include files before
				; next static scan
				;SS_EXECUTE
	FTM_NoSsExecute EQU   2H ;set if no text tables are in SS_EXECUTE
	FTM_PrsDefDeleted EQU 4H ;set if a "defining" prs reference has been
				 ;deleted
	FTM_BpDeleted EQU 8H	;break point has been deleted on Editted line
	FTM_TxtPaste  EQU 10H	;We are in a TxtPaste operation
	FTM_WatchPcode EQU 20H	;Set if Watch Pcode exists anywhere
	FTM_SaveProcHdr EQU 40H ;Set when in critical section while Ascii
				; saving a Proc Header. (a temp text table
				; is active).
	FTM_Linking EQU 80H	;set if LinkAllFiles is executing


sEnd	DATA

;CODE segment publics
;
sBegin	CODE
ife	TXTUTIL_ASM
	EXTRN	DescanOpcode:FAR
endif	;TXTUTIL_ASM
sEnd	CODE


;DATA segment publics
;
sBegin	DATA

;The following masks get ORed into compSwitches by SetCompSwitches()
	COMP_SW_E	EQU 1	;set for each module which has ON ERROR stmt
	COMP_SW_X	EQU 2	;set for each module which has RESUME NEXT stmt
	COMP_SW_V	EQU 4	;set if any module has ON <event> stmt
	COMP_SW_W	EQU 8	;set if any module has ON <event> stmt


	EXTRN	txtErr:word
	EXTRN	txtFindIndex:byte
	;return value set by TxtFindOp (and friends).  If 1st opcode in
	; list was found, 1 if 2nd opcode in list was found etc.
	; (this is only set if parm2 is not NULL on entry)

ife	TXTDEB_ASM
	;number of watch expressions in all loaded modules
	EXTRN	cWatch:word
endif	;TXTDEB_ASM

ife	TXTUTIL_ASM
	EXTRN	fViewInclude:byte	;non-zero if user wants to see INCLUDEd
	EXTRN	fLnNotIncl:byte		;static return value of LnOfOtx,
					; OtxOfLn, OtxBolOfOtx.  zero if given
					; line was an INCLUDEd line
	EXTRN	cInclNest:byte		;$INCLUDE nesting depth.  Non-zero if
					; currently loading an $INCLUDE file
	EXTRN	compSwitches:byte
endif	;TXTUTIL_ASM

ife	TXTLOAD_ASM
	EXTRN	cChansOpen:BYTE
endif	;TXTLOAD_ASM

sEnd	DATA

;CP segment publics
;
sBegin	CP

ife	TXTDEB_ASM
	EXTRN	WatchAdd:FAR
	EXTRN	WatchDel:FAR
	EXTRN	WatchInfo:FAR
	EXTRN	WatchRelease:FAR
	EXTRN	ZeroWatchVal:FAR
	EXTRN	DebugReset:NEAR
	EXTRN	fBpSet:FAR
	EXTRN	ToggleBp:FAR
	EXTRN	ClrBpAll:FAR
	EXTRN	FExecutable:FAR
	EXTRN	fNextStmtDoesIO:FAR
	EXTRN	SkipStop:FAR
endif	;TXTDEB_ASM

ife	TXTDIR_ASM
	EXTRN	TxtDirect:FAR
	EXTRN	SystemScan:FAR
	EXTRN	OtxEndProg:NEAR
endif	;TXTDIR_ASM

ife	TXTFIND_ASM
	EXTRN	LnOfOtx:FAR
	EXTRN	OPrsOfOtx:FAR
	EXTRN	OtxOfLn:FAR
	EXTRN	OtxBolOfOtx:FAR
	EXTRN	OtxBosOfOtx:FAR
	EXTRN	OtxBosNext:FAR
	EXTRN	OtxBolNext0:FAR
	EXTRN	OtxLabOfOtx:NEAR
	EXTRN	OtxNoInclude:FAR
	EXTRN	OtxResume:NEAR
	EXTRN	OtxResumeNext:FAR
	EXTRN	OtxTypDefined:NEAR
	EXTRN	TxtFindOp:NEAR
	EXTRN	TxtFindNextOp:NEAR
	EXTRN	TxtFindOpDS:NEAR
	EXTRN	TxtFindNextOpDS:NEAR
	EXTRN	TxtFindOpExec:FAR
	EXTRN	TxtFindNextOpFar:FAR
	EXTRN	TxtSkipOpFar:FAR
	EXTRN	TxtChkValidOpsExec:FAR

endif	;TXTFIND_ASM

ife	TXTLOAD_ASM
	EXTRN	LoadFile:FAR
	EXTRN	GetLineBd:NEAR
	EXTRN	UpdChanCur:NEAR
	EXTRN	StatusMsgCP:NEAR
	EXTRN	StatusMsg0CP:NEAR
	EXTRN	TxtGrowPsSrcEmScratch:FAR

endif	;TXTLOAD_ASM

ife	TXTMGR_ASM
	EXTRN	TxtChange:FAR
	EXTRN	TxtPaste:FAR
	EXTRN	TxtStartBigEdit:FAR
	EXTRN	TxtEndBigEdit:FAR
	EXTRN	TxtBindPrsS:NEAR
	EXTRN	UndefPrs:NEAR
	EXTRN	ChkAllUndefPrsSaveRs:NEAR
	EXTRN	ChkAllUndefPrs:NEAR
	EXTRN	FreeAllUndefinedPrs:NEAR
endif	;TXTMGR_ASM

ife	TXTMOVE_ASM
	EXTRN	TxtCurInit:NEAR
	EXTRN	TxtDelAll:FAR
	EXTRN	TxtDeleteAll:NEAR
	EXTRN	TxtDiscard:NEAR
	EXTRN	TxtActivate:NEAR
	EXTRN	TxtDeactivate:NEAR
	EXTRN	TxtPrsInit:FAR
	EXTRN	TxtFree:NEAR
	EXTRN	TxtFreeFar:FAR
	EXTRN	TxtMoveDown:NEAR
	EXTRN	TxtMoveUp:NEAR
	EXTRN	TxtMoveUpFar:FAR
	EXTRN	GetWOtx:NEAR
	EXTRN	PutWOtx:NEAR
endif	;TXTMOVE_ASM

ife	TXTSAVE_ASM
	EXTRN	SaveAllDeclares:FAR
	EXTRN	SaveFile:FAR
endif	;TXTSAVE_ASM

ife	TXTTHR_ASM
	EXTRN	TxtDeThread:NEAR
	EXTRN	TxtInsThread:NEAR
	EXTRN	TxtAddThread:NEAR
	EXTRN	ScanTypeBlock:FAR
endif	;TXTTHR_ASM

ife	TXTUTIL_ASM
	EXTRN	AskCantCont_CP:NEAR
	EXTRN	AskRudeEdit:NEAR
	EXTRN	AskRudeEditFar:FAR
	EXTRN	TxtFLnIncluded:FAR
	EXTRN	TxtViewIncl:FAR
	EXTRN	TxtDescan:FAR
	EXTRN	TxtModified:FAR
	EXTRN	TxtDescanCP:NEAR
	EXTRN	SystemDescanRude:FAR
	EXTRN	ModuleRudeEdit:NEAR
	EXTRN	ModuleRudeEditFar:FAR
	EXTRN	SetViewInclude:FAR
	EXTRN	TxtReInclude:FAR
	EXTRN	TxtSegCurCP:NEAR
	EXTRN	TxtTblSegCurCP:NEAR
	EXTRN	OtxDefType:NEAR
	EXTRN	OtxDefTypeCur:NEAR
	EXTRN	OtxDefTypeCurFar:FAR
	EXTRN	OtxDefType0Far:FAR
	EXTRN	OtxDefTypeEot:NEAR
	EXTRN	ONamOVarRudeOrParse:FAR
	EXTRN	ChkLastAs:NEAR
	EXTRN	SetCompSwitches:FAR
endif	;TXTUTIL_ASM


sEnd	CP

sBegin	SCAN
ife	TXTUTIL_ASM
	EXTRN	TxtSegCurSCAN:NEAR
endif	;TXTUTIL_ASM
sEnd	SCAN
