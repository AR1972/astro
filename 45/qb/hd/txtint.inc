;File: TXTINT.INC - Defines which apply to the QBI Text Manager
;NOTE: When making changes to this file, be sure to make equivalent
;		changes to file TXTINT.H.  See TXTINT.H for comments.

TXTINT_INC = ON			;remember that this file has been included

;Constant for maximum $INCLUDE: nesting depth
INCLUDE_DEPTH_MAX EQU 5

;Constants for parms past to TDataNew
DT_End		EQU 0	;marks end of table bdData
DT_Data		EQU 1	;entry refers to a DATA statement
DT_EndProc	EQU 2	;shows where SUB/FUNCTION fit into main module
DT_Label	EQU 3	;entry refers to label definition referenced by RESTORE
DT_Restore	EQU 4	;entry refers to a RESTORE statement

;----------------------------------------------------------------------
; BigEdit Related variables
; See further comments near TxtStartBigEdit in txtmgr.asm
;----------------------------------------------------------------------
BIG_EDIT_FALSE		EQU 0	;not between StartBigEdit and EndBigEdit
				;default.  set by TxtEndBigEdit()
BIG_EDIT_ACTIVE		EQU 1	;within a "big edit".  set by StartBigEdit()
BIG_EDIT_REPARSE	EQU 2	;TxtEndBigEdit is active, and is parsing all
				;opReParse lines entered during the BigEdit
BIG_EDIT_CANCEL		EQU 3	;user wants to back out of a "big edit"
				;set by TxtChange()/TxtDelete


sBegin	DATA

ife	TXTMGR_ASM
	EXTRN	bdlTxtScrap:byte ;needed for use in txtdata.asm
endif	;TXTMGR_ASM

ife	TXTLOAD_ASM
	EXTRN	chanCur:WORD
	EXTRN	NormFname:Byte
	EXTRN	sdNormFname:Word
	EXTRN	tEtTemp:WORD
	EXTRN	fProcDyn:BYTE
	EXTRN	fDynArrays:BYTE	;Initialized to FALSE by AsciiMerge,
				;set TRUE when $DYNAMIC is seen by TxtChange,
				;set FALSE when $STATIC is seen by TxtChange
	EXTRN	otxUpdLinks:WORD
endif	;TXTLOAD_ASM

sEnd	DATA

sBegin	CODE
ife	TXTDEB_ASM
	EXTRN	tOpWatch:NEAR
endif	;TXTDEB_ASM
sEnd	CODE

ife	TXTFIND_ASM
	EXTRN	OtxNoInclPrev:FAR
endif	;TXTFIND_ASM

ife	TXTMOVE_ASM
endif	;TXTMOVE_ASM

sBegin	CP
ife	TXTDATA_ASM
	EXTRN	TDataStart:NEAR
	EXTRN	TDataEntry:NEAR
	EXTRN	TDataEnd:NEAR
endif	;TXTDATA_ASM

ife	TXTDEB_ASM
	EXTRN	WatchMoved:NEAR
	EXTRN	ClrBpTxt:NEAR
	EXTRN	ChkWatchErr:NEAR
endif	;TXTDEB_ASM

ife	TXTFIND_ASM
	EXTRN	TxtSkipOp:NEAR
	EXTRN	TxtFlushCache:NEAR
	EXTRN	TxtMoved:NEAR
	EXTRN	TxtChkCache:NEAR
endif	;TXTFIND_ASM

ife	TXTLOAD_ASM
	EXTRN	LoadExitProc:NEAR
	EXTRN	LoadEnterProc:NEAR
	EXTRN	MakFilename:near
	EXTRN	OpenChan:near
	EXTRN	CloseChan:near
	EXTRN	SetPsErrMsg:NEAR
	EXTRN	InsertDynDiff:NEAR
	EXTRN	InsertEtDiff:NEAR
	EXTRN	SqueezeDefs:NEAR
endif	;TXTLOAD_ASM

ife	TXTMGR_ASM
	EXTRN	TxtDelete:NEAR
	EXTRN	TxtInsert:NEAR
	EXTRN	TxtInsUpdate:NEAR
 	EXTRN	TxtParseUndo:NEAR
	EXTRN	TxtCopyScrap:NEAR
	EXTRN	TxtInsScrap:NEAR
endif	;TXTMGR_ASM

ife	TXTTHR_ASM
	EXTRN	UpdateLinks:NEAR
	EXTRN	TxtDelThread:NEAR
endif	;TXTTHR_ASM

ife	TXTUTIL_ASM
	EXTRN	TxtReEnter:NEAR
	EXTRN	TblReInclude:NEAR
	EXTRN	DoReParse:NEAR
	EXTRN	ReParseTbl:NEAR
	EXTRN	EtDiff:NEAR
	EXTRN	UpdatePcs:NEAR
	EXTRN	SystemDescanCP:NEAR
	EXTRN	SystemDescanRudeCP:NEAR
	EXTRN	ChkLastAsText:NEAR
	EXTRN	PreScanAsChg:NEAR
endif	;TXTUTIL_ASM

sEnd	CP


; Debug functions
