;***
;uiint.inc
;
;	Copyright <C> 1985-1988 Microsoft Corporation
;
; NOTE: When making changes to this file, be sure to make equivalent
;	changes to file UIINT.H
;
;*******************************************************************************

	IncludeOnce	HEAP	

; Window stlye bits used by the user interface
UIWS_MOVECURSOR	EQU	0001H	

;Register Set Types (as passed to RsMake)
RS_scrap	EQU	0	;for Command Window or Scrap
RS_module	EQU	1
RS_includeFile	EQU	2
RS_document	EQU	3
RS_sub		EQU	4
RS_function	EQU	5

;flag parms for GetRsName()
RSN_fFullName	EQU 1
RSN_fIndent	EQU 2

;edit field id's
EFID_Default	EQU	128d	;for dialog boxes with only 1 edit field

;Direct Mode Action flags
FDM_ShowStmt	EQU	1
FDM_GetCmd	EQU	2
FDM_ExecCmd	EQU	4
FDM_ShowWatch	EQU	8

	CBMAX_CALLS	EQU 15d

; Masks to eliminate certain characters or sets of characters in
; a call to GetEditWordMask.  Note that these constants are not
; abitrary, and must relate to the table in UIWORD.ASM.

GEW_DFLTMASK	EQU	5E03h  ; default (normal) search characters
GEW_NODOTMASK	EQU	5E01h  ; Do not include . in search
GEW_HELPMASK	EQU	1E01h  ; Do not include . or ! in search
GEW_VARMASK	EQU	0000h  ; Rules for variable help search
GEW_VARDOTMASK	EQU	0002h  ; variable help with . allowed

hbufHelp	equ	0FFFEh

; Definition of a help buffer	

HelpBufType STRUC
	ContextNum	DD	?		;context number of topic
	bdlHelpText	DB   (SIZE BDL) DUP (?) ;BDL referencing topic
HelpBufType ENDS


helpDefaultId = 9998

; Return codes from routines in uinhelp.asm

HELP_OK        = 0
HELP_NF        = 1
HELP_HANDLED   = 2	; Someone else is responsible for informing the user
HELP_NOVARHELP = 4	; unable to regenerate variable help

; Flag values for HelpFlags

HLP_GOTBUF     = 01h	; Help system is initialized
HLP_NOSHRINK   = 02h	; Do not call HelpShrink in ShrinkHelp.
HLP_COMPRESS   = 04h	; Do not re-open help window when we re-enter UI
HLP_INHELP     = 08h	; In help system, do not recursively reenter.
HLP_VARHELP    = 10h	; We are displaying Variable Help
HLP_FAILOOM    = 20h	; We failed a memory alloc from the help engine
HLP_FAILFNF    = 40h	; We failed a file open in OpenFileOnPath
HLP_RTERR      = 80h	; a RT error (not Interp error) occured.

; Flag values for HelpFlags2

HLP2_DLGBOX    = 01h	; this help will go in a dialog box
; Prefix characters for context strings

PREFIX_MESSAGE = '-'

; base number of Special Interpreter Version Help Ids.	The message box
; that is displayed is identical to a runtime error, but the help topic
; is different

HELP_INTERPBASE = 2000

MAX_SEARCH_PATH	EQU	127+1	; path size + null


;; a-emoryh
MAX_PATH  EQU  80
cDEVICES  EQU  4

DEV_LPT1  EQU  0
DEV_LPT2  EQU  1
DEV_LPT3  EQU  2
DEV_COM1  EQU  3
DEV_COM2  EQU  4

externW iPrintPort
externW fPrintToFile
externB szPrintDest


sBegin	DATA
	extrn	fHistOn:BYTE
	extrn	fGotCmd:word
	extrn	uierr:word
	extrn	wndMain:word
	extrn	wndDebug:word
	extrn	wndHelp:word
	extrn	wndCmd:word
	extrn	wnd1:word
	extrn	wnd2:word
	extrn	pwndTop:word
	extrn	pwndAct:word
	extrn	fWndSplit:byte		
	extrn	fHelpVisible:byte
	extrn	b$fInt24Err:word	

ife UIEDIT_ASM
	extrn	hbufOfCmd:word
	extrn	fDoCmd:word
	extrn	bdEMScratch:byte
endif

ife	HENGINE_ASM			
	extrn	curHelpFile:word	
endif					

sEnd	DATA


sBegin	UI
	extrn	CwInit:near
	extrn	CwHook:near
	extrn	CwUnHook:near
	extrn	CwTerm:near
	extrn	UiRsActivate:near
	extrn	UiRsActivateWnd:near
	extrn	CmdFileSaveAll:near
	extrn	CmdSearchFind:near
	extrn	CmdSearchChange:near
	extrn	MsgBoxStd2:near
	extrn	MsgBoxBd:near		
	extrn	MsgBoxStdBd:near	
	extrn	MsgBoxCompErr:near
	extrn	WndAssign:near
	extrn	WndAssignList:near
	extrn	WndAssignList1:near
	extrn	WndAssignNext:near
	extrn	WndActivate:near
	extrn	RsMake:near
	extrn	GetEditLine:near
	extrn	GetEditColumn:near
	extrn	GetEditWord:near
	extrn	SelectTextPwndCur:near
	extrn	DrawTogglesLnCol:near
	extrn	DoDrawDebugScr:near
	extrn	DoLoadFile:near
	extrn	EnsShowDebugScr:near
	extrn	EnsShowOutputScr:near
	extrn	EnsShowOutSaveRs:near
	extrn	GetCmd:near
	extrn	FlushMsgs:near
	extrn	PromptForString:near
	extrn	DrawDebugScr:near
	extrn	StatusLineMsg:near
        extrn   DoStatusMsg:near

ife UIFILEIO_ASM			
	extrn	ReadQbIni:near		
	extrn	WriteQbIni:near 	
endif	;UIFILEIO_ASM			


ife	UIWORD_ASM			
	extrn	GetEditWordMask:near	
endif					



ife UIMAIN_ASM
	extrn	ExitUserInterface:near
	extrn	EnterUserInterface:near
	extrn	UiGrabSpace:near
	extrn	UiReleaseSpace:near
	extrn	UiAlphaORsBuild:near
	extrn	UiInit:far
endif	;UIMAIN_ASM

ife UIEDIT_ASM
	extrn	CowMoo:near
	extrn	DoCmd:near
	extrn	EditMgrFlush:near
	extrn	EditMgrFlush1:near
	extrn	SetUiErr:near
	extrn	SetUiErrOm:near
	extrn	SetUiErrCond:near	
	extrn	ShowStmt:near
	extrn	fCodeWnd:near
	extrn	GetSelText:near
	extrn	MoveTxdCursor:near
	extrn	CutAll:near
	extrn	PasteAll:near
	extrn	ReAssignBookMark:near	
	extrn	DiscardHelpBookMarks:near
endif	;UIEDIT_ASM

ife UIRS_ASM
	extrn	fCanContUI:near
	extrn	GetRsName:near
	extrn	ContContext:near
	extrn	NeedContContext:near
endif	;UIRS_ASM

ife UIMISC_ASM
	extrn	HookInt24:near
	extrn	UnHookInt24:near
	extrn	FInt24Err:near
	extrn	UIMessageBox:near	
	extrn	MsgBoxStd:near		
	extrn	MsgBoxStdRt:near	
endif	;UIMISC_ASM

ife	UIDEBUG_ASM
	extrn	DrawWatch:near
	extrn	DebugEnd:near
	extrn	DebugError:near
	extrn	DebugStop:near
	extrn	DebugTrace:near
	extrn	WatchInfoUI:near
endif	;UIDEBUG_ASM

ife	UIMEMORY_ASM
	EXTRN	UiStackAlloc:NEAR
	EXTRN	UiStackFree:NEAR
endif	;UIDEBUG_ASM

ife	UIUTIL_ASM
	EXTRN	LongToStdMsg:far
	EXTRN	HexToStdMsg:far
	extrn	fstrcpy2:near		
	extrn	fmemcpy:near		
	extrn	CbSzUi:near		
endif	;UIUTIL_ASM


ife	UINHELP_ASM			
	extrn	GetHelpTitleStr:near	
	extrn	GetHelpContextLen:near	
	extrn	GetHelpMsgSz:near	
	extrn	CmdHelpClose:near	
	extrn	DisplayHlpWndSz:near	
	extrn	DisplayHlpWndNc:near	
	extrn	KeywordHelp:near	
	extrn	KeywordHelpLookup:near	
	extrn	SelectHotLink:near	
	extrn	BufferNc:near		
	extrn	RetrieveHelpHistory:near
	extrn	RecordHelpHistory:near	
	extrn	CreateContext:near	
	extrn	GiveHelpOOM:near	
	extrn	StartHelp:near		
	extrn	SendHelpMsg:near	
endif					

ife	HENGINE_ASM			
	extrn	CloseCurHelpFile:near	
endif					


sEnd	UI
