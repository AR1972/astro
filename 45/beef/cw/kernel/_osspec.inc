;*
;*	CW : Character Windows
;*
;*	_osspec.inc : OSSPEC_ENTRIES macro for OS specific jump table
;*			construction


;*******************************************************************************


;*	Entries for OS specific routines go here!

;*	* For easy manipulation all entries must be:	mac	`label',os


OSSPEC_ENTRIES	MACRO	mac,os

;*	* kernel
	mac	BackToCow,os
IFDEF	DEBUG
	mac	CowAssertFailed,os
	mac	CowAssertFailedSz,os
	mac	CowAssertFailedLine,os
	mac	CwOutSz,os
	mac	_cwoutsz,os
ENDIF	;DEBUG
	mac	EndMouse,os
	mac	EndSysTimer,os
	mac	ExitKernel,os
	mac	FEnableMouse,os
	mac	FInitMouse,os
	mac	FInitSysTimer,os
	mac	GetCodeHandle,os
	mac	GlobalAlloc,os
	mac	GlobalFree,os
	mac	GlobalHandle,os
	mac	GlobalLock,os
	mac	GlobalReAlloc,os
	mac	GlobalUnlock,os
	mac	LeaveCow,os
	mac	MouseConditionalOff,os
	mac	MouseShowCursor,os
	mac	ReroutePrtSc,os
	mac	RerrExec,os
	mac	RerrLoadDrv,os
	mac	RerrLoadCwDrv,os
	mac	RerrLoadDrvFd,os
	mac	RerrLoadCwDrvFd,os
	mac	SetMouseCursor,os
	mac	SetMousePos,os
	mac	ThrowStack,os

;*	* kernel/cbox
	mac	GetProgDir,os

;*	* user
	mac	DisableInterrupts,os
	mac	EnableInterrupts,os
	mac	FFindFirst,os
	mac	FFindNext,os
	mac	FindClose,os
	mac	AtrOfPath,os
	mac	GetCurDrive,os
	mac	SetCurDrive,os
	mac	GetCurDir,os
	mac	FSetCurDir,os
	mac	FValidDrive,os

;*	* sbmgr
	mac	BltSb,os
	mac	CbAllocSb,os
	mac	CbReallocSb,os
	mac	CbSizeSb,os
	mac	FInitSegTable,os
IFNDEF	DEBUG
	mac	FInitSegOnce,os
ENDIF	;!DEBUG
	mac	FreeSb,os
	mac	GlobalSize,os
	mac	LcbAllocSb,os
	mac	LcbReallocSb,os
	mac	LpLockEs,os
	mac	LpLockHp,os
	mac	LpReLoadHp,os
	mac	SetEs,os
	mac	SetSbCur,os
	mac	SetSbCur2,os
	mac	UnlockEs,os
	mac	UnlockHp,os

;*	* math
	mac	ChkFor87,os

;*	* inter
IFDEF	DEBUG
;*	* non-debug winter.dmd is os-general
	mac	BltBh,os
	mac	_BltBh,os
	mac	CNatRet,os
	mac	DoJmp,os
	mac	NatRet,os
	mac	PcodeBegin,os
	mac	RetNative0,os
	mac	RetNative1,os
	mac	RetNative2,os
	mac	RetToolBox0,os
	mac	RetToolBox1,os
	mac	RetToolBox2,os
	mac	StartNMeas,os
	mac	StopNMeas,os
	mac	StartUMeas,os
	mac	StopUMeas,os
	mac	XOSExit,os
	mac	LTrickCall,os
	mac	StartCowMeas,os
	mac	StopCowMeas,os
	mac	TrickReturn,os
ENDIF	;DEBUG

;*	* cbox

	mac	AccessFile,os
	mac	BuildDriveTable,os
	mac	CbGlobalCompact,os
	mac	CchHandshakePrint,os
	mac	CchReadDosHnd,os
	mac	CchWriteDoshnd,os
	mac	CloseFile,os
	mac	CreateFile,os
	mac	DateStamp,os
	mac	DeleteFile,os
	mac	Exit,os
	mac	FiniWordTsr,os
	mac	FNetworkDrive,os
	mac	FPrintBuffer,os
	mac	FRemoveableDrive,os
	mac	FSetCodePage,os
	mac	FSetPrinter,os
	mac	FSetPrinterHandshake,os
	mac	GetCodePage,os
	mac	GetCountry,os
	mac	GetDate,os
	mac	GetDiskSpace,os
	mac	GetFileAttr,os
	mac	GetFileLength,os
	mac	GetTime,os
	mac	InChar,os
	mac	InitWordTsr,os
	mac	LszGetEnv,os
;	mac	PromptSwapDisk,os
	mac	ReadFile,os
	mac	ReadMouse,os
	mac	RenameFile,os
	mac	SeekFileSector,os
	mac	FSetFilePointer,os
	mac	VerDOSCur,os
	mac	WinMain,os
	mac	WriteFile,os

	mac	IdentifyWinOldApVersion,os
	mac	ClipboardCompact,os
	mac	CloseClipboard,os
	mac	EmptyClipboard,os
	mac	GetClipboardData,os
	mac	GetClipboardDataSize,os
	mac	OpenClipboard,os
	mac	SetClipboardData,os

ENDM


OSSPEC_OS2ONLY	MACRO	mac,os

ENDM


OSSPEC_DOS3ONLY	MACRO	mac,os

;*REVIEW: is this needed ???
	mac	AccessSwapFile,os
	mac	BindSegment,os
	mac	LockSegment,os
	mac	UnLockSegment,os
	mac	CbFreeMem,os
	mac	PrepareWild,os
	mac	FMatchWild,os
	mac	FValidDir,os
	mac	CbFreeEmm,os
	mac	CbInitEmm,os
	mac	CompactEmm,os
	mac	EndEmm,os
	mac	LcbSizeSb,os
	mac	FRestoreSbState,os
	mac	FInitSbSave,os
	mac	FSaveSbState,os
IFDEF	DEBUG
	mac	FCheckGlobalHeap,os
	mac	InitJmpSeg,os
ENDIF	;DEBUG

ENDM
