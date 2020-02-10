;*
;*
;*	stubs.asm	Stubs for OS specific routines that exist under
;*			one OS only.
;*

;*******************************************************************************

	include	kernel.inc

;*******************************************************************************


sBegin	KERNEL

	PUBLIC	AccessSwapFileOS2
	PUBLIC	BindSegmentOS2
	PUBLIC	LockSegmentOS2
	PUBLIC	UnLockSegmentOS2
	PUBLIC	CbFreeMemOS2
	PUBLIC	PrepareWildOS2
	PUBLIC	FMatchWildOS2
	PUBLIC	FValidDirOS2
	PUBLIC	CbFreeEmmOS2
	PUBLIC	CbInitEmmOS2
	PUBLIC	CompactEmmOS2
	PUBLIC	EndEmmOS2
	PUBLIC	LcbSizeSbOS2
	PUBLIC	FRestoreSbStateOS2
	PUBLIC	FInitSbSaveOS2
	PUBLIC	FSaveSbStateOS2

IFDEF	DEBUG
	PUBLIC	FCheckGlobalHeapOS2
	PUBLIC	InitJmpSegOS2
ENDIF	;DEBUG

assumes	CS,KERNEL
assumes	DS,nothing
assumes	ES,nothing


CbFreeMemOS2:
CbFreeEmmOS2:
CompactEmmOS2:
EndEmmOS2:
FRestoreSbStateOS2:
	retf

AccessSwapFileOS2:
LockSegmentOS2:
UnLockSegmentOS2:
FValidDirOS2:
LcbSizeSbOS2:
FInitSbSaveOS2:
FSaveSbStateOS2:
IFDEF	DEBUG
InitJmpSegOS2:
ENDIF	;DEBUG
	retf	2

PrepareWildOS2:
FMatchWildOS2:
IFDEF	DEBUG
FCheckGlobalHeapOS2:
ENDIF	;DEBUG
	retf	4

BindSegmentOS2:
CbInitEmmOS2:
	retf	6


sEnd	KERNEL


;*******************************************************************************

	END
