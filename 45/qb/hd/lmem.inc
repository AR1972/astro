;***
;lmem.inc - LMEM constants and entry points
;***
	.XLIST
;***
;
;	Copyright <C> 1988, Microsoft Corporation
;
;Purpose:
;	NOTE: This is a SHARED INCLUDE FILE, used by both the RUNTIME
;	NOTE: and the interpreter projects.  Any changes made to one should
;	NOTE: be carried over to the other!!!
;	NOTE: There are some minor differences between the two versions for
;	NOTE: this file, so DO NOT just copy it!!!
;
;
;******************************************************************************

LMEM_INC = ON
	IncludeOnce	sb

fcmpNoCompact		equ	0
fcmpCompact		equ	1
fcmpCompactHandles	equ	2
merrAllocMoveable	equ	1
merrAllocFixed		equ	2
merrReallocMoveable	equ	3
merrAllocBlock		equ	4

;Special Zeros
pvZero		equ	12H	;a pointer to 0 length block, 1st word is 0
ppvZero		equ	14H	;a pointer to pvZero

;Entry points
extrn	CreateHeap:far		;(SB)
extrn	CbCompactHeap:far	;(SB, WORD)
extrn	PpvAllocCb:far		;(SB, WORD)
extrn	FReallocPpv:far		;(SB, VOID **, WORD)
extrn	FreePpv:far		;(SB, VOID **)
extrn	CbSizePpv:far		;(SB, VOID **)

extrn	HFirstAllocBlock:far	;(SB, WORD)
extrn	FreeHandleBlock:far	;(SB, VOID **)


comment	| Unused entry points are kept below, in case they're needed later

extrn	PvAllocFixedCb:far	;(SB, WORD)
extrn	CbSizeFixedPv:far	;(SB, VOID *)
extrn	LockHeap:far		;(SB)
extrn	UnlockHeap:far		;(SB)

extrn	FCreateWindowsHeap:far	;(SB, WORD)
extrn	SwapHandles:far		;(SB, VOID **, VOID **)

;Debug entry points
extrn	CheckHeap:far		;(SB)
extrn	ShakeHeapSb:far		;(SB)
extrn	GetHeapInfo:far		;(SB, CKL FAR *)
extrn	FCheckHandle:far	;(SB, VOID **)
extrn	PpvWalkHeap:far		;(SB, VOID **)
extrn	PvWalkFixedHeap:far	;(SB, VOID *)
	|
