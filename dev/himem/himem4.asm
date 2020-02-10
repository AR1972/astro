;/*
; *                      Microsoft Confidential
; *			 Copyright (C) Microsoft Corporation 1988-1992
; *                      All Rights Reserved.
; */
	page	95,160
	title	himem4 - block allocation stuff (common to both 286&386)

	.xlist
	include	himem.inc
	.list

;	The stuff we provide:

	public	Version
	public	GetExtMemoryInfo
	public	LockExtMemory
	public	UnlockExtMemory
	public	end_of_hiseg
	public	textseg
	public	KiddValley
	public	KiddValleyTop
	public	segKiddValley
	public	cHandles

	public	ValidateHandle
	public	BlkMovX

;	externals from himem.asm

	extrn	PrevInt15:dword
	extrn	fHMAMayExist:byte
	extrn	fHMAExists:byte
	extrn	winbug_fix:word
	extrn	FLclEnblA20:far
	extrn	FLclDsblA20:far

        extrn   pAddMem:word

ifdef debug_tsr

	extrn	pQuery:dword
        extrn   pAlloc:dword
        extrn   pFree:dword
        extrn   MoveIt:dword
        extrn   pGetInfo:dword
        extrn   pRealloc:dword

else

	extrn	pQuery:word
        extrn   pAlloc:word
        extrn   pFree:word
        extrn   MoveIt:word
        extrn   pGetInfo:word
        extrn   pRealloc:word

endif

_text	ends

funky	segment	word public 'funky'
	assume	cs:funky,ds:_text

	extrn	mmove_segreinit:near
	extrn	IsCPU286:near
	extrn	end_of_funky_seg:near

ifndef debug_tsr
		org	HISEG_ORG
endif

;-----------------------------------------------------------------
; Following entries must be kept together in the same order

	public	HandleInfo
HandleInfo	label	byte
		db	1		; info structure version #
		db	SIZE Handle	; size of handle entries
cHandles	dw	DEFHANDLES	; number of handles
KiddValley	dw	end_of_funky_seg; The address of the handle table
segKiddValley	dw	seg funky	; segment of handle table
KiddValleyTop	dw	0		; end of handle table

; Above entries must be kept together in the same order
;-----------------------------------------------------------------

	public	LEnblA20
	public	LDsblA20

end_of_hiseg	dw	End286
textseg		dw	_text
LEnblA20	dd	_text:FLclEnblA20
LDsblA20	dd	_text:FLclDsblA20
;*----------------------------------------------------------------------*
;*									*
;*  Get XMS Version Number -				FUNCTION 00h    *
;*									*
;*	Returns the XMS version number					*
;*									*
;*  ARGS:   None							*
;*  RETS:   AX = XMS Version Number					*
;*	BX = Internal Driver Version Number				*
;*	DX = 1 if HMA exists, 0 if it doesn't				*
;*  REGS:   AX, BX and DX are clobbered					*
;*									*
;*  INTERNALLY REENTRANT						*
;*									*
;*----------------------------------------------------------------------*


Version	proc    far

	mov	ax,XMSVersion
	mov	bx,HimemVersion
	xor	dh,dh

;	Is Int 15h hooked?

	cmp	word ptr [PrevInt15][2],0	; Is the segment non-zero?
	jne     VHooked
	mov	dl,[fHMAMayExist]		; No, return the status at
	ret					;  init time.

VHooked:
	mov	dl,[fHMAExists]			; Yes, return the real status
	ret

Version	endp

;*----------------------------------------------------------------------*
;*									*
;*  LockExtMemory -					FUNCTION 0Ch    *
;*									*
;*	Locks a block of extended memory				*
;*									*
;*  ARGS:   DX = 16-bit handle to the extended memory block		*
;*  RETS:   AX = 1 of successful, 0 otherwise.	BL = Error code		*
;*	DX:BX = 32-bit linear address of the base of the memory block   *
;*  REGS:   AX, BX, DX and Flags clobbered				*
;*									*
;*  INTERNALLY NON-REENTRANT						*
;*									*
;*----------------------------------------------------------------------*

LockExtMemory proc far

	cli 				; This is a non-reentrant function

	call    ValidateHandle		; Is the handle valid?
	jnc     LEMBadh
	mov     bx,dx			; Move the handle into BX

;	Are we at some preposterously large limit?

	cmp	[bx].cLock,0FFh
	je	LEMOverflow

	inc	[bx].cLock		; lock the block

	mov	dx, [bx].Base.hi	; return the 32-bit address of base
	mov	bx, [bx].Base.lo

	mov	ax, bx			; save low 16 bits
	shl	dx, 10			; *1024 of hi 16 bits
	shl	bx, 10			; *1024 of low 16 bits
	shr	ax, 06			; take the overflow into account
	or	dx, ax

	mov	ax,1			; return success
	ret

LEMBadh:
	mov	bl,ERR_INVALIDHANDLE
	jmp	short LEMErrExit

LEMOverflow:
	mov	bl,ERR_LOCKOVERFLOW
LEMErrExit:
	xor     ax,ax			; Return failure
	mov	dx,ax
	ret

LockExtMemory endp


;*----------------------------------------------------------------------*
;*									*
;*  UnlockExtMemory -					FUNCTION 0Dh    *
;*									*
;*	Unlocks a block of extended memory				*
;*									*
;*  ARGS:   DX = 16-bit handle to the extended memory block		*
;*  RETS:   AX = 1 if successful, 0 otherwise.	BL = Error code		*
;*  REGS:   AX, BX and Flags clobbered					*
;*									*
;*  INTERNALLY NON-REENTRANT						*
;*									*
;*----------------------------------------------------------------------*

UnlockExtMemory proc far

	cli				; This is a non-reentrant function

	call    ValidateHandle		; Is the handle valid?
	jnc     UEMBadh
	mov	bx,dx			; Move the handle into BX

	cmp	[bx].cLock,0		; is handle locked?
	je	UEMUnlocked		; No, return error

	dec	[bx].cLock		; Unlock the block

	mov     ax,1			; Return success
	xor     bl,bl
	ret

UEMUnlocked:
	mov	bl,ERR_EMBUNLOCKED
	jmp	short UEMErrExit

UEMBadh:
	mov	bl,ERR_INVALIDHANDLE
UEMErrExit:
	xor	ax,ax
	ret

UnlockExtMemory endp

;*----------------------------------------------------------------------*
;*									*
;*  ValidateHandle -							*
;*									*
;*	Validates an extended memory block handle			*
;*									*
;*  ARGS:   DX = 16-bit handle to the extended memory block		*
;*  RETS:   Carry is set if the handle is valid				*
;*  REGS:   Preserved except the carry flag				*
;*									*
;*----------------------------------------------------------------------*

ValidateHandle proc near

	push	ax
	push	bx
	push	cx
	push	dx

	mov	bx,dx		; Move the handle into BX

;	The handle must be equal to or above "KiddValley".

	cmp	bx,[KiddValley]
	jb	VH9f

;	The handle must not be above "KiddValleyTop".

	cmp	bx,[KiddValleyTop]
	ja	VH9f

;	(The handle-"KiddValley") must be a multiple of a handle's size.

	sub	dx,[KiddValley]
	mov	ax,dx
	xor	dx,dx
	mov	cx,SIZE Handle
	div	cx
	or	dx,dx		; Any remainder?
	jnz     VH9f 		; Yup, it's bad

;	Does the handle point to a currently USED block?

	cmp	[bx].Flags,USEDFLAG
	jne     VH9f 		; This handle is not being used.

;	The handle looks good to me...

	stc			; return success
VH9:
	pop	dx
	pop	cx
	pop	bx
	pop	ax
	ret

VH9f:

;	It's really bad.
	clc 			; Return failure
	jmp	short vh9	; we can take a performance hit if it is
				;  a bad handle
	ret

ValidateHandle endp

BlkMovX	proc	near
	assume	ds:_text
	jmp	MoveIt
BlkMovX	endp

.286p
	include	himem4a.asm
.386p
	include	himem4b.asm
.286p	
;*----------------------------------------------------------------------*
;*									*
;*  InstallMoveBlock -					HARDWARE DEP.   *
;*									*
;*	Copies 386 move down on top of 286 move and adjusts end pointer *
;*	  if needed.  Also performs mmove_segreinit to set segment	*
;*	  of the installed move routine.				*
;*									*
;*  ARGS:   None							*
;*  RETS:   None							*
;*  REGS:   AX, CX, DX, DI, SI, ES and Flags are clobbered		*
;*									*
;*----------------------------------------------------------------------*

	public	InstallMoveBlock

SKEW	equ	(offset Begin386 - offset Begin286)

InstallMoveBlock proc far
	assume	ds:_text,es:nothing

	call	IsCPU286
	jz	IMB_done		; done if cpu is 286

	mov     si,offset Begin386
	mov	cx,(offset End386-offset Begin386)
	push	ds			; save _text segment

	push	cs			; es = cs
	pop	es
	push	cs
	pop	ds			; ds = cs
	assume	ds:nothing

	mov	di,offset Begin286
	rep	movsb			; move it!

	pop	ds			; restore ds -> _text
	assume	ds:_text
	mov	end_of_hiseg,di		; save new end of hi segment

        ; For the TSR version, these jump labels point to dwords.  For the
        ; DOS device driver version, these jump labels point to word offsets.

ifdef debug_tsr

        ; All these functions live in the 'funky' segment.  We need to patch
        ; up their selectors to match the new hiseg value after compacting in
        ; pack_and_truncate().

	mov	MoveIt.off, offset MoveExtended386
	mov	pQuery.off, (offset QueryExtMemory - SKEW)
	mov	pAlloc.off, (offset AllocExtMemory - SKEW)
	mov	pFree.off, (offset FreeExtMemory - SKEW)
	mov	pGetInfo.off, (offset GetExtMemoryInfo - SKEW)
	mov	pRealloc.off, (offset ReallocExtMemory - SKEW)

else

	mov	MoveIt, offset MoveExtended386
	mov	pQuery, (offset QueryExtMemory - SKEW)
	mov	pAlloc, (offset AllocExtMemory - SKEW)
	mov	pFree, (offset FreeExtMemory - SKEW)
	mov	pGetInfo, (offset GetExtMemoryInfo - SKEW)
	mov	pRealloc, (offset ReallocExtMemory - SKEW)

endif

        ; pAddMem should be a word offset for both Himem versions - TSR and
        ; DOS device driver.

	mov	pAddMem, (offset AddMem - SKEW)

IMB_done:
	push	cs
	pop	es			; point es: to new segment
	call	mmove_segreinit		; now adjust the segment stuff
	ret				; far return
InstallMoveBlock endp


;*----------------------------------------------------------------------*
;*									*
;*  NOTE: RequestUMB and ReleaseUMB will not be implemented by HIMEM.	*
;*									*
;*----------------------------------------------------------------------*

funky	ends
	end

