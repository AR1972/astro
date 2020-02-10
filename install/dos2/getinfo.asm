;============================================================================
	title	GETINFO
;============================================================================
;
;   (C) Copyright MICROSOFT Corp. 1991-1992
;
;   Title:    DOS2.EXE - GUI Portion of DOS Install
;
;   Module:   GETINFO - Routine that returns a pointer to the vInfo data 
;			data created by the CUI portion of DOS Install
;
;   Version:  0.001
;
;   Date:     Jan 26,1992
;
;   Author:   HKN
;
;============================================================================
;
;   Change log:
;
;     DATE    REVISION			DESCRIPTION
;   --------  --------	-------------------------------------------------------
;   01/26/92  Original
;
;============================================================================

include model.inc
include dpmi.inc

.data

RealModeCallFrame	RealModeCallStruc	<>

.code

;===========================================================================
;
;	Procedure	: GetInfo
;
;	Input		: None
;	Output		: Returns a Selector:Offset to the Vinfo Data
;
;	Description:
;		
;		1. Simulate Real Mode Int 2fh ax=4910h
;		2. Build a selector for the segment indiacted by ES in the
;		   Real Mode Call Structure.
;		3. Return the Selector:Offset
;
;===========================================================================

GetInfo	proc	uses es di bx cx

	;
	; Initialize the RealModeCallFrame with the inout registers we want 
	; to pass to the int 2f call. Note we are going to initialize RegSS
	; and RegSP to 0. This will make the DPMI server provide a 30 word 
	; stack which is sufficient for our purposes.
	;
	lea	di, [RealModeCallFrame]
	mov	word ptr [di].RegEAX, 4910h
	xor	ax, ax
	mov	[di].RegSS, ax
	mov	[di].RegSP, ax
	pushf
	pop	ax
	mov	[di].RegFlg,ax
	
	;
	; Simulate the Real Mode Int.
	;
	mov	ax, ds
	mov	es, ax			; es:di = RealModeCallStruc
	mov	bx, 02fh		; bl = int number
	xor	cx, cx
	mov	ax, SIMULATE_RELMODE_INT
	int	DPMI
	jc	GIerror

	;
	; The Int 2f was successful. At this point the registers returned by 
	; the real mode int call are in the RealModeCallStruc. The expected
	; output from the int 2f should be: AX = -1 and ES:BX points to the 
	; vInfo data.
	;
	mov	ax, word ptr [di].RegEAX
	cmp	ax, -1
	jne	GIerror

	;
	; We now need to map the segment in RegES to a selector.
	;
	mov	cx, 1			; one descriptor
	mov	ax, ALLOC_LDT_DESC
	int	DPMI
	jc	GIerror
	
	mov	bx, ax			; bx = selector
	mov	ax, [di].RegES
	mov	dx, ax
	mov	cl, 12
	shr	ax, cl
	mov	cl, 4
	shl	dx, cl
	mov	cx, ax			; cx:dx = 32 bit linear base address
	mov	ax, SET_SEG_BASE_ADDR
	int	DPMI			; set the base address of the desc
	jc	GIerror

	mov	dx, 0ffffh
	xor	cx, cx			; cx:dx = 64K limit
	mov	ax, SET_SEG_LIMIT
	int	DPMI
	jc	GIerror

	mov	dx, bx
	mov	ax, word ptr [di].RegEBX	
	ret

GIerror:
	;
	; If error return NULL
	;
	xor	dx, dx
	xor	ax, ax
	ret

GetInfo	endp

	end
	
	





