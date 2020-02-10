;*
;*	COW : Character Oriented Windows
;*
;*	prtsc.asm : ^PrtSc stuff.

	include kernel.inc
	include inscr.inc		;* for inst


;----------------------------------------------------------------------------

externFP	<HandlerInt05Dead>

;----------------------------------------------------------------------------

sBegin	DATA
    assumes DS,DATA

externB	fGrabInt05
externW	OFF_pfnHdlrInt05
externW	SEG_pfnHdlrInt05

sEnd	DATA

;----------------------------------------------------------------------------

sBegin	INIT
    assumes CS,INIT
    assumes SS,DATA
    assumes DS,DATA

;----------------------------------------------------------------------------
;
; Reroute interrupt 05 (^PrtSc) if necessary.
;
;   entry:  ds:bx -> inst structure.
;
;   preserve BX!

segROM	equ	0F000h

    assumes CS,INIT
    assumes DS,DGROUP
    assumes SS,DGROUP
    assumes ES,NOTHING

cProc	ReroutePrtSc,<FAR,PUBLIC,ATOMIC>
cBegin	ReroutePrtSc

	cmp	fGrabInt05,0			; If latch is dropped, int05
	jne	dontgrab			;   is already rerouted okay.

	mov	ax,SEG_pfnHdlrInt05		; AX:DX -> the original
	mov	dx,OFF_pfnHdlrInt05		;   int05 vector.

	cmp	ax,segROM			; If the old int05 didn't
	jne	@F				;   point to ROM, use it.

	test	[bx].finstInst,finstGraphics	; If we're not going to gfx
	jz	@F				;   mode, use old int05.

	mov	ax,SEG kernelBase		; Gfx mode w/ ROM int05:
	mov	dx,kernelOffset HandlerInt05Dead ;   reroute to dead routine.

@@:
	push	ds
	mov	ds,ax				; DS:DX -> new int05 vector.
    assumes ds,NOTHING
	mov	ax,2505h
	int	21h
	pop	ds
    assumes ds,DGROUP

dontgrab:

cEnd	ReroutePrtSc

sEnd	INIT

;----------------------------------------------------------------------------

	END
