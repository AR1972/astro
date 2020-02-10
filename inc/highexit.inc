;******************************************************************************
;
;   (C) Copyright MICROSOFT Corp. 1989-1991
;
;   Module:   HIGHEXIT.INC - Code executed after LoadHigh or DeviceHigh
;
;   Date:     May 14, 1992
;
;******************************************************************************
;
;   Modification log:
;
;     DATE    WHO      DESCRIPTION
;   --------  -------  --------------------------------------------------------
;   05/14/92  t-richj  Original
;   06/21/92  t-richj  Final revisions before check-in
;
;******************************************************************************
;
;
; Public:
;
;       PROCEDURES
;		FreeUMBs
;		HeadUMB
;               UnHideUMBs
;
; Internal:
;
;       PROCEDURES
;		freeMCB
;               he_unlink
;		isHideMCB
;               linkumb
;
; Definitions:
;
;       DOS_STRATEGY_SET -      AX value when calling int 21h, to set the DOS
;                               memory-allocation strategy.
;       DOS_UMBLINK_SET -       AX value when calling int 21h, to set the UMB
;                               link-state.
;       DOS_GET_LISTS -         AH value when calling int 21h, to get the DOS
;                               "list of lists".
;       UMB_HeadIdx -           After returning from int 21h, func 52h...
;                               ES:[Umb_HeadIdx] will contain the address of
;                               the first UMB delimiter, usually 0x9FFF... if
;                               it's 0xFFFF, then it hasn't been set yet.
;
; *****************************************************************************

DOS_STRATEGY_GET  equ   5800h	; Int 21h, Func 58h, Svc 0 = get alloc strategy
DOS_STRATEGY_SET  equ   5801h	; Int 21h, Func 58h, Svc 1 = set alloc strategy
DOS_UMBLINK_GET   equ   5802h	; Int 21h, Func 58h, Svc 2 = get link state
DOS_UMBLINK_SET   equ   5803h	; Int 21h, Func 58h, Svc 3 = set link state
DOS_GET_LISTS     equ     52h	; Int 21h, Func 52h = get list of lists

UMB_HeadIdx       equ     8Ch	; Offset from ES (after func52h) to get UMBHead

;
; If HV_LoadHigh has been defined, this is going into stub.asm under
; cmd\command... and stub doesn't include dosmac.inc.  So, rather than
; include dosmac to get BREAK, its definition is just copied here.
;

ifdef HV_LoadHigh

BREAK   MACRO   subtitle	; Arena.inc, below, needs this macro... but
	SUBTTL  subtitle	; if we include dosmac to get it, we've gotta
	PAGE			; include more crap, and so on, and so on, and
ENDM				; so on, and so on, and so on, and so on...

include arena.inc	; For DeviceHigh, highload and highexit are in the
			; same module, so they both have arena.inc; in LoadHigh
endif			; they don't.

; -----------------------------------------------------------------------------
;*** UnHideUMBs - Marks HIDDEN elements as FREE
; -----------------------------------------------------------------------------
; ENTRY:  None; perhaps, earlier, HideUMBs was called... if not, we have
;               very little to do, as no elelments will be marked as HIDDEN.
; EXIT:   Sets InHigh to zero; carry clear if HideUMBs was called earlier.
; ERROR:  None
; USES:   fInHigh (from highvar.inc), carry flag
; -----------------------------------------------------------------------------

	public  UnHideUMBs

UnHideUMBs	proc	near
	push	ax		; Save ax for what we're about to do

; -----------------------------------------------------------------------------
; BUGBUG t-richj 11-8-92: The following six lines were commented out for a good
;    length of time.  Those six constitute a check of whether or not we should
;    indeed clean up the upper-memory chain; without such a check, COMMAND.COM
;    will destroy the current link-state and memory-allocation strategy after
;    every command execution.
; -----------------------------------------------------------------------------

	getdata	al, fInHigh	; Get InHigh from data segment
	or	al, al
	jnz	uhu10		; If didn't call loadhigh/devicehigh earlier,

	pop	ax		; then there's nothing to do here... so
	stc			; restore everything and return.  Just like
	ret			; that.


uhu10:	call	linkumb		; Make sure UMBs are linked in.
	call	FreeUMBs

	putdata	fInHigh, 0	; We're leaving, so update fInHigh.

	call	he_unlink	; Unlink UMBs

	pop	ax
	clc
	ret
UnHideUMBs	endp

; -----------------------------------------------------------------------------
;*** he_unlink - unlinks UMBs if fm_umb is set to 0
; -----------------------------------------------------------------------------
; ENTRY:    fm_umb == 1 : leave linked, else unlink
; EXIT:     None
; ERROR:    None
; USES:     AX, BX
; -----------------------------------------------------------------------------

he_unlink	proc	near
	xor	bh, bh
	getdata	bl, fm_umb		; Restore original link-state
	mov	ax, DOS_UMBLINK_SET
	int	21h

ifdef HV_LoadHigh
	xor	bh, bh
	getdata	bl, fm_strat		; Restore original mem-alloc strategy

	mov	ax, DOS_STRATEGY_SET
	int	21h
endif
	ret
he_unlink	endp

; -----------------------------------------------------------------------------
;*** freeUMBs - frees all HIDDEN memory elements in upper-memory.
; -----------------------------------------------------------------------------
; ENTRY:    None
; EXIT:     None; HIDDEN memory elements returned to FREE
; ERROR:    None (ignore CF)
; USES:     Flags
; -----------------------------------------------------------------------------

	public	FreeUMBs

FreeUMBs	proc	near
	pushreg	<ax, es>

	call	HeadUmb		; Returns with carry if err, else ES == MCB
	jc	fusX

	mov	es, ax		; Prepare for the loop; ES = current MCB addr.

fus10:	call	isHideMCB	; Returns with ZF set if owner is 0
	jnz	fus20
	call	freeMCB

fus20:	mov	al, es:[arena_signature]
	cmp	al, arena_signature_end
	jz	fusX		; That means this was the last MCB--that's it.

	mov	ax, es
	add	ax, es:[arena_size]
	inc	ax
	mov	es, ax		; Go on forward.
	jmp short fus10

fusX:	popreg	<es, ax>
	ret
FreeUMBs	endp

; -----------------------------------------------------------------------------
;*** isHideMCB - returns with ZF set if current MCB (ES:0) is HIDDEN
; -----------------------------------------------------------------------------
; ENTRY:    ES:0 should point to an MCB
; EXIT:     ZF set if MCB is hidden, else !ZF
; ERROR:    None
; USES:     Flags
; -----------------------------------------------------------------------------

isHideMCB	proc	near
	push	ax

	cmp	es:[arena_owner], SystemPSPOwner	; If the owner's SYSTEM
	jnz	ihm_x					; then check for HIDDEN

	mov	ax, word ptr es:[arena_name]
	cmp	ax, 'IH'
	jnz	ihm_x
	mov	ax, word ptr es:[arena_name+2]
	cmp	ax, 'DD'
	jnz	ihm_x
	mov	ax, word ptr es:[arena_name+4]
	cmp	ax, 'NE'
	jnz	ihm_x
	mov	ax, word ptr es:[arena_name+6]
	cmp	ax, '  '

ihm_x:	pop	ax
	ret
isHideMCB	endp

; -----------------------------------------------------------------------------
;*** freeMCB - marks as free the MCB at ES:0
; -----------------------------------------------------------------------------
; ENTRY:    ES:0 should point to an MCB
; EXIT:     None; MCB free'd
; ERROR:    None
; USES:     AX
; -----------------------------------------------------------------------------

freeMCB	proc	near
	mov	es:[arena_owner], 0
	mov	ax, '  '
	mov	word ptr es:[arena_name+0], ax
	mov	word ptr es:[arena_name+2], ax
	mov	word ptr es:[arena_name+4], ax
	mov	word ptr es:[arena_name+6], ax
	ret
freeMCB	endp

; -----------------------------------------------------------------------------
;*** HeadUmb - returns in AX the address of the first UMB block (0x9FFF)
; -----------------------------------------------------------------------------
; ENTRY:  Nothing
; EXIT:   AX contains 0x9FFF for most systems
; ERROR:  Carry set if pointer is 0xFFFF (if not set up yet--DH runs into this)
; USES:   Flags, AX
; -----------------------------------------------------------------------------

	public	HeadUmb

HeadUmb	proc	near
	pushreg	<si, ds, es>

	mov	ah, DOS_GET_LISTS	; Call int 21h, function 52h...
	int	21h

	mov	ax, es:[UMB_HeadIdx]	; And read what's in ES:008C
	cmp	ax, 0FFFFh
	jz	xhu_e			; If it's 0xFFFF, it's an error...

	clc				; Else, it isn't.
	jmp short xhu_x

xhu_e:	stc

xhu_x:	popreg	<es, ds, si>
	ret
HeadUmb	endp

; -----------------------------------------------------------------------------
;*** linkumb - links UMBs not already linked in; updates fm_umb as needed
; -----------------------------------------------------------------------------
; ENTRY:    None
; EXIT:     fm_umb == 0 if not linked in previously, 1 if already linked in
; ERROR:    None
; USES:     AX, BX, fm_umb
; -----------------------------------------------------------------------------

linkumb	proc	near
	mov	ax, DOS_UMBLINK_GET
	int	21h			; Current link-state is now in al

	or	al, al			; BUGBUG: proper check?
	jnz	lumbX			; Jumps if UMBs already linked in

	mov	ax, DOS_UMBLINK_SET
	mov	bx, 1
	int	21h

lumbX:
	ret
linkumb	endp

