;/*
; *                      Microsoft Confidential
; *			 Copyright (C) Microsoft Corporation 1988-1992
; *                      All Rights Reserved.
; */
	page	95,160
	title	himem5.asm - Extended Memory Moves

;
;----------------------------------------------------------------------------
;
; M001 : inserted a jmp $+2 between an out & in while reading the ISR
; M003 : fixed bug to do with not returning int 15h errors on a blockmove
;	call.
; M006 : Preserve _textseg in DS.
;
;----------------------------------------------------------------------------
;
	.xlist
	include	himem.inc
	.list


	extrn	TopOfTextSeg:word
	extrn	hiseg:word
	extrn	pReqHdr:dword
	extrn	dd_int_loc:word
	extrn	interrupt:word

ifdef debug_tsr

	extrn	pVersion:dword
	extrn	pQuery:dword
        extrn   pAlloc:dword
        extrn   pFree:dword
        extrn   pLock:dword
        extrn   pUnlock:dword
        extrn   MoveIt:dword
        extrn   pGetInfo:dword
        extrn   pRealloc:dword

endif

	extrn	fInHMA:byte
	extrn	EndText:byte
_text	ends

funky	segment	word public 'funky'
	assume	cs:funky

	extrn	KiddValley:word
	extrn	KiddValleyTop:word
	extrn	segKiddValley:word
	extrn	end_of_hiseg:word
	extrn	textseg:word

	extrn	CS0:word, CS1:word, LCSS:word, CSDES:word
	extrn	Patch3:word, descCS:byte, OurGDT:byte, GDTPtr:byte

;*----------------------------------------------------------------------*
;*									*
;*   pack_and_truncate - packs everything down into the			*
;*	lowest available memory and sets up variable for driver		*
;*	truncation, then terminates.					*
;*									*
;*----------------------------------------------------------------------*

	public	pack_and_truncate
pack_and_truncate	proc	far

	assume	ds:_text,es:nothing
	push	ds
	mov	dx, offset _text:EndText	; end of text seg
	add	dx, 15
	and	dx, not 15		; size of text seg including init code

	mov	ax, TopOfTextSeg	; end of resident text seg
	or	ax, ax
	jnz	@f
	xor	di, di
	pop	es
	jmp	InitFailed
	
@@:
	add	ax, 15
	and	ax, not 15		; size of resident text seg

	sub	dx, ax			; size of memory whole between
	shr	dx, 4			;  resident text seg and funky seg
					;  The funky seg should be moved down
					;  'dx' number of paragraphs
	mov	ax, hiseg		; Get the current seg at which funky
					;  is running from

	cmp	ax, dx			; If funky is already running from a
					;  segment value less than 'dx'
					;  number of paras funky can be
					;  moved to zero segment only
	jbe	@f
	mov	ax, dx			; ax has min of seg of funky
					;             & memory whole size in para
@@:
	or	ax, ax			; if funky is to be moved by zero
					;  paras our job is over
	jnz	@f
	mov	si, end_of_hiseg	; init regs for entry into
					;  MoveTableHandle label
	mov	di, si
	mov	es, hiseg
	mov	ds, hiseg
	jmp	short MoveHandleTable
@@:
	mov	dx, hiseg		; current segment value of funky
	push	ds
	pop	es
	assume	es:_text
	mov	ds, dx			; which is our source for move
	assume	ds:nothing
	sub	dx, ax			; less the 'paras' to be shrinked
	mov	hiseg, dx		; is the new seg value of funky
	mov	es, dx			; which is our dest. for the move
	assume	es:nothing
	mov	si, HISEG_ORG
	mov	di, si
	mov	cx, end_of_hiseg
	sub	cx, si			; size of funky without ORG
	cld
	rep	movsb			; move it!!!

;;
MoveHandleTable:
	inc	di			; round to word value
	and	di,0fffeh

	mov	si,di
	assume	es:funky
	mov	es:[segKiddValley], es	; segment of handle table
	xchg	si,es:KiddValley	; replace KiddValley with new location
	mov	cx,es:KiddValleyTop
	sub	cx,si
;
COMMENT ^
LBL	label byte
;
LBLOFF	= ( (offset LBL) - (offset MoveBlock286) )
SIZE286	= ( SizeMoveBlock286 + (MAXHANDLES * (size Handle)) )
SIZE386	= ( SizeMoveBlock386 + (MAXHANDLES * (size Handle)) )
IF ( SIZE286 GT LBLOFF )
 %OUT HANDLE TABLE OVERFLOWING INTO INIT CODE ON 286!!!
 .ERR
ENDIF
;;
IF ( SIZE386 GT LBLOFF )
 %OUT HANDLE TABLE OVERFLOWING INTO INIT CODE ON 386!!!
 .ERR
ENDIF
;;
^
	rep	movsb			; move the handle table down
	mov	es:KiddValleyTop,di	; update end of table
	call	mmove_segreinit		; set the memory mover up
	assume	es:nothing

	pop	ds			; restore _text segment
	assume	ds:_text

ifdef debug_tsr

        ; We need to patch up the selector values for the far function
        ; addresses in ControlJumpTable for those functions that live in the
        ; 'funky' segment.

	mov     ax, hiseg
	mov     pVersion.sel, ax
	mov     pQuery.sel, ax
        mov     pAlloc.sel, ax
        mov     pFree.sel, ax
        mov     pLock.sel, ax
        mov     pUnlock.sel, ax
        mov     MoveIt.sel, ax
        mov     pGetInfo.sel, ax
        mov     pRealloc.sel, ax

endif

	add	di,15			; round new segment to paragraph
	and	di,not 15

InitFailed:
ifdef	debug_tsr
	mov	ax,ds			; # paragraphs to keep =
	mov	dx,es			;   (ES - DS) +
	sub	dx,ax			;      (DI >> 4) +
	mov	ax,di			;	 10h
	shr	ax,4
	add	dx,ax
	add	dx,10h			; PSP size
	mov	ax,3100h
	int	21h
else

	push	ds			; save textseg		M006
	lds	si,[pReqHdr]		; discard the initialization code
	mov	word ptr ds:[si].Address[0],di
	mov	word ptr ds:[si].Address[2],es
	mov	ds:[si].Status,100h	; Store return code - DONE
	pop	ds			; restore textseg	M006

	pop	ax			; throw away return from InitDriver

	push	cs
	call	an_iret			; call an iret in our segment

	or	di, di
	jz	we_are_quitting


	mov	dd_int_loc,offset Interrupt	; replace Interrupt with
						; tiny permanent stub

	mov	ax, es:KiddValleyTop	; M006
	sub	ax, es:KiddValley	; M006
	add	ax, es:end_of_hiseg	; M006

	sub	ax, HISEG_ORG		; size of resident funky including
	mov	cs:HMALen, ax

	mov	ax, ((multMULT shl 8)+multMULTGETHMAPTR)
	xor	bx, bx			; in case there is no HMA handler
	int	2fh
	cmp	cs:HMALen, bx
	ja	we_are_quitting

	cmp	di, HISEG_ORG
	ja	we_are_quitting

	mov	bx, cs:HMALen
	mov	ax, ((multMULT shl 8)+multMULTALLOCHMA)
	int	2fh
	cmp	di, 0ffffh
	je	we_are_quitting

	call	MoveHi

we_are_quitting:
	pop	bp
	pop	si
	pop	di
	pop	es
	pop	ds
	pop	dx
	pop	cx
	pop	bx
	pop	ax
	ret				; far return from driver init
endif

pack_and_truncate	endp

HMALen		dw	?		; Length of funky (without init code)


;
;---------------------------------------------------------------------------
;
; procedure : MoveHi
;
;---------------------------------------------------------------------------
;
MoveHi	proc	near
	push	ds			; M006
	push	di			; remember offset in HMA
	mov	si, HISEG_ORG
	mov	cx, cs:HMALen

;	mov	ax, textseg		; DS is already inited to textseg
;	mov	ds, ax

	assume	ds:_text
	mov	ds, hiseg
	assume	ds:nothing
	rep	movsb			; move it to HMA
	pop	di			; get back offset in HMA
	mov	ax, HISEG_ORG
	sub	ax, di
	shr	ax, 1
	shr	ax, 1
	shr	ax, 1
	shr	ax, 1
	mov	bx, es
	sub	bx, ax


;	mov	ax, textseg
;	mov	ds, ax			; get addressability to text seg		

	pop	ds			; M006

	assume	ds:_text
	mov	fInHMA, 1		; Flag that we are running from HMA

	mov	hiseg, bx
	mov	es, bx
	mov	es:[segKiddValley], es

	call	mmove_segreinit		; update the mem mover segments

	mov	di, TopOfTextSeg	; end of resident text code

	mov	ax, ds			; M006

	lds	si, pReqHdr
	assume	ds:nothing

	mov	word ptr ds:[si].Address[0],di
	mov	word ptr ds:[si].Address[2],ax
	
	ret
MoveHi	endp

;
an_iret proc near
	iret
an_iret	endp

;*----------------------------------------------------------------------*
;*									*
;*   mmove_segreinit - sets segment of memory move routine		*
;*									*
;*	es: new segment value						*
;*									*
;*----------------------------------------------------------------------*

	public	mmove_segreinit

mmove_segreinit proc near
	assume	ds:nothing,es:nothing
	call	IsCPU286
	jnz	mmove_segre_386

	assume	es:funky		; note: es = new funky segment
;					;       cs = current funky segment
	mov	ax,es
	mov	es:[cs0],ax			; Patch far jumps to restore
	mov	es:[cs1],ax			; Patch far jumps to restore
	inc	ax				;* Trick 3
	mov	es:[LCSS],ax
	dec	ax
	mov	dx,16
	mul	dx				; Believe or not, this is
	mov	es:[CSDES].seg_base_lo,ax	; faster than a 32 bit shift
	mov	es:[CSDES].seg_base_hi,dl
	ret

;	Install the 386 MoveBlock routine.

mmove_segre_386:
	mov     ax,es			; dx has CS of codeg
	mov     es:[patch3],ax 		; Patch code
	mov     cx,16
	mul     cx
	mov     es:[descCS].LO_apDesc386,ax ; Set up selector for our CS
	mov     es:[descCS].MID_apDesc386,dl
	add	ax,offset OurGDT	; Calculate Base of GDT
	adc     dx,0
	mov     es:[GDTPtr.LO_apBaseGdt],ax
	mov     es:[GDTPtr.HI_apBaseGdt],dx
	assume	es:nothing
	ret

mmove_segreinit endp

;
;------------------------------------------------------------------------
;
; procedure : MmovSegReinit
;
;             Interface rooutine to call mmove_segreinit as a far rtn
;
;------------------------------------------------------------------------
;
		public	MmovSegReinit
MmovSegReinit	proc	far
		call	mmove_segreinit
		ret
MmovSegReinit	endp

;*----------------------------------------------------------------------*
;*									*
;*   IsCPU286 -- returns zero true if CPU is a pre-386 (286)		*
;*									*
;*	Trashes AX                                                      *
;*									*
;*      M006 - preserve state of flags during check
;*----------------------------------------------------------------------*

        public  IsCPU286
IsCPU286 proc	near
	pushf
        pop     ax                      ; Get flags into AX
        or      ax,0F000h		; Set the top bits, preserve the low
	push    ax
	popf
	pushf				; Try and get it back out
	pop     ax
	test    ax,0F000h		; If the top four bits are zero...
	                                ;  it's an 80-286
        ret                             ; Return the Z flag				

IsCPU286 endp


	public	end_of_funky_seg
end_of_funky_seg:
funky	ends
	end



