;*
;*	COW : Character Oriented Windows
;*
;*	ldreloc.asm : loader relocation
;*	* NOTE : non-conforming procedures (DS == psLom)
;*	* NOTE : this code cloned from loader/loadrelo.asm

	TITLE	LOADRELOC - RelocSegment procedure

	.xlist
	include kernel.inc
	include rlb.inc			;* relocation info
	.list

sBegin	BSS
    assumes DS,DGROUP

externW psLom

sEnd	BSS

;*****************************************************************************

sBegin	KERNEL
    assumes CS,KERNEL
    assumes SS,DGROUP
    assumes DS,NOTHING		;* NOTE : DS == psLom !


;*****************************************************************************

;*	* Jump table for relocations
mprt2pfn	EQU	THIS WORD
	Assert	<rtFixed EQ 0>
	DW	RelocBase			;* rtFixed acts like base
	Assert	<rtOffset EQ 1>
	DW	RelocOffset
	Assert	<rtBase EQ 2>
	DW	RelocBase
	Assert	<rtPtr EQ 3>
	DW	RelocPtr
;*	* if additive (Base only allowed for fixed)
	DW	RelocBaseAdditive

;*****************************************************************************

;********** FRelocSegment **********
;*	entry :
;*		DS == psLom.
;*		pnewseg1 = pointer to a NEW_SEG1
;*		psLoad = physical segment where loaded
;*		segLoaded = 1 based segment # just loaded
;*	* check checksum of segment
;*	* Relocate an already loaded segment
;*	* kernel work buffer contains proper RLB info (lprlb = psLom:offRlbLom)
;*	exit : AX != 0 => ok, AX == 0 => error (checksum error)

cProc	FRelocSegment,<PUBLIC,NEAR>,<SI, DI, DS>
    parmW	pnewseg1
    parmW	psLoad
    parmW	segLoaded
cBegin	FRelocSegment

;{{
;	(lprlb in ds:bx)
;	Assert(lprlb->magic == magicNe);
;	if (segLoaded == lprlb->segSpecial)
;		lprls = &lprlb->rgrls[0];	// rlsSpecial
;	else
;		lprls = &lprlb->rgrls[segLoaded - lprlb->segFirst + 1];
;}}
	mov	bx,ds:[offRlbLom]
	AssertEQ ds:[bx].magicRlb,magicNe
	mov	si,segLoaded
	mov	ax,si
	sub	si,ds:[bx].segFirstRlb	;* si = segLoaded - lprlb->segFirst
	cmp	ax,ds:[bx].segSpecialRlb
	lea	bx,[bx].rgrlsRlb		;* first RLS (rlsSpecial)
	je	use_special_rls
	Assert	<SIZE RLS EQ 4>
	shl	si,1
	shl	si,1
	lea	bx,[bx+si+SIZE RLS]		;* address of sequential one
use_special_rls: ;* ds:bx => RLS

;*	* Check checksum
;{{
;	wSum = lprls->csum;		/* start with total */
;	cw = pnsg->ne_cbseg / 2;
;	lpw = psLoad:0;
;	while (cw--)
;		wSum += *lpw++;
;	if (wSum != 0)
;		return 0;
;}}

	mov	dx,ds:[bx].csumRls		;* wSum
	mov	si,pnewseg1			;* ds:si => NSG info
;*	* Check checksum of code segment
	mov	cx,ds:[si].ns_cbseg		;* in file size
	shr	cx,1
	jcxz	got_checksum

	push	ds
	push	si
	mov	ds,psLoad
	xor	si,si				;* ds:si => code to sum
csum_loop:
	lodsw
	add	dx,ax
	loop	csum_loop
	pop	si
	pop	ds
got_checksum:	;* dx = final sum (should be 0)
	or	dx,dx
	jz	checksum_ok
	xor	ax,ax
	jmp	reloc_end			;* return FALSE if error

checksum_ok:

;{{
;	if (!(pnewseg1->ns_flags & NSRELOC))
;		return;	// no relocation for this one
;}}
	test	ds:[si].ns_flags,NSRELOC
	jnz	reloc_seg
	jmp	reloc_complete			;* no relocations (easy)

reloc_seg:
;{{
;	lpw (ds:si) = lprlb (ds:offRlbLom) + lprls->preloc;
;	crlc = *lpw;				// 1st word is count
;	Assert(crlc != 0);
;	lprlc (ds:si) = lpw;			// followed by relocations
;	while (crlc--)
;		{ /*RelocLoop*/
;		Reloc(lprlc);
;		lprlc++;
;		}
;}}
;*	* ds:bx = lprls (reload DS for each relocation)
	mov	ax,ds:[bx].rltRls		;* ax = rlt
	and	ax,MASK prelocRlt		;* just pointer part
	mov	si,ax
	add	si,ds:[offRlbLom]		;* ds:si = lpw

	lods	word ptr ds:[si]
	mov	cx,ax				;* crlc
	AssertNE cx,0

	mov	es,psLoad			;* es = segment where loaded

RelocLoop:	;* CX = crlc
	push	cx
;{{
;	/* Reloc(lprlc (DS:SI)) - part 1 */
;	rli (cx) = lprlc->rli;
;	offSrc (di) = lprlc->offSrc;
;	Assert(!rli.fImport);
;	/* Determine target value */
;	if (rli.rt == rtFixed)
;		{
;		Assert(rli.seg != 0 && rli.seg <= nexe.ne_cseg);	
;		pnewseg1 (BX) = neLom.ne_segtab[rli.seg];
;		lpDest (DX:AX) = MAKELONG(0, pnewseg1->ns_handle);
;		Assert(!(pnewseg1->ns_flags & NSMOVE));
;		}
;	else
;		{
;		/* moveable */
;		lpDest (DX:AX) = MAKELONG(neLom.ne_enttab[rli.ithunk], neLom);
;		}
;}}
	mov	cx,ds:[si].rliRlc
	mov	di,ds:[si].offSrcRlc

IFDEF DEBUG
	test	cx,MASK fImportRli
	jz	ok_so_far
bad_reloc:
	cCall	CowAssertFailed
	DB	"rlc$"
ok_so_far:
ENDIF ;DEBUG

	Assert	<rtFixed EQ 0>
	test	cx,MASK rtRli
	jnz	entry_moveable

;*	* LOBYTE of RLI is segTarg !
	mov	bl,cl
	xor	bh,bh
IFDEF DEBUG
	or	bx,bx
	jz	bad_reloc
	cmp	bx,ds:[neLom.ne_cseg]
	ja	bad_reloc			;* Error if invalid segno
ENDIF
	Assert	<SIZE NEW_SEG1 EQ 10>
	dec	bx				;* 1 based => zero based
	shl	bx,1
	mov	ax,bx
	shl	bx,1
	shl	bx,1
	add	bx,ax
	add	bx,ds:[neLom.ne_segtab]
	mov	dx,ds:[bx].ns_handle		;* ps of fixed segment
IFDEF DEBUG
;*	* Segment MUST be FIXED !
	test	byte ptr [bx].ns_flags,NSMOVE
	jnz	bad_reloc
ENDIF ;DEBUG
	Assert	<LOW(MASK rtRli) EQ 0>
	jmp	short do_reloc


entry_moveable:
	mov	ax,cx
	and	ax,MASK ithunkRli
	Assert	<SIZE ENTMOVE EQ 6>
	shl	ax,1
	mov	dx,ax
	shl	ax,1
	add	ax,dx				;* times 6
	add	ax,ds:[neLom.ne_rgentmove]	;* point to opcode
	mov	dx,ds				;* dx = ps of entries

do_reloc:
;*	* do relocation : ES:DI => first fixup destination
;*			  DX:AX = fixup value
;*			  CX = rli
	Assert	<MASK rtRli EQ 3000H>
	Assert	<MASK fAddRli EQ 4000H>
	mov	bl,ch
	shr	bx,1				;* *16
	shr	bx,1
	shr	bx,1				;* *2
	and	bx,01110B			;* 1 or 8 vectors
IFDEF DEBUG
;*	* we only have 5 entries
	cmp	bx,01010B
	jae	bad_reloc
ENDIF ;DEBUG
	call	mprt2pfn[bx]			;* do it

;*	* Next relocation please

	add	si,SIZE RLC
	pop	cx				;* crlcRead
	loop	RelocLoop

reloc_complete:
	mov	ax,sp				;* return TRUE
reloc_end:	;* ax = return code

cEnd	FRelocSegment


;*****************************************************************************
;*	* Fixup Routines :

; Offset fixup chain
cProc	RelocOffset,<NEAR, ATOMIC>
cBegin
srsoff1:
	mov	bx,ax
	xchg	es:[di],bx
	mov	di,bx
	inc	bx
	jnz	srsoff1
cEnd

; Segment fixup chain
cProc	RelocBase,<NEAR, ATOMIC>
cBegin
srsseg1:
	mov	bx,dx
	xchg	es:[di],bx
	mov	di,bx
	inc	bx
	jnz	srsseg1
cEnd

; Segment chain fixup Additive
cProc	RelocBaseAdditive,<NEAR, ATOMIC>
cBegin
	add	es:[di],dx
cEnd


; Segment:Offset fixup chain
cProc	RelocPtr,<NEAR, ATOMIC>
cBegin
srsptr1:
	mov	bx,ax
	xchg	es:[di],bx
	mov	es:[di+2],dx
	mov	di,bx
	inc	bx
	jnz	srsptr1
cEnd

;*****************************************************************************

sEnd	KERNEL

	END
