.386p
page	58,132
;******************************************************************************
	title	xmsutils.asm - XMS utility functions
;******************************************************************************
;
; (C) Copyright MICROSOFT Corp. 1992
;
; Title:	EMM386.EXE - MICROSOFT Expanded Memory Manager 386 Driver
;
; Module:	XMS memory utility functions
;
; Date: 	May 18, 1992
;
;******************************************************************************
;
;   Change log:
;
;     DATE    REVISION			DESCRIPTION
;   --------  --------	-------------------------------------------------------
;   5/18/92	1	initial code
;******************************************************************************
;
;   Functional Description:
;	This module implements utilities for the EMS/XMS pool sharing code
;	added to EMM386.
;
;******************************************************************************

page
;******************************************************************************
; INCLUDES
;******************************************************************************
include	vdmseg.inc
include vdmsel.inc
include vm386.inc
include	page.inc
include	emmfunct.inc
include	emmdata.inc
include emm386.inc
include debmac.inc

;******************************************************************************
; EXTERNALS
;******************************************************************************

_TEXT	segment
	extrn	CallRealModeRtn:near
ifdef DEBUG
	extrn	pTestDbgIns:far
	extrn	pDebugPrintf:far
endif
_TEXT	ends

R_CODE	segment
	extrn	XMMcontrol:DWORD
ifdef DEBUG
	extrn	rTestDbgIns:far
	extrn	rDebugPrintf:far
endif
R_CODE	ends

;******************************************************************************
; EQUATES / STRUCTURES
;******************************************************************************

MAX_NUM_XMS_BLKS   equ	20	; Really weird to have more XMS blocks than this
MIN_XMS_BLK_SIZE   equ	36	; Don't use XMS blocks smaller than this (in k)
MIN_XMS_ALLOC_SIZE equ	256	; Try to alloc at least 256k XMS blocks

; Control structure for each XMS block allocated

XMS_Block	struc
XMS_Next	dd	?	; next XMS_Block in linked list
XMS_Handle	dw	?	; XMS handle to block
XMS_Base_Page	dd	?	; address of 1st aligned 4k page in block
XMS_Total_Pages dw	?	; total # 4k pages in block
XMS_Free_Pages	dw	?	; # 4k pages currently free in block
XMS_Free_Head	dw	?	; index of 1st/next free 4k page in block
XMS_Free_List	dw	?	; linked list of free 4k pages in block
XMS_Block	ends


;******************************************************************************
; LOCAL DATA
;******************************************************************************

_DATA	segment

XMS_Block_List	dd	0	; head of XMS_Block struc linked list
XMS_Block_Cnt	dw	0	; # XMS blocks allocated

; Sizes of XMS blocks to allocate in 16k EMS pages

XMS_Block_Size	label	word
		dw	256 / 16		; 256 k bytes / 16 k pages
		dw	512 / 16
		dw	1024 / 16
		dw	2048 / 16
		dw	4096 / 16
		dw	8192 / 16
		dw	16384 / 16
XMS_SIZE_ENTRIES equ	($ - XMS_Block_Size) / 2


_DATA	ends

page
;=============================================================================
;==	C O D E  S E G M E N T
;=============================================================================
_TEXT	segment
assume	cs:_TEXT,ds:_DATA,ss:STACK,es:ABS0,gs:R_CODE
page
align 16

;***********************************************************************
; QueryXMSpages:    This routine determines if a specified number of
;	XMS "pages" (16k each) are available.
;
; Entry: (Protected Mode)
;	BX = Number of XMS pages desired.
; Exit:
;	CY = clear if desired # XMS pages are available,
;	CY = set if not enough XMS pages available, plus
;		AX = number of XMS pages actually available
; Used:
;	AX
;***********************************************************************
	public	QueryXMSpages

QueryXMSpages proc  near

	AssertSegReg	ds, VDMD_GSEL
	AssertSegReg	es, DATA32_GSEL
	AssertSegReg	gs, RCODEA_GSEL

	;pDebugF "QueryXMSpages: wanted %d, found ", bx

	push	bx
	push	cx
	push	dx
	push	esi

;  See if there are enough free pages in already allocated XMS blocks

	xor	ax, ax

;EnterCrit				; BEGIN CRITICAL SECTION
pushf
cli
	mov	esi, [XMS_Block_List]
QFXp_scan:
	or	esi, esi
	jz	short QFXp_not_enough

	add	ax, es:[esi].XMS_Free_Pages
	cmp	ax, bx
	jae	short QFXp_got_them

	mov	esi, es:[esi].XMS_Next
	jmp	short QFXp_scan

;  Find out how may pages the XMS driver can give us

QFXp_not_enough:

;ExitCrit				; END CRITICAL SECTION
popf
	sub	bx, ax				; need this many more pages
	mov	dx, ax				; this many we already have
	mov	ax, QUERY_XMS_PAGES
	call	CallRealModeRtn

	cmp	ax, bx				; get all we needed?
	jae	short QFXp_got_them2

	add	ax, dx				; what is avail + what we have
	stc
	jmp	short QFXp_exit

QFXp_got_them:

;ExitCrit				; END CRITICAL SECTION
popf

QFXp_got_them2:
	clc

QFXp_exit:
	;pDebugF "%d\n", ax

	pop	esi
	pop	dx
	pop	cx
	pop	bx

	ret

QueryXMSpages endp


;***********************************************************************
; AllocateXMSpages:	This routine allocates a requested number of XMS
;	"pages" (16k each) and fills in page table entries with their
;	address.
;
; Entry: (Protected Mode)
;	BX = Number of 16k XMS pages desired.
; Exit:
;	CY = clear if desired # XMS pages allocated.
;		Page table entries set.
;		[TopOfFreeEMSspace] updated.
;	CY = set if not enough XMS pages available.
; Used:
;
;***********************************************************************
	public	AllocateXMSpages

AllocateXMSpages proc	near

	AssertSegReg	ds, VDMD_GSEL
	AssertSegReg	es, DATA32_GSEL
	AssertSegReg	gs, RCODEA_GSEL

	;;;pDebugF "AllocateXMSPages: %d pages\n", bx

	pushad
	mov	bp, sp

AXp_scan_again:

;  Find out how many pages are available in existing XMS blocks

	xor	ax, ax

;EnterCrit				; BEGIN CRITICAL SECTION
pushf
cli
	mov	esi, [XMS_Block_List]
	or	esi, esi		; Q: any XMS blocks yet?
	jz	AXp_not_enough		; N:

AXp_scan:
	add	ax, es:[esi].XMS_Free_Pages
	mov	esi, es:[esi].XMS_Next
	or	esi, esi
	jnz	short AXp_scan

;  If there are enough free pages, allocate them

	mov	bx, word ptr [bp].Pushad_ebx	; # pages to assign

	cmp	ax, bx			; Q: enough pages available?
	jb	AXp_not_enough		; N:

	mov	ax, [BotOfVCPIspace]	; double check that there is enough
	sub	ax, [TopOfFreeEMSspace] ;   handle space for this allocation
	shr	ax, 2			; 4k to 16k pages
	cmp	ax, bx
	jb	AXp_no_handles

	mov	esi, [XMS_Block_List]
	mov	ecx, [page_directory]
	movzx	edi, [TopOfFreeEMSspace]
	lea	edi, [ecx][edi*4]	; es:[edi] -> TopOfFreeEMSspace

AXp_alloc_block:

	mov	cx, es:[esi].XMS_Free_Pages
	jcxz	AXp_next_block

	cmp	cx, bx				; Q: enough free pages in this
	jbe	short AXp_take_pages		;    block for remainder?
	mov	cx, bx				; Y: just take what we need
AXp_take_pages:
	sub	bx, cx
	sub	es:[esi].XMS_Free_Pages, cx

	push	bx				; remaining page count
	mov	ebx, es:[esi].XMS_Base_Page
	or	bx, P_AVAIL OR fXMSPageAllocated

	movzx	edx, es:[esi].XMS_Free_Head	; index of 1st free page

	cld

AXp_set_PTE_loop:

IFDEF DEBUG
	cmp	dword ptr es:[edi], 0
	jz	short @f
	INT	3
@@:
ENDIF
	mov	eax, edx
	shl	eax, 14 			; free page index to offset
	add	eax, ebx			; page = base + free page offset

	STOS_DWORD_PTR_ES_EDI			; store PTE
	add	eax, PAGE_SIZE
	STOS_DWORD_PTR_ES_EDI			; store PTE
	add	eax, PAGE_SIZE
	STOS_DWORD_PTR_ES_EDI			; store PTE
	add	eax, PAGE_SIZE
	STOS_DWORD_PTR_ES_EDI			; store PTE

	movzx	edx, es:[esi][edx*2].XMS_Free_List ; index of nxt free page

	loop	AXp_set_PTE_loop

	mov	es:[esi].XMS_Free_Head, dx	; update free page list

	pop	bx				; remaining page count
	or	bx, bx				; Q: all pages assigned?
	jz	short AXp_assigned_all		; Y:

AXp_next_block:

	mov	esi, es:[esi].XMS_Next

ifdef DEBUG
	or	esi, esi
	jnz	AXp_alloc_block
	; something wrong if we get here!
	pDebugBreak
else
	jmp	short AXp_alloc_block
endif

AXp_no_handles:
;ExitCrit				; END CRITICAL SECTION
popf
	jmp	short AXp_alloc_failed

AXp_assigned_all:

	sub	edi, [page_directory]
	shr	di, 2			; index back to PTE
	mov	[TopOfFreeEMSspace], di ; update top of EMS memory

;ExitCrit				; END CRITICAL SECTION
popf
	clc
	jmp	short AXp_exit


;  Try to get new XMS_Block(s) for the remaining pages

AXp_not_enough:

;ExitCrit				; END CRITICAL SECTION
popf
	mov	bx, word ptr [bp].Pushad_ebx
	sub	bx, ax			; number additional 16k pages needed

	call	AllocateXMSblock
	jnc	AXp_scan_again		; if alloc'd something, see if we can
					;   fulfill the page request now

;  Couldn't get all the pages wanted, but some XMS may have been allocated so
;  check the XMS_Block list and free any with no used pages

AXp_alloc_failed:

	call	ReleaseXMSblocks

AXp_fail:
	stc

AXp_exit:
	popad

	ret

AllocateXMSpages endp


;***********************************************************************
; FreeXMSpages: This routine scans free EMS space and releases free pages
;	that were dynamically allocated from XMS memory.
;
; Entry: (Protected Mode)
;
; Exit:
;	Page table entries cleared.
;	[TopOfFreeEMSspace] updated.
; Used:
;
;***********************************************************************
	public	FreeXMSpages

FreeXMSpages	proc	near

	AssertSegReg	ds, VDMD_GSEL
	AssertSegReg	es, DATA32_GSEL
	AssertSegReg	gs, RCODEA_GSEL

	pushad

;  Pack all free XMS page PTEs at the top of free space so their pages
;  can be released.  This has to be done so [TopOfFreeEMSspace] can be
;  moved down.

	xor	edi, edi
	mov	edx, [page_directory]

;EnterCrit				; BEGIN CRITICAL SECTION
pushf
cli
	; Scan EMS space top down to find the 1st non XMS allocated page

	movzx	esi, [TopOfFreeEMSspace]
	mov	bx, [TopOfUsedEMSspace]

FXp_scan_non_xms:
	cmp	si, bx				; Q: out of free space?
	jbe	short FXp_pack_done		; Y: finished
	bt	dword ptr es:[edx][esi*4-4*4], fXMSPageAllocatedBit
	jnc	short FXp_got_non_xms
	sub	si, 4
	jmp	short FXp_scan_non_xms

	; Scan for the first/next XMS page

FXp_got_non_xms:
	or	di, di				; Q: 1st time?
	jz	short FXp_1st_non_xms		; Y:

FXp_scan_xms:
	cmp	di, bx				; Q: out of free space?
	jbe	short FXp_pack_done		; Y: finished
	bt	dword ptr es:[edx][edi*4-4*4], fXMSPageAllocatedBit
	jc	short FXp_swap_PTE
	sub	di, 4
	jmp	short FXp_scan_xms

	; Swap XMS/non XMS entries so XMS entries at top

FXp_swap_PTE:
	mov	cx, 4				; 4 PTE entries per EMS page
FXp_swap_loop:
	mov	eax, es:[edx][edi*4-1*4]	; swap PTE entries
	xchg	eax, es:[edx][esi*4-1*4]
	mov	es:[edx][edi*4-1*4], eax
	dec	si
	dec	di
	loop	FXp_swap_loop
	jmp	short FXp_scan_non_xms

FXp_1st_non_xms:
	lea	di, [si-4]			; start looking for XMS pages
	jmp	short FXp_scan_xms		;   from below the non XMS page

FXp_pack_done:

;ExitCrit				; END CRITICAL SECTION
popf
	nop				; interrupt window
	nop
	nop

;  Now free the XMS pages at the top of free space.  We have to scan from
;  top down again because there is the possibility that things changed
;  during the interrupt window above.

;EnterCrit				; BEGIN CRITICAL SECTION
pushf
cli
	; Find the last XMS page in free space from top down

	movzx	esi, [TopOfFreeEMSspace]
	mov	bx, [TopOfUsedEMSspace]

FXp_get_last_xms:
	cmp	si, bx
	jbe	short FXp_got_xms_range
	bt	dword ptr es:[edx][esi*4-4*4], fXMSPageAllocatedBit
	jnc	short FXp_got_xms_range
	sub	si, 4
	jmp	short FXp_get_last_xms

	; Release the pages from PTE esi to [TopOfFreeEMSspace] - 1

FXp_got_xms_range:

	mov	bx, [TopOfFreeEMSspace] ; bx = old TopOfFreeEMSspace
	mov	[TopOfFreeEMSspace], si ; si = new ...

	lea	edi, [edx][esi*4]	; edi -> PTE for 1st page to free
	mov	cx, bx
	sub	cx, si			; cx = # 4k pages to free
	jbe	FXp_free_done

	shr	cx, 2			; cx = # 16k pages to free

	mov	esi, [XMS_Block_List]	; esi -> xms block info
ifdef DEBUG
	or	esi, esi
	jnz	short @f
	pDebugBreak
	jmp	FXp_free_done
@@:
endif
	mov	ebx, es:[esi].XMS_Base_Page	; ebx = base addr 4 this block

	cld

FXp_free_page:
	mov	eax, es:[edi]			; 1st PTE of page to free

ifdef DEBUG
	bt	eax, fXMSPageAllocatedBit
	jnc	short FXp_Bad_PTE
endif
	sub	eax, ebx			; Q: Is the page within the
	jb	short FXp_wrong_block		;    range of this XMS block?
	shr	eax, 14
	cmp	ax, es:[esi].XMS_Total_Pages
	jae	short FXp_wrong_block

FXp_got_block:
	mov	dx, ax				; Y: link it to the head of
	xchg	dx, es:[esi].XMS_Free_Head	;    the free page list
	mov	es:[esi][eax*2].XMS_Free_List, dx

	inc	es:[esi].XMS_Free_Pages 	; another 16k free page

	xor	eax, eax			; zero PTEs for this EMS page
	STOS_DWORD_PTR_ES_EDI
	STOS_DWORD_PTR_ES_EDI
	STOS_DWORD_PTR_ES_EDI
	STOS_DWORD_PTR_ES_EDI

	loop	FXp_free_page
	jmp	short FXp_free_done

FXp_bad_PTE:
ifdef DEBUG
	pDebugF "FreeXMSpages: Non XMS page or bad PTE %lxh @ %lxh\n", <eax, edi>
	pDebugBreak
endif
	add	edi, 4*4		; something screwy, skip this page
	loop	FXp_free_page
	jmp	short FXp_free_done

;  Page is not within the current XMS block, locate the correct block.

FXp_wrong_block:
	mov	edx, esi			; remember where we started

FXp_try_next_block:
	mov	eax, es:[edi]			; PTE of page to locate
	mov	esi, es:[esi].XMS_Next		; try next/first XMS block
	or	esi, esi
	jnz	short FXp_try_this_block
	mov	esi, [XMS_Block_List]

FXp_try_this_block:
	mov	ebx, es:[esi].XMS_Base_Page

	cmp	esi, edx			; Q: back where we started?
	jz	short FXp_bad_PTE		; Y: shouldn't happen...

	sub	eax, ebx			; Q: page within this block?
	jb	short FXp_try_next_block
	shr	eax, 14
	cmp	ax, es:[esi].XMS_Total_Pages
	jae	short FXp_try_next_block
	jmp	DebFar FXp_got_block		 ; Y:

FXp_free_done:

;ExitCrit				; END CRITICAL SECTION
popf
	call	ReleaseXMSblocks

	popad

	ret

FreeXMSpages	endp


;========================================================================


;***********************************************************************
; AllocateXMSblock:	This routine allocates a block of XMS memory
;	and adds it to the XMS_Block linked list.  Note that this
;	routine tries to allocate an XMS block that will hold the
;	desired number of pages, but may not be able to do so if there
;	is not a single large enough XMS block available.  If there
;	is not enough memory, the largest available block will be
;	allocated.  The return value (CY flag set or clear) indicates
;	if any XMS memory was allocated.
;
; Entry: (Protected Mode)
;	BX = # 16k pages needed
; Exit:
;	CY clear if an XMS block allocated
;	CY set if nothing allocated
; Used:
;	EAX, EBX, ECX, DX
;***********************************************************************
	public	AllocateXMSblock

AllocateXMSblock proc	near

	AssertSegReg	es, DATA32_GSEL
	AssertSegReg	gs, RCODEA_GSEL

	push	esi
	push	edi

;  Make sure the number of pages to allocate is at least our min allocation
;  block size and still small enough to be mapped with EMS handle space.

	cmp	bx, MIN_XMS_ALLOC_SIZE / 16 ; asking for our min block size?
	jae	short AXb_big_enough	    ; (bx in 16k pages, not k)
	movzx	ebx, [XMS_Block_Cnt]
	cmp	bx, XMS_SIZE_ENTRIES
	jb	AXb_get_size
	mov	bx, XMS_SIZE_ENTRIES - 1
AXb_get_size:
	mov	bx, [XMS_Block_Size][ebx*2]
AXb_big_enough:

	mov	ax, [BotOfVCPIspace]
	sub	ax, [TopOfFreeEMSspace] ; ax = # additional XMS pages that
	shr	ax, 2			;   can be added to EMS pool

	cmp	ax, bx
	jae	short AXb_small_enough
	mov	bx, ax
AXb_small_enough:

;  Add overhead pages to contain the XMS_Block structure and free page list.
;  We always add one 4k page (which is usually enough) to hold XMS_Block and
;  the free list, and to make sure that the free XMS 16k pages start on a
;  4k page boundry (XMS blocks are 1k aligned).  The overhead size in bytes
;  is SIZE XMS_Block + 2 * (number usable pages + 1)

	mov	cx, 4			; always add 4k for overhead/alignment

	movzx	eax, bx
	inc	ax
	lea	eax, [eax*2+SIZE XMS_Block]	; actual overhead size

	sub	ax, 1024		; alignment page gives 1k overhead--
	jb	short AXb_overhead_set	;   which is enough for most cases

	pDebugF "AllocXMSBlock: large overhead block, untested code"
	pDebugBreak

	add	ax, PAGE_SIZE - 1	; otherwise add enough 4k pages to
	shr	ax, 10			;   cover additional overhead
	add	cx, ax
AXb_overhead_set:

	shl	bx, 4			; 16k pages to k
	add	bx, cx			; add overhead / alignment size

ifdef DEBUG
	push	bx			; save requested size
endif
	mov	ax, ALLOC_XMS_BLOCK
	call	CallRealModeRtn

ifdef DEBUG
	pop	cx
	cmp	ax, cx
	jz	short AXb_got_em_all
	pDebugF "AllocXMSblock: requested %d, got %d\n", <cx, ax>
AXb_got_em_all:
endif

	; ax=block size (in k), ebx = physical address, dx=XMS handle

	or	ax, ax			; returns 0 size if failed
	jz	AXb_failed

;  We may not have gotten a block as large as we wanted, so recalculate the
;  amount of overhead needed to control the actual block.

	movzx	ecx, ax
	shr	cx, 4			  ; size in k to # 16k pages
	inc	cx			  ; overhead (bytes) = SIZE XMS_Block +
	lea	ecx, [ecx*2+SIZE XMS_Block] ;		       2 * (# pages + 1)

	mov	edi, ebx		; edi -> XMS_Block struc for this block
	lea	ebx, [ebx+ecx+PAGE_SIZE-1]
	and	bx, NOT (PAGE_SIZE - 1) ; ebx -> 1st page after overhead area

	movzx	eax, ax
	shl	eax, 10 		; size in k to bytes
	lea	ecx, [eax+edi]		; end of block
	sub	ecx, ebx		; size of free page area in bytes
	shr	ecx, 14 		; ...................... in 16k pages

;  Initialize XMS_Block fields

	mov	es:[edi].XMS_Next, 0
	mov	es:[edi].XMS_Handle, dx
	mov	es:[edi].XMS_Total_Pages, cx
	mov	es:[edi].XMS_Free_Pages, cx
	mov	es:[edi].XMS_Base_Page, ebx

;  Initialize the block's list of free pages

	xor	ax, ax
	push	edi
	add	edi, XMS_Free_Head
	.errnz	XMS_Free_List - XMS_Free_Head - 2

	cld
AXb_set_free_list:
	STOS_WORD_PTR_ES_EDI
	inc	ax
	loop	AXb_set_free_list

	mov	ax, -1				; one more to terminate list
	STOS_WORD_PTR_ES_EDI

	pop	edi

;  Add new XMS block to list of other blocks

;EnterCrit				; BEGIN CRITICAL SECTION
pushf
cli
	inc	[XMS_Block_Cnt]
	mov	esi, [XMS_Block_List]
	or	esi, esi
	jnz	short AXb_find_end

	mov	[XMS_Block_List], edi	; 1st XMS block allocated
	jmp	short AXb_done

AXb_find_end:
	mov	ebx, esi
	mov	esi, es:[esi].XMS_Next
	or	esi, esi
	jnz	short AXb_find_end

	mov	es:[ebx].XMS_Next, edi	; add to end of list

AXb_done:

;ExitCrit				; END CRITICAL SECTION
popf
	clc				; Success!
	jmp	short AXb_exit

AXb_failed:
	stc				; Failure

AXb_exit:
	pop	edi
	pop	esi
	ret

AllocateXMSblock endp


;***********************************************************************
;  ReleaseXMSblocks:	This routine scans the list of XMS_Blocks and
;	frees any that have no allocated pages.
;
;  Entry: (Protected mode)
;	None.
;  Exit:
;	None.
;  Used:
;	AX
;***********************************************************************
	public	ReleaseXMSblocks

ReleaseXMSblocks proc	near

	AssertSegReg	es, DATA32_GSEL
	AssertSegReg	gs, RCODEA_GSEL

	push	ebx
	push	edx
	push	esi

RXb_scan_list:

;EnterCrit				; BEGIN CRITICAL SECTION
pushf
cli
	mov	ebx, [p_data]
	add	ebx, offset _DATA:XMS_Block_List    ; EBX -> [XMS_Block_List]
	.errnz	XMS_Next
	mov	esi, [XMS_Block_List]

RXb_check_block:
	or	esi, esi
	jz	short RXb_none_to_free

	mov	ax, es:[esi].XMS_Free_Pages
	cmp	ax, es:[esi].XMS_Total_Pages	; Q: all pages free in block?
	jne	short RXb_check_next		; N:

	mov	edx, es:[esi].XMS_Next
	mov	es:[ebx].XMS_Next, edx
	dec	[XMS_Block_Cnt]

;ExitCrit				; END CRITICAL SECTION
popf
	mov	dx, es:[esi].XMS_Handle

	pDebugF "ReleaseXMSblocks: freeing block %lxh handle %xh\n", <esi, dx>

	mov	ax, FREE_XMS_BLOCK
	call	CallRealModeRtn

	jmp	short RXb_scan_list

RXb_check_next:
	mov	ebx, esi
	mov	esi, es:[esi].XMS_Next
	jmp	short RXb_check_block

RXb_none_to_free:

;ExitCrit				; END CRITICAL SECTION
popf
	pop	esi
	pop	edx
	pop	ebx

	ret

ReleaseXMSblocks endp

_TEXT	ends

;========================================================================

R1_CODE segment
	assume	cs:R1_CODE, ds:NOTHING, es:NOTHING, fs:NOTHING, gs:NOTHING

;***********************************************************************
; rQueryXMSpages:   This routine checks free XMS blocks to see if a
;	specific number of 16k pages are available.  This routine only
;	looks for the requested number of pages, the returned value
;	may be less than the total number free as long as the requested
;	number are found.  If the total number of requested pages are
;	not free, the returned value will be the total number of free
;	pages.	In other words, this routine looks for free pages until
;	it either finds the number wanted, or it runs out of free pages.
;
; Entry: (Virtual Mode)
;	BX = Number of 16k XMS pages desired.
; Exit:
;	AX = # XMS pages available
; Used:
;
;***********************************************************************
	public	rQueryXMSpages

rQueryXMSpages proc near

	call	SetIFflag	; invoked like an int routine, enable ints
				;   if caller had them enabled
	push	bx
	push	cx
	push	dx
	push	si
	push	di
	push	ds
	mov	ax, seg R_CODE
	mov	ds, ax
	assume	ds:R_CODE

	xor	si, si			; # free 16k pages found so far
	mov	di, MAX_NUM_XMS_BLKS	; limit how far we'll look

	push	0		; null handle to end free block loop

;  Get size of largest free XMS block and determine the number of 16k
;  pages it can contain.  Each block will contain at least one page of
;  overhead for alignment and the free page list.

rQF_next:
	push	bx		; XMS calls return error code in BL
	mov	ah, 08h 	; query free extended memory
	call	[XMMcontrol]	; sets ax=largest free, dx=total free
	pop	bx

	cmp	ax, MIN_XMS_BLK_SIZE	; don't mess with tiny XMS blocks
	jb	short rQF_done

	push	ax			; save size in k
	shr	ax, 4			; k to 16k pages
	mov	cx, 4			; always sub 4k for overhead/alignment
	movzx	eax, ax
	inc	ax
	lea	eax, [eax*2+SIZE XMS_Block]	; actual overhead size

	sub	ax, 1024		; alignment page gives 1k overhead--
	jb	short rQF_overhead_set	;   which is enough for most cases

	add	ax, PAGE_SIZE - 1	; otherwise sub enough 4k pages to
	shr	ax, 10			;   cover additional overhead
	add	cx, ax
rQF_overhead_set:

	pop	ax		; block size in k
	neg	cx
	add	cx, ax		; less overhead / alignment size

	shr	cx, 4		; k to 16k pages
	add	si, cx		; accumulate free 16k pages

	cmp	si, bx		; quicker exit if found enough free
	jae	short rQF_done

	cmp	ax, dx		; if largest == total, this is the last block
	je	short rQF_done	;   available so don't need to allocate it

;  Allocate largest block to find out what the next largest block is

	mov	dx, ax		; allocate the largest free block
	push	bx		; XMS calls return error code in BL
	mov	ah, 09h 	; allocate extended memory block
	call	[XMMcontrol]
	pop	bx

	or	ax, ax		; ax = 0 if allocation failed, shouldn't
	jz	short rQF_done	;   happen, but...

	push	dx		; save handle to this XMS block

	dec	di		; look for another block if there aren't too
	jnz	short rQF_next	;   many of them

	;fall through to rQF_done

;  Free the blocks allocated above

rQF_done:

	pop	dx		; XMS handle or terminating 0
	or	dx, dx
	jz	short rQF_exit

	mov	ah, 0Ah 	; free extended memory block
	call	[XMMcontrol]
	jmp	short rQF_done

rQF_exit:
	mov	ax, si		; accumulated free pages

	pop	ds
	pop	di
	pop	si
	pop	dx
	pop	cx
	pop	bx

	iret			; NOTE: IRET, not ret

rQueryXMSpages endp

;***********************************************************************
; rAllocateXMSblock:	This routine allocates a block of XMS memory
;	and returns info about the block.
;
; Entry:
;	BX = max block size wanted, in k
; Exit:
;	AX = block size allocated, in k
;	EBX= physical address of block
;	DX = XMS handle to block
; Used:
;
;***********************************************************************
	public rAllocateXMSblock

rAllocateXMSblock   proc    near

	call	SetIFflag	; invoked like an int routine, enable ints
				;   if caller had them enabled
	push	ds
	mov	ax, seg R_CODE
	mov	ds, ax
	assume	ds:R_CODE

	push	bx
	mov	ah, 08h 		; query free XMS memory
	call	[XMMcontrol]
	pop	bx

	cmp	ax, bx			; Q: can we get all that's wanted?
	jae	short rAXb_get_it_all	; Y:
	cmp	ax, MIN_XMS_BLK_SIZE	; N: Q: is there at least the min?
	jae	short rAXb_get_biggest	;    Y: get that amount
	jmp	DebFar rAXb_fail

rAXb_get_it_all:
	mov	ax, bx

rAXb_get_biggest:
	push	ax			; save size to alloc

	mov	dx, ax
	mov	ah, 09h 		; allocate extended memory block
	call	[XMMcontrol]
	or	ax, ax
	jz	short rAXb_fail_alloc

	push	dx			; save block handle

	mov	ah, 0Ch 		; lock extended memory block
	call	[XMMcontrol]
	or	ax, ax
	jz	short rAXb_fail_lock	; shouldn't happen, but...

	xchg	bx, dx
	shl	ebx, 16
	mov	bx, dx			; ebx = physical address of block

	pop	dx			; dx = XMS handle
	pop	ax			; ax = block size

	rDebugF "rAllocXMSblock: %dk @ %lxh handle %xh\n", <ax, ebx, dx>

	pop	ds

	iret			; NOTE: IRET, not ret

rAXb_fail_lock:
	pop	dx			; XMS handle
	mov	ah, 0Ah 		; free extended memory block
	call	[XMMcontrol]

	rDebugF "rAllocXMSblock: lock failed!\n"

rAXb_fail_alloc:
	pop	ax			; size to alloc

	rDebugF "rAllocXMSblock: alloc of %dk failed!\n", ax

rAXb_fail:
	xor	ax, ax			; 0 allocated size means failure

	pop	ds

	iret			; NOTE: IRET, not ret

rAllocateXMSblock   endp

;***********************************************************************
; rFreeXMSblock:	This routine returns an XMS block to the XMS
;	memory manager.
;
; Entry: (Virtual mode)
;	DX = handle of XMS block to free
; Exit:
;
; Uses:
;	AX, BX
;***********************************************************************
	public rFreeXMSblock

rFreeXMSblock	proc	near

	call	SetIFflag	; invoked like an int routine, enable ints
				;   if caller had them enabled
	push	ds
	mov	ax, seg R_CODE
	mov	ds, ax
	assume	ds:R_CODE

	;;;rDebugF "rFreeXMSblock: freeing handle %xh\n", dx

	mov	ah, 0Dh 	; unlock extended memory block
	call	[XMMcontrol]
	mov	ah, 0Ah 	; free extended memory block
	call	[XMMcontrol]

ifdef DEBUG
	cmp	ax, 1
	jz	short @f
	rDebugF "rFreeXMSblock: free handle %xh failed!\n", dx
	rDebugBreak
@@:
endif
	pop	ds

	iret

rFreeXMSblock	endp


;***********************************************************************
; SetIFflag:	Helper routine to set IF flag based on flags in IRET
;	frame on stack.
;
; Entry: (Virtual mode)
;	SP-> [IP] [IP] [CS] [FL]
; Exit:
;	Interrupts may be enabled.
; Uses:
;	AX, BX
;***********************************************************************
	public SetIFflag

SetIFflag proc	near

	push	bp			;	 0    2    4	6    8
	mov	bp, sp			; bp -> [bp] [ip] [ip] [cs] [fl]

	test	byte ptr [bp+9], 2	; Q: int's enabled in stack image of
	jz	short IF_set		;    flags?

	sti				; Y:
IF_set:
	pop	bp
	ret

SetIFflag endp


R1_CODE ends

END
