.386p
;******************************************************************************
	title	SEGFIX
;******************************************************************************
;
;   (C) Copyright MICROSOFT Corp. 1989-1991
;   (C) Copyright COMPAQ Computer Corp. 1989-1991
;
;   Title:    EMM386.EXE - MICROSOFT Expanded Memory Manager 386 Driver
;
;   Module:   SEGFIX - Sets the descriptor caches
;
;   Version:  0.001
;
;   Date:     Feb 16,1990
;
;   Author:   Harish K. Naidu
;
;******************************************************************************
;
;   Change log:
;
;     DATE    REVISION			DESCRIPTION
;   --------  --------	-------------------------------------------------------
;   02/16/89  Original
;
;******************************************************************************
;
;   Functional Description:
;
;
;******************************************************************************

;******************************************************************************
;	P U B L I C S
;******************************************************************************

	public	segfixup

;******************************************************************************
;	D E F I N E S
;******************************************************************************
	include vdmseg.inc
	include	desc.inc

SGDTD_GSEL	equ	08h 		; gdt data alias
SIDTD_GSEL	equ	10h 		; idt data alias
SVDMC_GSEL	equ	18h 		; VDM Code selector
SVDMD_GSEL	equ	20h 		; VDM Data Selector

;******************************************************************************
;	E X T E R N A L   R E F E R E N C E S
;******************************************************************************

LAST	segment

	extrn	SetSegDesc:near
	extrn	SegTo24:near

LAST	ends


SGDT	segment

GDT_ENTRY	0, 0, 1, 0			; null selector
GDT_ENTRY	0, 0, 0, D_DATA0		; GDT alias
GDT_ENTRY	0, 0, 0, D_DATA0		; IDT alias
GDT_ENTRY	0, 0, 0, D_CODE0		; VDM Code
GDT_ENTRY	0, 0, 0, D_DATA0		; VDM Data

SGDTLEN	EQU	$-SGDT

SGDT	ends

SIDT	segment

IDT_ENTRY	0, 0, 0 	 

SIDTLEN	EQU	$-SIDT

SIDT	ends


LAST	segment 

	assume	cs:LAST

SGDT_Ptr 	dd 2 dup (0)	; GDT ptr for LGDT
SIDT_Ptr 	dd 2 dup (0)	; IDT ptr for LIDT

Sreal_idt	dw	0400h	; real mode DOS IDT limit
		dd	0	; and base ptr
		dw	0	; just in case qword used

ss_save		dw	?	; temporary location to save stack
sp_save		dw	?


;--------------------------------------------------------------------------
;
; Procedure SegFixup
;
; This routine will switch to protect mode and set up the selectors with 
; real mode attributes and return to real mode. This is called at init
; time. This is necessary to fix up the BUGGY ROMS of certain machines
; like APRICOT Qi which does not do this for fs and gs. Therefore on this
; machine one cannot access fs from REAL MODE!! unless something similar
; to this routine is done
;
; USES: ALL
;--------------------------------------------------------------------------

segfixup	proc	near

	mov	ax,SGDT
	mov	es,ax			; ES:0 -> gdt

	mov	ax,SGDT
	call	SegTo24
	mov	cx,SGDTLEN
	mov	ah,D_DATA0
	mov	bx,SGDTD_GSEL
	call	SetSegDesc		; Set up SGDT alias descriptor

	mov	ax,SIDT
	call	SegTo24
	mov	cx,SIDTLEN
	mov	ah,D_DATA0
	mov	bx,SIDTD_GSEL
	call	SetSegDesc		; Set up SIDT alias descriptor

	mov	ax,seg LAST
	call	SegTo24
	mov	cx,0			; 0 = 64K size
	mov	ah,D_CODE0
	mov	bx,SVDMC_GSEL
	call	SetSegDesc		; Set up LAST Code descriptor


	mov	ax,seg LAST
	call	SegTo24
	mov	cx,0			; 0 = 64K size
	mov	ah,D_DATA0
	mov	bx,SVDMD_GSEL
	call	SetSegDesc		; Set up LAST Data descriptor




; The GDT and IDT pointers are setup up so that when CEMM gets turned on,
; the LGDT and LIDT instructions will have their correct pointers.
;
	mov	ax,SGDT		; DS:SI point to the GDT's entry location.
	mov	ds,ax
	mov	si,SGDTD_GSEL


	mov	ax,seg LAST	; ES:DI point to data strucutre used by the LGDT.
	mov	es,ax
	assume	es:LAST
	mov	di,offset LAST:SGDT_Ptr

	movsd	; The 8 byte descriptor is copied over as is.
	movsd

;
; Since only the first 6 bytes of the GDT pointer is needed for the base and
; linear address, the upper 8 bits of the linear address must be copied down
; to its proper location.
;
	mov	al,byte ptr es:[SGDT_Ptr][7]
	mov	byte ptr es:[SGDT_Ptr][5],al

;
; The exact same operations are done for the IDT pointer.
;
	mov	si,SIDTD_GSEL
	mov	di,offset LAST:SIDT_Ptr
	movsd
	movsd
	mov	al,byte ptr es:[SIDT_Ptr][7]
	mov	byte ptr es:[SIDT_Ptr][5],al

	pushf
	mov	cs:[ss_save],ss
	mov	cs:[sp_save],sp

	cli

;
;   load gdt and ldt base registers  (DB 66h needed for 32 bit address)
;
	db	66h
	lgdt	fword ptr CS:[SGDT_ptr]
	db	66h
	lidt	fword ptr CS:[SIDT_ptr]
;
;   go protected and enable paging - turn on bits in CR0
;

	mov	eax,cr0

	or	eax, MSW_PROTECT ; or EAX,imm32
		; PROT MODE

	mov	cr0,eax

;	far jump to flush prefetch, and reload CS

	db	0eah			; far jmp opcode
	dw	offset LAST:spm1	; offset
	dw	SVDMC_GSEL		; selector
spm1:


;  We are now in protected mode.

;
;
;  Intel shows DS,ES,FS,GS,and SS set up to make sure 'Real Mode' type
;  access rights, and limit are installed.  In this program, that happens
;  to already be the case, but for general purposeness, VDMD_GSEL fits
;  the bill.
;
	mov	ax,SVDMD_GSEL		; selector with real mode attributes
	mov	ds,ax
	mov	es,ax
	mov	ss,ax
	mov	fs,ax
	mov	gs,ax

;
; Switch back to real
;

;
;    reset the PE bit ...
;
	mov	eax,cr0			;  get CR0
	and	eax,07FFFFFFEh		; force real mode and shut down paging
	mov	cr0,eax			; set CR0

					; flush prefetched instructions with:
	db	0EAh			; Far Jump opcode
	dw	offset LAST:srl386_b	; destination offset
	dw	LAST			; destination segment
srl386_b:

	lidt	fword ptr cs:[Sreal_idt]

	mov	ss, cs:[ss_save]	; restore stack
	mov	sp, cs:[sp_save]
	
	mov	eax,cr3			; get CR3
	mov	cr3,eax			; set CR3 => clear TLB

	popf
	ret

segfixup	endp


LAST	ends
	end
