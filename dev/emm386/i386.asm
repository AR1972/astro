.386p
page	58,132
;******************************************************************************
	TITLE	i386.asm - Support Routines for protected mode system
;******************************************************************************
;
;   (C) Copyright MICROSOFT Corp. 1986-1991
;   (C) Copyright COMPAQ Computer Corp. 1986-1991
;
;   Title:    EMM386.EXE - MICROSOFT Expanded Memory Manager 386 Driver
;
;   Module:   i386.asm - Support Routines for protected mode system
;
;   Version:  0.04
;
;   Date:     January 31, 1986
;
;   Author:
;
;******************************************************************************
;
;   Change log:
;
;     DATE    REVISION			DESCRIPTION
;   --------  --------	-------------------------------------------------------
;   01/31/86  Original
;   05/12/86  A-RRH	Cleanup and segment reorganization
;   06/21/86  0.02	Added clds just in case (SBP).
;   06/28/86  0.02	Name changed from CEMM386 to CEMM (SBP).
;   07/06/86  0.04	changed assume to _DATA (SBP).
;
;******************************************************************************
;
;   Functional Description:
;
;	Steve Preston
;	January 18,1985
;
;	DESCRIPTION
;
;	These routines manage the various 386 memory management
;	tables and manipulate descriptors and selectors.
;
;	All the routines which manipulate descriptors are callable
;	in both real and protected mode.
;
;	In general all registers are preserved.
;
;	The following routines are provided:
;
;		SetPageEntry - set up an entry in a Page Directory on Page
;					Table.
;
;		GetPageEntry - retrieve a page dir/page table entry.
;
;		InitPages - initialize page directory and page table.
;
;		PageDirOff - convert 32 bit addr to page dir entry offset
;
;		PageTableOff - convert 32 bit addr to page table entry offset
;
;	WARNING This code is 386 specific, it will NOT run on an 8088.
;
;******************************************************************************
.lfcond 				; list false conditionals
;
	include VDMseg.inc
	include VDMsel.inc
	include desc.inc
	include page.inc

;******************************************************************************
; 		E X T E R N A L S
;******************************************************************************
LAST	SEGMENT
extrn	SegTo24:near
extrn	SetSegDesc:near
LAST	ENDS


;******************************************************************************
;		S E G M E N T   D E F I N I T I O N S
;******************************************************************************
LAST SEGMENT

    assume cs:LAST,ds:_DATA,es:_DATA

;**	SetGateDesc - set up Gate Descriptor entry
;
;
;	ENTRY	DX,AX = 32 bit offset of target routine
;		CX = target code segment selector
;		ES:[DI] = points to table entry
;		BL = access bits
;	EXIT	descriptor set.
;	USES	Flags, other regs preserved
;
;	WARNING This code only works on a 386. It can be called in
;		either mode.

	public SetGateDesc
SetGateDesc proc near
;
	push	ax
	push	di
;
	cld			; stings foward
;
	stosw				; store low word of offset
	mov	ax,cx
	stosw				; store selector
	mov	al,0
	mov	ah,bl
	stosw				; store access rights
	mov	ax,dx
	stosw				; store high word of offset
;
	pop	di
	pop	ax
	ret
SetGateDesc endp


;**	SetPageEntry - set up entry in Page Directory or Page Table
;
;
;	ENTRY	DX,AX = 32 bit address of page or page table
;		ES:[DI] = page directory or table entry to set
;		BX = access/status bits ( bits 0 - 11 )
;	EXIT	ES:[DI] = next page directory or table entry
;	USES	Flags, other regs preserved
;
;	WARNING This code only works on a 386. It can be called in
;		either mode.

	public SetPageEntry
SetPageEntry proc near
;
	push	ax
;
	cld			; strings foward
;
	and	bx,0FFFh	; turn off any bits in address range
	or	ax,bx		; mov status bits into AX
	stosw			; store status and addr bits 12 - 15
	mov	ax,dx		; AX = addr bits 16-31
	stosw			; store addr bits 16-31
;
	pop	ax
	ret
SetPageEntry endp


;**	GetPageEntry -	up entry in Page Directory or Page Table
;
;	ENTRY	ES:[DI] = page directory or table entry
;	EXIT	DX,AX = 32 bit address of page or page table
;		BX = access/status bits (bits 0 - 11).
;	USES	Flags, other regs preserved
;
;	WARNING This code only works on a 386. It can be called in
;		either mode.

	public GetPageEntry
GetPageEntry proc near
;
	mov	ax,ES:[DI]	; AX = low word of entry
	mov	bx,ax		; get access/status rights bits
	and	bx,00FFFh	; turn off address bits
;
	and	ax,0F000h	; turn off status bits
	mov	dx,ES:[DI+2]	; get high word of addr
;
	ret
GetPageEntry endp


;**	PageDirOff - convert 32 bit linear address to page directory offset
;
;
;	ENTRY	EAX = 32 bit linear address
;	EXIT	DI = offset in page dir to appropriate entry
;	USES	Flags, other regs preserved
;
;	WARNING This code only works on a 386. It can be called in
;		either mode.

	public PageDirOff
PageDirOff proc near
;
	push	ebx		; push EBX

	mov	ebx,eax		; EBX = EAX

	shr	ebx,22		; EBX[0:9] = EBX[22:31] = high 10 bits
	shl	bx,2		; *4 for dword indexing into table

	mov	di,bx		; DI = dir offset

	pop	ebx		; pop EBX
;
	ret
PageDirOff endp


;**	PageTableOff - convert 32 bit linear address to page table offset
;
;
;	ENTRY	EAX = 32 bit linear address
;	EXIT	DI = offset in page table to appropriate entry
;	USES	Flags, other regs preserved
;
;	WARNING This code only works on a 386. It can be called in
;		either mode.

	public PageTableOff
PageTableOff proc near
;
	push	ebx		; push EBX

	mov	ebx,eax		; mov EBX,EAX

	shr	ebx,12		; EBX[0:9] = EBX[12:21] = middle 10 bits
	and	bx,3FFh 	; only EBX[0:9]
	shl	bx,2		; *4 for dword indexing into table

	mov	di,bx		; DI has table offset

	pop	ebx		; pop	EBX
;
	ret
PageTableOff endp


LAST	ends
	end
