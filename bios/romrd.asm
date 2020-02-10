	page	,132
	title	RomDrive physical ROM access routine

;	This is an extended-memory capable version which uses the
;	  ROM BIOS's Int15(ah=87h) block move function.
;	Note:  This module assumes low 16Meg mapping (286 space only)

;
;----------------------------------------------------------------------------
;
; M071 : Made ROMDRIVEBASE_LO equated to 0000 instead of 00 for more
;	 readability
;
;----------------------------------------------------------------------------
;
;ROMCODE	SEGMENT	byte public
;	assume	cs:ROMCODE
	.xlist
	include	biosseg.inc
	include	msgroup.inc
	.list
	assume	cs:bios_data

IFNDEF	FAKEROM

ROMDRIVEBASE_LO	equ	0000h			; M071
ROMDRIVEBASE_HI	equ	0b0h

ELSE

ROMDRIVEBASE_LO	equ	0
ROMDRIVEBASE_HI	equ	0

ENDIF ; FAKEROM

	public	xfer_from_rom

;	Entry:
;	       dx:bx  - 32 bit ROM offset of transfer block
;	       es:di  - real mode address where it should go
;	       cx     - count in bytes, guaranteed to have at least 2 zero lsbs
;		         note:  transfer length == 00 means 64K
;

;	EXIT:
;	    If error detected
;		Carry Set
;		   al == error code 8 (sector not found)

gdts	dw	0,0,0,0		; gdt 0 == dummy
	dw	0,0,0,0		; gdt 1 == GDT seg (Int15 fills in)

	dw	0ffffh		; gdt 2 limit = 65535
src_low	dw	0		; gdt 2 low 16 bits
src_hi	db	0
	db	93h,0,0		; gdt 2 access rights & reserved

	dw	0ffffh		; gdt 3 limit = 65535
dst_low	dw	0		; gdt 3 low 16 bits
dst_hi	db	0
	db	93h,0,0		; gdt 3 access rights & reserved

	dw	0,0,0,0		; gdt 4 == ROM BIOS CODE (Int15 fills in)
	dw	0,0,0,0		; gdt 5 == stack (Int15 fills in)

rombase_low	dw	ROMDRIVEBASE_LO	; point to full ROM address
rombase_high	dw	ROMDRIVEBASE_HI	; as linear address

xfer_from_rom:
	add	bx,rombase_low	; add in ROM base linear address
	adc	dx,rombase_high

IFDEF	FAKEROM

;	****  For debug purposes, we'll also add in the offset to
;		the phony ROM image we load as part of us.  When
;		physical ROM hardware becomes available, the following
;		can be deleted.

	add	bx,offset rom_image
	adc	dx,0
	mov	ax,cs
	rol	ax,1
	rol	ax,1
	rol	ax,1
	rol	ax,1
	push	ax
	and	ax,0fff0h
	add	ax,bx
	mov	src_low,ax		; save low 16 bits of source
	pop	ax
	adc	al,dl
	and	al,0fh		; limit to low meg for simulation
	mov	src_hi,al

;	*this is the end of the debug simulation code*

ELSE
	mov	src_low, bx
	mov	src_hi, dl

ENDIF ; FAKEROM

;	Now calculate and store the destination address

	mov	ax,es
	rol	ax,1
	rol	ax,1
	rol	ax,1
	rol	ax,1
	push	ax
	and	ax,0fff0h
	add	ax,di
	mov	dst_low,ax
	pop	ax
	adc	al,0
	and	al,0fh
	mov	dst_hi,al

	push	es
	push	si
	mov	si,offset gdts
	push	cs
	pop	es
	shr	cx,1			; convert count to words
	jnz	not_64k
	or	ch,80h			; force 64k to be 32K words, not zero
not_64k:
	mov	ah,87h
	int	15h
	pop	si
	pop	es
	mov	al,0
	jnc	no_disk_error
	mov	al,8			; all errors are type 8
no_disk_error:
	ret

IFDEF	FAKEROM
rom_image:
	include	rom8k.inc
ENDIF ; FAKEROM

bios_data	ENDS
	end

