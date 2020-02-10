	page	,132
	title	RomDrive physical ROM access routine

	.xlist
	include	biosseg.inc
	include	msgroup.inc
	.list
	assume	cs:bios_data

ifndef		FAKEROMDRIVE
ROMDRIVESEG	equ	0A000h
endif	; FAKEROMDRIVE

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

xfer_error:
	mov	al,8			; return 'sector not found'
	stc
	ret

ifndef	FAKEROMDRIVE
xfer_from_rom:
	push	ds
	push	si
	mov	ax, ROMDRIVESEG
	mov	ds, ax
	mov	si, bx
	shr	cx,1			; convert count to words
	jnz	not_64k
	or	ch,80h			; force 64k to be 32K words, not zero
not_64k:
	cld
	rep	movsw
	pop	si
	pop	ds
	clc
	ret
else
xfer_from_rom:
;	assume dx is zero for now.

	push	ds
	push	si
	mov	si,offset rom_image
	add	si,bx
	shr	cx,1			; convert count to words
	jnz	not_64k
	or	ch,80h			; force 64k to be 32K words, not zero
not_64k:
	cld
	push	cs
	pop	ds
	rep	movsw
	pop	si
	pop	ds
	clc
	ret

rom_image:
	include	rom8k.inc
endif	; FAKEROMDRIVE

bios_data	ENDS
	end
