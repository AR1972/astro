	page    ,132
	title   ROM COMMAND Resident ROM Header

comment %------------------------------------------------------------------

This module provides a standard ROM header for ROM COMMAND as
described in the IBM System BIOS technical reference.

MS-DOS ROM $Exec header is also supplied, as described in the
MS-DOS 5.0 ROM Version technical specification.  ROM program
names for the command interpreter are supplied.

This code must appear on a 2K boundary within I/O adapter address
space.

%--------------------------------------------------------------------------



ROMHEADER	segment


;***	Adapter ROM module signature and size
;
;	The signature must be aligned on a 2K address boundary.
;	A checksum of bytes in the specified range must be zero
;	for the ROM module to be valid.

	db      55h,0AAh                ; ROM signature
	db      70h			; ROM size (56K) in 512-byte pages

	; Note that size field is usually set automatically when ROM images
	; are put together.  The zero checksum is handled at that point, too.

	assume  cs:ROMHEADER

;*	POST Initialization (POST makes far call to here).

	jmp     near ptr POSTInit       ; POST initialization

;*	ROM file header for DOS $ROMFindFirst, $ROMFindNext, $Exec.

ROMFile	macro	name,entry
	local	nambeg,namend
	db	namend-nambeg	;; file name length
nambeg	db	"&name"		;; file name
namend	label	byte
	jmp	near ptr entry	;; exec entry point
	endm

	ROMFile	COMMAND,Launch		; COMMAND launch
	ROMFile	COMMAND.COM, Launch	; one more name fore command

	db      0               	; end of file names




;***    POST initialization - no action needed

POSTInit:
	retf




;***    COMMAND launch

	include	command.loc		; COMMAND image location

Launch:

;       CS = ROMHEADER segment
;       DS,ES,SS = segment of PSP

;*      Copy COMMAND image to RAM.
;
;	We supply to routines, one to copy the COMMAND image from
;	ROM below 1 MB, the other to copy the image from extended
;	address space.  We choose between the two routines by
;	determining whether command.loc, the COMMAND location file,
;	defines COMMAND_SEG (meaning COMMAND is below 1 MB) or
;	COMMAND_LO (meaning COMMAND is in extended address space).
;
;	The OEM may replace this whole IFDEF structure with their
;	own loader routine.  For example, if the COMMAND image is
;	in port-accessed ROM, I would use ROMnTYPE=BASE in the ROM
;	image description file.  This would generate COMMAND_LO and
;	COMMAND_HI address symbols in command.loc.  I'd replace
;	the following IFDEF structure with code to use COMMAND_LO
;	(and COMMAND_HI, if necessary) along with COMMAND_SIZ to
;	copy the COMMAND image from port-accessed ROM.

;	------------------------------------------------------------
	ifdef	COMMAND_SEG
;	COMMAND image is in first megabyte.  Cool.
	
	mov	ax,COMMAND_SEG		; AX = COMMAND image seg addr
	mov	ds,ax
	xor	si,si			; DS:SI = ptr to COMMAND image

	mov     di,100h                 ; ES:DI = ptr to byte after PSP
	mov     cx,COMMAND_SIZ		; CX = # bytes to copy
	cld
	rep     movsb                   ; copy COMMAND image to RAM

	else
;	------------------------------------------------------------
	ifdef	COMMAND_LO

;	COMMAND image is in extended address space.
;	Copy via int 15h block move.

	jmp	short setupGDT		; jump over data

;	Global Descriptor Table (GDT)

GDT	db	10h dup (0)		; reserved zero
	dw	COMMAND_SIZ		; segment length
	dw	COMMAND_LO		; low 16 bits of source address
	db	COMMAND_HI		; high 8 bits of source address
	db	93h			; access rights byte
	db	2 dup (0)		; reserved zero
	dw	COMMAND_SIZ		; segment length
	db	3 dup (?)		; 24-bit dest addr (to be filled in)
	db	93h			; access rights byte
	db	12h dup (0)		; reserved zero

;	Set up Global Descriptor Table at PSP+100h+COMMAND_SIZ.

setupGDT:

GDT_OFF	equ	100h+COMMAND_SIZ	; offset of GDT location in RAM

	cld

	mov	ax,cs
	mov	ds,ax			; DS = ROMHEADER seg addr
	mov	si,offset GDT		; DS:SI = ptr to GDT template
	mov	cx,30h			; CX = size of template
	mov	di,GDT_OFF		; ES:DI = ptr to GDT RAM location
	rep	movsb			; copy template to RAM

	mov	ax,es			; AX = PSP seg addr
	mov	ds,ax			; DS = PSP seg addr again

;	Compute 24-bit linear destination address.

	mov	bx,10h
	add	ax,bx			; AX = seg addr of PSP+100h
	mul	bx			; DX:AX = linear addr of PSP+100h
	mov	ds:GDT_OFF+1Ah,ax	; put 24-bit addr in the GDT
	mov	byte ptr ds:GDT_OFF+1Ch,dl

	mov	si,GDT_OFF		; ES:DI = ptr to GDT
	mov	cx,(COMMAND_SIZ+1)/2	; CX = # words to xfr
	mov	ah,87h			; AH = 'Move Extended Memory Block'
	int	15h			; call I/O subsystem extensions
	jnc	moved			; it worked

;	Block move failed.

;	Need to issue "Cannot load ROM COMMAND" message.

	mov	ax,4C01h		; AX = 'Terminate', exit code 1
	int	21h			; call DOS

moved:

;	------------------------------------------------------------
	endif
	endif

;*      Transfer control to COMMAND in RAM.

	mov     ax,es                   ; AX = PSP segment
	mov     ds,ax                   ; DS = PSP segment

	push    ax
	mov     ax,100h                 ; AX = offset of COMMAND start addr
	push    ax                      ; stack = CS:IP for COMMAND start
	retf                            ; transfer to COMMAND in RAM



EndOfHeader	label	byte

ROMHEADER	ends

	end

