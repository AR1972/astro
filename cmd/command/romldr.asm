	page	,132
	title	ROM COMMAND Image Loader

comment	%

LoadFromROM loads all or any piece of the ROM COMMAND image into RAM.

This source module, along with romhead.asm and romres.asm, are OEM
replaceable, depending on what kind of ROM storage and layouts
are used.  The supplied routines handle ROM images in the first
Mbyte of address space as well as transient images loaded from
extended address space ROM.  The correct routines are selected
at build time by the symbols defined in command.loc and rescom.loc.  
name_SEG implies that 'name' will reside within the first Mbyte.
name_LO or name_HI implies that 'name' will reside in extended
address space.  Location files (*.loc) are generated during the
build and depend on sizes of code granules as well as the ROM
image description file (in romimg directory).

If other storage options are used, the OEM will replace loader
routines with code that fetches or locates code in the storage
actually used.  If the storage option uses a linear address space
(such as port-accessed ROM), the name_LO/name_HI location symbols may
still be used (ROMnTYPE=BASE in the ROM image description file). 
Otherwise, the loader may ignore the location files, and be
hard-coded to retrieve the desired code.

LoadFromROM is normally part of the resident code segment.  If banked
ROM's are used such that the resident code will be switched out when
copying from the ROM COMMAND image, this routine should be moved to
the resident data segment.  To do this, change CODERES to DATARES
and, in rom.lnk, move romldr.obj to between stub.obj and rdata.obj. 
Then uncomment the dword pointer at the end of this module and the
code in init.asm that initializes LoadFromROM_seg. Finally, rename
the procedure itself LoadFromROM_Proc and make it FAR.  Make sure
that any references to LoadFromROM reference it as a dword extrn and
have a segment register pointing at DATARES.  Note that all calls to
LoadFromROM are from resident code.

%



;***	CONSTANTS

	include	command.loc		; location of ROM COMMAND image
	include	comseg.asm		; all COMMAND segment definitions




;***	DATA

ifdef COMMAND_LO

;	ROM COMMAND image is in extended address space
;	We'll need access to our GDT in DATARES.

DATARES segment public byte		; resident data

	extrn	Int15GDT:byte,gdtSrcLen:word,gdtSrcLo:word,gdtSrcHi:byte
	extrn	gdtDstLen:word,gdtDstLo:word,gdtDstHi:byte

DATARES ends

endif ; COMMAND_LO




;***	CODE

CODERES	segment public byte

	public	LoadFromROM

	assume	cs:CODERES,ds:DATARES,es:NOTHING,ss:NOTHING




;***	LoadFromROM - load all or any piece of ROM COMMAND image into RAM
;
;	ENTRY	SI = offset into image
;		CX = number bytes to load
;		ES:DI = ptr to destination buffer
;		DS = DATARES seg addr
;
;	EXIT	nothing
;
;	USED	AX,CX,SI,DI
;
;	EFFECTS -
;
;	CX bytes are copied from SI offset in image to buffer at ES:DI.
;	Caller's stack is used.
;
;	NOTES -
;
;	We assume enough room in the destination segment above ES:DI.
;	ES:DI is not normalized.

LoadFromROM	proc

ifdef COMMAND_SEG

;	ROM COMMAND image is below 1M.  Easy street.

	push	ds			; preserve DS

	mov	ax,COMMAND_SEG
	mov	ds,ax			; DS:SI = ptr into image

	cld
	rep	movsb

	pop	ds
	ret

else
ifdef COMMAND_LO

;	ROM COMMAND image is in extended address space.
;	Copy it via int 15h block move.

	push	bx			; preserve registers
	push	dx

	mov	gdtSrcLen,cx		; set segment lengths
	mov	gdtDstLen,cx

;	Compute 24-bit linear source address from SI and image address.

	mov	ax,COMMAND_LO		; AX = lo 16 bits of image address
	add	ax,si			; AX = lo 16 bits of source address
					; Preserve CY!
	mov	gdtSrcLo,ax
	mov	al,COMMAND_HI
	adc	al,0			; AL = hi 8 bits of source address
	mov	gdtSrcHi,al

;	Compute 24-bit linear destination address from ES:DI.

	mov	ax,es
	mov	bx,10h
	mul	bx			; DX:AX contains linear equiv of ES
	add	ax,di
	adc	dx,0			; DX:AX contains linear equiv of ES:DI
	mov	gdtDstLo,ax		; store 24-bit dest addr
	mov	gdtDstHi,dl

	mov	si,offset DATARES:Int15GDT
	push	es			; preserve ES
	push	ds
	pop	es			; ES:DI = ptr to GDT

	add	cx,1
	shr	cx,1			; CX = # words to xfr
	mov	ah,87h			; AH = 'Move Extended Memory Block'
	int	15h			; call I/O subsystem extensions
	pop	es			; restore ES
	jnc	moved			; it worked

;	Block move failed.

;	Need to issue "Cannot access ROM COMMAND" message.

moved:
	pop	dx			; restore some registers
	pop	bx
	ret

endif
endif

LoadFromROM	endp




;;;***	Far Call Vectors (in resident data segment)
;;
;;LoadFromROM	label	dword
;;		dw	LoadFromROM_Proc
;;LoadFromROM_seg	dw	?


CODERES ends

	end
