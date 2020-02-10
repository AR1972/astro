	page	,132
	title	RomDrive initialization module (disposable)

	.xlist
	include version.inc	; <jrcb>
	include	biosseg.inc
	include	msgroup.inc
	.list
	assume	cs:datagrp


; INIT PACKET OFFSETS
INIT_NUM	EQU	BYTE PTR (13) + 0
INIT_BREAK	EQU	DWORD PTR (13) + 1
INIT_BPB	EQU	DWORD PTR (13) + 5
INIT_DOSDEV	EQU	BYTE PTR (13) + 9

	public	ROM$INIT

	extrn	DEVEXIT:near
	extrn	RDRIVEBPB:byte
	extrn	bpb_rd_length:abs
	extrn	ptrsavx:dword
	extrn	rombase:dword

	extrn	xfer_from_rom:near

;	Note:  This module is not disposed of when this is BIOS
;	  resident, because it is too late to dispose of code by
;	  the time DOS calls back to the INIT entry.  It doesn't
;	  matter much, though, because when ROMDRVALONE is false,
;	  this module isn't very big.

;	externals in message module

	ifdef	romdrvalone
	extrn	headermes:byte
	extrn	badvermes:byte
	extrn	dos_drv:byte
	endif

INITAB	DW	RDRIVEBPB

;**	ROM$INIT - Device Driver Initialization routine
;
;	ROMDRIVE Initialization routine. This is the COMMON initialization
;	code used by ALL driver TYPEs. Its jobs are to:
;
;	    1.  Access the BPB on the ROM drive and set parameters
;	           appropriately.
;	    2.  Print out report of ROMDrive install (on standalone vers)
;	    3.	Set the return INIT I/O packet values
;
;	If at any time during the above steps an error may be detected. When
;	this happens one of the error messages is printed and ROMDrive
;	"de-installs" itself by returning a unit count of 0 in the INIT
;	device I/O packet. The DOS device installation code is responsible
;	for taking care of the details of re-claiming the memory used by the
;	device driver.
;
;	Step 3 sets the INIT I/O packet return values for # of units,
;	Break address, and BPB array pointer and returns via DEVEXIT.
;
;	ENTRY from ROM$IN
;	EXIT Through DEVEXIT
;	USES ALL
;

ROM$INIT:
	cld

;	Standalone version is tightly DOS 5.0 version bound.

	ifdef	ROMDRVALONE
	mov	ah,30h			; get version
	int	21h
	xchg	ah,al
	cmp	ax,500h			; version 5.00 or higher?
	jae	ver_ok			; 5.x or higher are okay

	mov	dx,offset badvermes
	push	cs
	pop	ds
	mov	ah,9			; use DOS function 9 to display string
	int	21h

error_initializing:
	xor	al,al			; indicate no devices
	jmp	short init_done		; and return

ver_ok:

;	display message including drive letter

	LDS	SI,[ptrsavx]
	MOV	AL,[SI.INIT_DOSDEV] ; DOS drive letter
	ADD	CS:[DOS_DRV],AL     ; Need explicit over, this is a forward ref
	mov	dx,offset headermes
	push	cs
	pop	ds
	mov	ah,9
	int	21h		; display message
	endif

;	Now let's try to get the DPB from that device

	push	cs
	pop	es
	mov	di,offset rdrivebpb
	mov	bx,0bh			; this is where the dpb lives
	xor	dx,dx			; high 16 bits of address
	mov	cx,bpb_rd_length
	call	xfer_from_rom

	ifdef	romdrvalone
	jc	error_initializing
	endif

	mov	al,1			; one drive, please!

;**	init_done - Set INIT packet I/O return values
;
;	This entry is used in ERROR situations to return
;	a unit count of 0 by jumping here with AL = 0.
;	The successful code path falls through to here
;	with AL = 1
;
;	ENTRY
;	    AL = INIT packet unit count
;	EXIT
;	    through DEVEXIT
;	USES
;	    DS, BX, CX
;

;	set the return INIT I/o packet values

init_done:
	LDS	BX,[ptrsavx]
	MOV	[BX.INIT_NUM],AL

	ifdef	romdrvalone
	MOV	WORD PTR [BX.INIT_BREAK],offset initab   ;SET BREAK ADDRESS
	MOV	WORD PTR [BX.INIT_BREAK + 2],CS
	endif
	MOV	WORD PTR [BX.INIT_BPB],OFFSET INITAB	   ;SET POINTER TO BPB ARRAY
	MOV	WORD PTR [BX.INIT_BPB + 2],CS
	JMP	DEVEXIT
;

bios_data ends
	end
