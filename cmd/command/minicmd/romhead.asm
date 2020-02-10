	page    ,132
	title   ROM COMMAND Resident ROM Header

comment %------------------------------------------------------------------

This module provides a standard ROM header for ROM COMMAND as
described in the IBM System BIOS technical reference.

MS-DOS ROM $Exec header is also supplied for COMMAND launch.

The size field and checksum of the BIOS-compatible ROM module are set
when putting this object code together with other code granules to
make the ROM module.

%--------------------------------------------------------------------------



ROMHEADER	segment


;***	Adapter ROM module signature and size
;
;	The signature must be aligned on a 2K address boundary.
;	A checksum of bytes in the specified range must be zero
;	for the ROM module to be valid.

	db      55h,0AAh                ; ROM signature
	db      08h			; ROM size in 512-byte pages
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
	ROMFile	COMMAND.COM, Launch	; COMMAND.COM alias for command.com

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

;	------------------------------------------------------------
	ifdef	COMMAND_SEG
;	COMMAND image is in first megabyte.  Cool.
	
	mov	ax,COMMAND_SEG		; AX = COMMAND image seg addr
	push	ax
	xor	ax, ax
	push	ax
	retf

	else
	%out MINICOMMAND should be below 1 megabyte
	.err
	endif

ROMHEADER	ends

	end

