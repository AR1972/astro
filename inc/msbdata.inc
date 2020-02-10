;
;	Microsoft Confidential
;	Copyright (C) Microsoft Corporation 1991
;	All Rights Reserved.
;

; MSBDATA.INC
;
; This file defines some locations in the BIOS data segment that are
; accessed by code outside the BIOS, e.g. by the DOS and by the ROMDOS
; loader.
;
	include	oemconf.inc


	ifndef	In_Bios		; if we're actually running in the BIOS,
;				;  then our segment is already open and
;				;  we'll actually generate code

Bdata	segment at 70h
	assume	cs:Bdata
	org	3		; fixed location in Bios_Data segment
int_2f	=	0		; define dummy so msbdata will assemble

	endif

	public	dosdatasg
DosDataSg dw	0		; Let DOS find it's data segment through here
				; Note: this value is set by the ROMDOS loader
				; to tell where the DOS data and init code
				; has been loaded into RAM


;	dos's int 2f handler will exit via a jump through here.  This is
;	  how the BIOS hooks int2f
	
	public	bios_i2f	; M012
bios_i2f proc	far
;	jmp	int_2f		; assembler won't generate forward
;				;  reference to a far label
	db	0eah
	dd	int_2f		; generate manual far jump
bios_i2f endp

ifdef	ROMEXEC
	public	romstartaddr
romstartaddr	dw	ROMEXEC_SEG ; The start address for the romfind routines
				    ; is obtained from here (70:A)
else
	public	romstartaddr
romstartaddr	dw	0	; The start address for the romfind routines
				; This is to maintain binary compatibilty
				;  with DISK based DOS 5.0
endif


; This is a byte used for special key handling in the resident
; console device driver.  It must be here so that it can be included
; in the WIN386 instance table (in INC\LMSTUB.ASM).

	public	altah

altah		db	0	;special key handling


ifdef	ROMDOS
; ROMDOS support:
;   The following values in the BIOS data segment are set by the ROMDOS
;   loader at boot time before the BIOS starts running.

	public	BIOS_Res
BIOS_Res	dw	?	; segment addr of BIOS resident code (in ROM)

	public	DOS_Res
DOS_Res		dw	?	; segment addr of DOS resident code (in ROM)

	public	BootFlags
BootFlags	dw	?	; boot options from CMOS or multiboot menu


endif
; 
; End of ROMDOS support

	ifndef	In_Bios
Bdata	ends
	endif

