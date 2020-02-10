.386p
page	58,132
;******************************************************************************
	title	TABDEF.ASM - 386 Protected Mode CPU Tables
;******************************************************************************
;
;   (C) Copyright MICROSOFT Corp. 1986-1991
;   (C) Copyright COMPAQ Computer Corp. 1986-1991
;
;   Title:    EMM386.EXE - MICROSOFT Expanded Memory Manager 386 Driver
;
;   Module:   TABDEF.ASM - 386 Protected Mode CPU Tables
;
;   Version:  2.00
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
;   01/31/86  A. Short	Tables for Standalone protected mode system
;	      A-WCC	Modified for Virtual DOS
;   05/12/86  B-RRH	Cleanup and segment reorganization
;   06/28/86  0.02	Name changed from CEMM386 to CEMM (SBP).
;   07/05/86  0.04	Moved KBD and PRINT to DCODE segment (SBP).
;   05/08/87  2.00	Added selectors for RCODE_GSEL and RSS_GSEL (SBP).
;   10/12/88  3.32 (*D) Add VCPI code (DJM).
;   08/23/89  4.10	Add DEB386 support & PAGED_GSEL (LC)
;
;******************************************************************************
;
;   Functional Description:
;
;******************************************************************************
.lfcond 				; list false conditionals

NAME	tabdef
;

.xlist
	include VDMseg.inc
	include VDMsel.inc
	include desc.inc
	include page.inc
.list


ifdef	BugMode
DCODE	SEGMENT
	extrn	kputc:far
	extrn	kgetc:far
DCODE	ENDS
endif

_TEXT SEGMENT
	extrn	ExceptHandler0:far
	extrn	ExceptHandler1:far
	extrn	ExceptHandler2:far
	extrn	ExceptHandler3:far
	extrn	ExceptHandler4:far
	extrn	ExceptHandler5:far
	extrn	ExceptHandler6:far
	extrn	ExceptHandler7:far
	extrn	ExceptHandler8:far
	extrn	ExceptHandler9:far
	extrn	ExceptHandler10:far
	extrn	ExceptHandler11:far
	extrn	ExceptHandler12:far
	extrn	ExceptHandler13:far
	extrn	ExceptHandler14:far
	extrn	ExceptHandler15:far
	extrn	ExceptHandler16:far
	extrn	ExceptHandler17:far

	extrn	EMM_pEntry:far
	extrn	pINT13hHandler:far
	extrn	pINT15hHandler:far
	extrn	pTrapHandler:far
	extrn	pINT4BhHandler:far

	extrn	pINT25hHandler:far
	extrn	pINT26hHandler:far
	extrn	pINT2ahHandler:far
	extrn	pINT2fhHandler:far
	extrn	pINT33hHandler:far
	extrn	pINT5chHandler:far

_TEXT ENDS


;***	GDT - Global Descriptor Table
;
;	This is the system GDT. Some parts are statically initialised,
;	others must be set up at run time, either because masm can't
;	calculate the data or it changes while the system is running.
;
;	WARNING
;
;	Don't change this without consulting "sel.inc", and the
;	routines which initialise the gdt.
;

GDT SEGMENT

gdtstart	label byte	; label for everyone to refer to the GDT

GDT_ENTRY	0, 0, 1, 0			; null selector
GDT_ENTRY	0, 0, 0, D_DATA0		; GDT alias
GDT_ENTRY	0, 0, 0, D_DATA0		; IDT alias
GDT_ENTRY	0, 0, 0, D_LDT0 		; LDT
GDT_ENTRY	0, 0, 0, D_DATA0		; LDT alias
GDT_ENTRY	0, 0, 0, D_386TSS0		; TSS
GDT_ENTRY	0, 0, 0, D_DATA0		; TSS alias
GDT_ENTRY	0, 0, <400h>, D_DATA3		; Real Mode IDT
GDT_ENTRY	400h, 0, 0, D_DATA0		; ROM Data
GDT_ENTRY	0, 0, 0, D_CODE0		; VDM Code
GDT_ENTRY	0, 0, 0, D_DATA0		; VDM Data
GDT_ENTRY	0, 0, 0, D_DATA0		; VDM Stack
GDT_ENTRY	0, 0bh, 1000h, D_DATA0		; Mono Display
GDT_ENTRY	8000h, 0bh, 4000h, D_DATA0	; Colour Disp
GDT_ENTRY	0, 0ah, 0, D_DATA0		; EGA Low
GDT_ENTRY	0, 0ch, 0, D_DATA0		; EGA High
	db	0FFh,0FFh,0,0,0,D_DATA0,0CFh,0	; DATA32 - large linear addr
GDT_ENTRY	0, 0, 0, 0			; debugger work
GDT_ENTRY	0, 0, 0, 0			; debugger work
GDT_ENTRY	0, 0, 0, 0			; debugger work
GDT_ENTRY	0, 0, 0, 0			; debugger work
ifndef	BugMode
GDT_ENTRY	0, 0, 0, 0			; general work
GDT_ENTRY	0, 0, 0, 0			; general work
else
IDT_ENTRY	DEBC_GSEL, <offset DCODE:kputc>, D_GATE3 ; call gate
IDT_ENTRY	DEBC_GSEL, <offset DCODE:kgetc>, D_GATE3 ; call gate
endif
GDT_ENTRY	0, 0, 0, D_CODE0		; R_CODE code segment selector
GDT_ENTRY	0, 0, 0, D_DATA0		; R_CODE data alias
GDT_ENTRY	0, 0, 0, D_DATA0		; real mode SS alias
GDT_ENTRY	0, 0, 0, D_DATA0		; VM1_GSEL - vm trap scratch
GDT_ENTRY	0, 0, 0, D_DATA0		; VM2_GSEL - vm trap scratch
GDT_ENTRY	0, 0, 0, D_DATA0		; MBSRC_GSEL - move blk scratch
GDT_ENTRY	0, 0, 0, D_DATA0		; MBTAR_GSEL - move blk scratch
GDT_ENTRY	0, 0, 0, D_DATA0		; PAGET_GSEL - page table area
GDT_ENTRY	0, 0, 0, D_DATA0		; VDM Code - Data Alias
GDT_ENTRY	0, 0, 0, D_DATA0		; EMM1 - EMM scratch selector
GDT_ENTRY	0, 0, 0, D_DATA0		; EMM2 - EMM scratch selector
GDT_ENTRY	0, 0, 0, D_DATA0		; extra entry
GDT_ENTRY	0, 0, 0, D_DATA0		; PAGED_GSEL page directory ;LEO
GDT_ENTRY	0, 0, 0, D_DATA0		; RMS_GSEL small protected mode base stack
GDT_ENTRY	0, 0, 0, D_CODE0		; R1CODE_GSEL - code selector
GDT_ENTRY	0, 0, 0, D_DATA0		; R1CODEA_GSEL - data selector
ifndef BugMode
GDT_ENTRY	0, 0, 0, 0			; debugger work 1
GDT_ENTRY	0, 0, 0, 0			; debugger work	2
GDT_ENTRY	0, 0, 0, 0			; debugger work	3
GDT_ENTRY	0, 0, 0, 0			; debugger work	4
GDT_ENTRY	0, 0, 0, 0			; debugger work	5
GDT_ENTRY	0, 0, 0, 0			; debugger work	(Addresses all memory)
endif
	public	GDTLEN
GDTLEN		equ	$ - gdtstart

GDT ENDS

ifdef TSSQLEO

;***	TSS for protected Mode
;
;	This is the VDM TSS. We only use one, for loading
;	SS:SP on privilige transitions. We don't use all
;	the 286 task switching stuff.
;
;

TSS	segment
;
	TssArea 	TSS386STRUC	<>
;
;   I/O Bit Map for Virtual Mode I/O trapping
;
	public	IOBitMap
IOBitMap	label	byte
	db	2000h dup (0)		; initialize all ports to NO trapping
	db	0FFh			; last byte is all 1's

	public	TSSLEN
TSSLEN		equ	$ - tss

TSS	ends
endif

;***	IDT for protected mode
;
;   This is the protected mode interrupt descriptor table.
;
;   The first 78h entries are defined.	Only processor exceptions and
;   hardware interrupts are fielded through the PM IDT.  Since the
;   gate DPLs are < 3, all software INTs are funneled through INT 13
;   (GP exception) and emulated.   Note that Null IDT entries and limit
;   exceptions produce the same results (GP error code) as DPL faults,
;   so we can use a truncated IDT.  This assumes no one is reprogramming
;   the 8259s base vector for some reason - don't know of any DOS apps
;   that do this.
;
IDT SEGMENT

idtstart	label byte

IDT_ENTRY	VDMC_GSEL,<offset _TEXT:ExceptHandler0>,D_386INT0 ; 00 Divide Error
ifndef	BugMode
IDT_ENTRY	VDMC_GSEL,<offset _TEXT:ExceptHandler1>,D_386INT0 ; 01 Debug
else
IDT_ENTRY	VDMC_GSEL,<offset _TEXT:ExceptHandler1>,D_386INT3 ; 01 Debug
endif
IDT_ENTRY	VDMC_GSEL,<offset _TEXT:ExceptHandler2>,D_386INT0 ; 02 NMI/287 Error
IDT_ENTRY	VDMC_GSEL,<offset _TEXT:ExceptHandler3>,D_386INT0 ; 03 Breakpoint
IDT_ENTRY	VDMC_GSEL,<offset _TEXT:ExceptHandler4>,D_386INT0 ; 04 INTO
IDT_ENTRY	VDMC_GSEL,<offset _TEXT:ExceptHandler5>,D_386INT0 ; 05 BOUND/Print Screen
IDT_ENTRY	VDMC_GSEL,<offset _TEXT:ExceptHandler6>,D_386INT0 ; 06 Invalid Opcode
IDT_ENTRY	VDMC_GSEL,<offset _TEXT:ExceptHandler7>,D_386INT0 ; 07 287 Not Available

IDT_ENTRY	VDMC_GSEL,<offset _TEXT:ExceptHandler8>,D_386INT0 ; 08 Double Exception
IDT_ENTRY	VDMC_GSEL,<offset _TEXT:ExceptHandler9>,D_386INT0 ; 09 (not on 386)
IDT_ENTRY	VDMC_GSEL,<offset _TEXT:ExceptHandler10>,D_386INT0 ; 0A Invalid TSS
IDT_ENTRY	VDMC_GSEL,<offset _TEXT:ExceptHandler11>,D_386INT0 ; 0B Segment Not Present
IDT_ENTRY	VDMC_GSEL,<offset _TEXT:ExceptHandler12>,D_386INT0 ; 0C Stack Fault
IDT_ENTRY	VDMC_GSEL,<offset _TEXT:ExceptHandler13>,D_386INT0 ; 0D General Protection
IDT_ENTRY	VDMC_GSEL,<offset _TEXT:ExceptHandler14>,D_386INT0 ; 0E Page Fault
IDT_ENTRY	VDMC_GSEL,<offset _TEXT:ExceptHandler15>,D_386INT0 ; 0F Intel Reserved

IDT_ENTRY	VDMC_GSEL,<offset _TEXT:ExceptHandler16>,D_386INT0 ; 10 Coprocessor error
;LEO IDT_ENTRY	0, 0, 0 		; 10 [287 Error]/Video INT (This exception
					;    cannot occur on AT/Explorer architecture)
IDT_ENTRY	VDMC_GSEL,<offset _TEXT:ExceptHandler17>,D_386INT0 ; 11 Equipment Check
;LEO IDT_ENTRY	0, 0, 0 		; 11 Equipment Check
IDT_ENTRY	0, 0, 0 		; 12 Memory Size
IDT_ENTRY VDMC_GSEL,<offset _TEXT:pINT13hHandler>,D_386INT3 ; 13 Disk INT  ;LEO
IDT_ENTRY	0, 0, 0 		; 14 RS232
IDT_ENTRY VDMC_GSEL,<offset _TEXT:pINT15hHandler>,D_386INT3 ; 15 MoveBlock,Post&Wait,MP
IDT_ENTRY	0, 0, 0 		; 16 Keyboard
IDT_ENTRY	0, 0, 0 		; 17 Printer

IDT_ENTRY	0, 0, 0 		; 18 Resident BASIC
IDT_ENTRY	0, 0, 0 		; 19 Bootstrap
IDT_ENTRY	0, 0, 0 		; 1A Time of Day
IDT_ENTRY	0, 0, 0 		; 1B Break
IDT_ENTRY	0, 0, 0 		; 1C Timer Tick
IDT_ENTRY	0, 0, 0 		; 1D Ptr to Video Param
IDT_ENTRY	0, 0, 0 		; 1E Ptr to Disk Params
IDT_ENTRY	0, 0, 0 		; 1F Ptr to Graphics

IDT_ENTRY	0, 0, 0 		; 20 DOS
IDT_ENTRY	0, 0, 0 		; 21 DOS
IDT_ENTRY	0, 0, 0 		; 22 DOS
IDT_ENTRY	0, 0, 0 		; 23 DOS
IDT_ENTRY	0, 0, 0 		; 24 DOS

;;IDT_ENTRY	0, 0, 0 		; 25 DOS
IDT_ENTRY VDMC_GSEL,<offset _TEXT:pINT25hHandler>,D_386INT3 ; ABS disk read

;;IDT_ENTRY	0, 0, 0 		; 26 DOS
IDT_ENTRY VDMC_GSEL,<offset _TEXT:pINT26hHandler>,D_386INT3 ; ABS disk write

IDT_ENTRY	0, 0, 0 		; 27 DOS

IDT_ENTRY	0, 0, 0 		; 28 DOS
IDT_ENTRY	0, 0, 0 		; 29 DOS

;;IDT_ENTRY	0, 0, 0 		; 2A DOS
IDT_ENTRY VDMC_GSEL,<offset _TEXT:pINT2ahHandler>,D_386INT3 ; LM

IDT_ENTRY	0, 0, 0 		; 2B DOS
IDT_ENTRY	0, 0, 0 		; 2C DOS
IDT_ENTRY	0, 0, 0 		; 2D DOS
IDT_ENTRY	0, 0, 0 		; 2E DOS

;;IDT_ENTRY	0, 0, 0 		; 2F ELIM
IDT_ENTRY VDMC_GSEL,<offset _TEXT:pINT2fhHandler>,D_386INT3 ; LM

IDT_ENTRY	0, 0, 0 		; 30 DOS
IDT_ENTRY	0, 0, 0 		; 31 DOS
IDT_ENTRY	0, 0, 0 		; 32 DOS

;;IDT_ENTRY	0, 0, 0 		; 33 DOS
IDT_ENTRY VDMC_GSEL,<offset _TEXT:pINT33hHandler>,D_386INT3 ; mouse

IDT_ENTRY	0, 0, 0 		; 34 DOS
IDT_ENTRY	0, 0, 0 		; 35 DOS
IDT_ENTRY	0, 0, 0 		; 36 DOS
IDT_ENTRY	0, 0, 0 		; 37 DOS

IDT_ENTRY	0, 0, 0 		; 38 DOS
IDT_ENTRY	0, 0, 0 		; 39 DOS
IDT_ENTRY	0, 0, 0 		; 3A DOS
IDT_ENTRY	0, 0, 0 		; 3B DOS
IDT_ENTRY	0, 0, 0 		; 3C DOS
IDT_ENTRY	0, 0, 0 		; 3D DOS
IDT_ENTRY	0, 0, 0 		; 3E DOS
IDT_ENTRY	0, 0, 0 		; 3F DOS

IDT_ENTRY	0, 0, 0 		; 40 Reserved
IDT_ENTRY	0, 0, 0 		; 41 Reserved
IDT_ENTRY	0, 0, 0 		; 42 Reserved
IDT_ENTRY	0, 0, 0 		; 43 Reserved
IDT_ENTRY	0, 0, 0 		; 44 Reserved
IDT_ENTRY	0, 0, 0 		; 45 Reserved
IDT_ENTRY	0, 0, 0 		; 46 Reserved
IDT_ENTRY	0, 0, 0 		; 47 Reserved

IDT_ENTRY	0, 0, 0 		; 48 Reserved
IDT_ENTRY	0, 0, 0 		; 49 Reserved
IDT_ENTRY	0, 0, 0 		; 4A Reserved
IDT_ENTRY VDMC_GSEL,<offset _TEXT:pINT4BhHandler>,D_386INT3 ; 4B: DMAserv  ;LEO
IDT_ENTRY	0, 0, 0 		; 4C Reserved
IDT_ENTRY	0, 0, 0 		; 4D Reserved
IDT_ENTRY	0, 0, 0 		; 4E Reserved
IDT_ENTRY	0, 0, 0 		; 4F Reserved

IDT_ENTRY	0, 0, 0 		; 50 Reserved
IDT_ENTRY	0, 0, 0 		; 51 Reserved
IDT_ENTRY	0, 0, 0 		; 52 Reserved
IDT_ENTRY	0, 0, 0 		; 53 Reserved
IDT_ENTRY	0, 0, 0 		; 54 Reserved
IDT_ENTRY	0, 0, 0 		; 55 Reserved
IDT_ENTRY	0, 0, 0 		; 56 Reserved
IDT_ENTRY	0, 0, 0 		; 57 Reserved
IDT_ENTRY	0, 0, 0 		; 58 Reserved
IDT_ENTRY	0, 0, 0 		; 59 Reserved
IDT_ENTRY	0, 0, 0 		; 5A Reserved
IDT_ENTRY	0, 0, 0 		; 5B Reserved

;;IDT_ENTRY	0, 0, 0 		; 5C Reserved
IDT_ENTRY VDMC_GSEL,<offset _TEXT:pINT5chHandler>,D_386INT3 ; LM

IDT_ENTRY	0, 0, 0 		; 5D Reserved
IDT_ENTRY	0, 0, 0 		; 5E Reserved
IDT_ENTRY	0, 0, 0 		; 5F Reserved

IDT_ENTRY	0, 0, 0 		; 60 User Programs
IDT_ENTRY	0, 0, 0 		; 61 User Programs
IDT_ENTRY	0, 0, 0 		; 62 User Programs
IDT_ENTRY	0, 0, 0 		; 63 User Programs
IDT_ENTRY	0, 0, 0 		; 64 User Programs
IDT_ENTRY	0, 0, 0 		; 65 User Programs
IDT_ENTRY	0, 0, 0 		; 66 User Programs
IDT_ENTRY	VDMC_GSEL,<offset _TEXT:EMM_pEntry>,D_386INT3 ; 67 ELIM

IDT_ENTRY	0, 0, 0 		; 68 Not Used
IDT_ENTRY	0, 0, 0 		; 69 Not Used
IDT_ENTRY	0, 0, 0 		; 6A Not Used
IDT_ENTRY	0, 0, 0 		; 6B Not Used
IDT_ENTRY	0, 0, 0 		; 6C Not Used
IDT_ENTRY	0, 0, 0 		; 6D Not Used
IDT_ENTRY	0, 0, 0 		; 6E Not Used
IDT_ENTRY	0, 0, 0 		; 6F Not Used

;
;  The following table entries extend the IDT to cover the full 256 possible
;  IDT entries.  This is provided for the VCPI interface since it allows the
;  remapping of the 8259 chips to any vector.
;

IDT_ENTRY	0, 0, 0 		; 70h
IDT_ENTRY	0, 0, 0 		; 71h
IDT_ENTRY	0, 0, 0 		; 72h
IDT_ENTRY	0, 0, 0 		; 73h
IDT_ENTRY	0, 0, 0 		; 74h
IDT_ENTRY	0, 0, 0 		; 75h
IDT_ENTRY	0, 0, 0 		; 76h
IDT_ENTRY	0, 0, 0 		; 77h
IDT_ENTRY	0, 0, 0 		; 78h
IDT_ENTRY	0, 0, 0 		; 79h
IDT_ENTRY	0, 0, 0 		; 7Ah
IDT_ENTRY	0, 0, 0 		; 7Bh
IDT_ENTRY	0, 0, 0 		; 7Ch
IDT_ENTRY	0, 0, 0 		; 7Dh
IDT_ENTRY	0, 0, 0 		; 7Eh
IDT_ENTRY	0, 0, 0 		; 7Fh

IDT_ENTRY	0, 0, 0 		; 80h
IDT_ENTRY	0, 0, 0 		; 81h
IDT_ENTRY	0, 0, 0 		; 82h
IDT_ENTRY	0, 0, 0 		; 83h
IDT_ENTRY	0, 0, 0 		; 84h
IDT_ENTRY	0, 0, 0 		; 85h
IDT_ENTRY	0, 0, 0 		; 86h
IDT_ENTRY	0, 0, 0 		; 87h
IDT_ENTRY	0, 0, 0 		; 88h
IDT_ENTRY	0, 0, 0 		; 89h
IDT_ENTRY	0, 0, 0 		; 8Ah
IDT_ENTRY	0, 0, 0 		; 8Bh
IDT_ENTRY	0, 0, 0 		; 8Ch
IDT_ENTRY	0, 0, 0 		; 8Dh
IDT_ENTRY	0, 0, 0 		; 8Eh
IDT_ENTRY	0, 0, 0 		; 8Fh

IDT_ENTRY	0, 0, 0 		; 90h
IDT_ENTRY	0, 0, 0 		; 91h
IDT_ENTRY	0, 0, 0 		; 92h
IDT_ENTRY	0, 0, 0 		; 93h
IDT_ENTRY	0, 0, 0 		; 94h
IDT_ENTRY	0, 0, 0 		; 95h
IDT_ENTRY	0, 0, 0 		; 96h
IDT_ENTRY	0, 0, 0 		; 97h
IDT_ENTRY	0, 0, 0 		; 98h
IDT_ENTRY	0, 0, 0 		; 99h
IDT_ENTRY	0, 0, 0 		; 9Ah
IDT_ENTRY	0, 0, 0 		; 9Bh
IDT_ENTRY	0, 0, 0 		; 9Ch
IDT_ENTRY	0, 0, 0 		; 9Dh
IDT_ENTRY	0, 0, 0 		; 9Eh
IDT_ENTRY	0, 0, 0 		; 9Fh
IDT_ENTRY	0, 0, 0 		; A0h
IDT_ENTRY	0, 0, 0 		; A1h
IDT_ENTRY	0, 0, 0 		; A2h
IDT_ENTRY	0, 0, 0 		; A3h
IDT_ENTRY	0, 0, 0 		; A4h
IDT_ENTRY	0, 0, 0 		; A5h
IDT_ENTRY	0, 0, 0 		; A6h
IDT_ENTRY	0, 0, 0 		; A7h
IDT_ENTRY	0, 0, 0 		; A8h
IDT_ENTRY	0, 0, 0 		; A9h
IDT_ENTRY	0, 0, 0 		; AAh
IDT_ENTRY	0, 0, 0 		; ABh
IDT_ENTRY	0, 0, 0 		; ACh
IDT_ENTRY	0, 0, 0 		; ADh
IDT_ENTRY	0, 0, 0 		; AEh
IDT_ENTRY	0, 0, 0 		; AFh
IDT_ENTRY	0, 0, 0 		; B0h
IDT_ENTRY	0, 0, 0 		; B1h
IDT_ENTRY	0, 0, 0 		; B2h
IDT_ENTRY	0, 0, 0 		; B3h
IDT_ENTRY	0, 0, 0 		; B4h
IDT_ENTRY	0, 0, 0 		; B5h
IDT_ENTRY	0, 0, 0 		; B6h
IDT_ENTRY	0, 0, 0 		; B7h
IDT_ENTRY	0, 0, 0 		; B8h
IDT_ENTRY	0, 0, 0 		; B9h
IDT_ENTRY	0, 0, 0 		; BAh
IDT_ENTRY	0, 0, 0 		; BBh
IDT_ENTRY	0, 0, 0 		; BCh
IDT_ENTRY	0, 0, 0 		; BDh
IDT_ENTRY	0, 0, 0 		; BEh
IDT_ENTRY	0, 0, 0 		; BFh
IDT_ENTRY	0, 0, 0 		; C0h
IDT_ENTRY	0, 0, 0 		; C1h
IDT_ENTRY	0, 0, 0 		; C2h
IDT_ENTRY	0, 0, 0 		; C3h
IDT_ENTRY	0, 0, 0 		; C4h
IDT_ENTRY	0, 0, 0 		; C5h
IDT_ENTRY	0, 0, 0 		; C6h
IDT_ENTRY	0, 0, 0 		; C7h
IDT_ENTRY	0, 0, 0 		; C8h
IDT_ENTRY	0, 0, 0 		; C9h
IDT_ENTRY	0, 0, 0 		; CAh
IDT_ENTRY	0, 0, 0 		; CBh
IDT_ENTRY	0, 0, 0 		; CCh
IDT_ENTRY	0, 0, 0 		; CDh
IDT_ENTRY	0, 0, 0 		; CEh
IDT_ENTRY	0, 0, 0 		; CFh
IDT_ENTRY	0, 0, 0 		; D0h
IDT_ENTRY	0, 0, 0 		; D1h
IDT_ENTRY	0, 0, 0 		; D2h
IDT_ENTRY	0, 0, 0 		; D3h
IDT_ENTRY	0, 0, 0 		; D4h
IDT_ENTRY	0, 0, 0 		; D5h
IDT_ENTRY	0, 0, 0 		; D6h
IDT_ENTRY	0, 0, 0 		; D7h
IDT_ENTRY	0, 0, 0 		; D8h
IDT_ENTRY	0, 0, 0 		; D9h
IDT_ENTRY	0, 0, 0 		; DAh
IDT_ENTRY	0, 0, 0 		; DBh
IDT_ENTRY	0, 0, 0 		; DCh
IDT_ENTRY	0, 0, 0 		; DDh
IDT_ENTRY	0, 0, 0 		; DEh
IDT_ENTRY	0, 0, 0 		; DFh
IDT_ENTRY	0, 0, 0 		; E0h
IDT_ENTRY	0, 0, 0 		; E1h
IDT_ENTRY	0, 0, 0 		; E2h
IDT_ENTRY	0, 0, 0 		; E3h
IDT_ENTRY	0, 0, 0 		; E4h
IDT_ENTRY	0, 0, 0 		; E5h
IDT_ENTRY	0, 0, 0 		; E6h
IDT_ENTRY	0, 0, 0 		; E7h
IDT_ENTRY	0, 0, 0 		; E8h
IDT_ENTRY	0, 0, 0 		; E9h
IDT_ENTRY	0, 0, 0 		; EAh
IDT_ENTRY	0, 0, 0 		; EBh
IDT_ENTRY	0, 0, 0 		; ECh
IDT_ENTRY	0, 0, 0 		; EDh
IDT_ENTRY	0, 0, 0 		; EEh
IDT_ENTRY	0, 0, 0 		; EFh
IDT_ENTRY	0, 0, 0 		; F0h
IDT_ENTRY	0, 0, 0 		; F1h
IDT_ENTRY	0, 0, 0 		; F2h
IDT_ENTRY	0, 0, 0 		; F3h
IDT_ENTRY	0, 0, 0 		; F4h
IDT_ENTRY	0, 0, 0 		; F5h
IDT_ENTRY	0, 0, 0 		; F6h
IDT_ENTRY	0, 0, 0 		; F7h
IDT_ENTRY	0, 0, 0 		; F8h
IDT_ENTRY	0, 0, 0 		; F9h
IDT_ENTRY	0, 0, 0 		; FAh
IDT_ENTRY	0, 0, 0 		; FBh
IDT_ENTRY	0, 0, 0 		; FCh
IDT_ENTRY	0, 0, 0 		; FDh
IDT_ENTRY	0, 0, 0 		; FEh
IDT_ENTRY	0, 0, 0 		; FFh

	public	IDTLEN
idtlen		equ	this byte - idtstart

IDT ends

END
