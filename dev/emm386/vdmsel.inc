ifndef INC_LIST
.xlist
endif
;******************************************************************************
;
;   (C) Copyright MICROSOFT Corp. 1986-1991
;   (C) Copyright COMPAQ Computer Corp. 1986-1991
;
;   Title:    EMM386 - MICROSOFT Expanded Memory Manager 386
;
;   Module:   VDMSEL.INC - Selector Definitions for CEMM
;
;   Version:  2.00
;
;   Date:     May 12, 1986
;
;   Author:
;
;******************************************************************************
;
;   Change log:
;
;     DATE    REVISION			DESCRIPTION
;   --------  --------	-------------------------------------------------------
;	      Original
;   05/12/86  A-RRH	Cleanup and segment reorganization
;   06/07/86  C-SBP	merged with module from Rick
;   06/28/86  0.02	Name change from CEMM386 to CEMM (SBP).
;   07/10/86  0.05	Changed CODE_GSEL to RCODEA_GSEL (SBP).
;   05/08/86  2.00	Added RCODE_GSEL  and  RSS_GSEL (SBP).
;   10/12/88  3.32 (*D) VCPI added (DJM).
;   08/23/89  4.10  Added selctors for DEB386 and PAGED_GSEL (LC)
;
;******************************************************************************
;
;	DESCRIPTION
;
;	These are the fixed selector numbers for the GDT and LDT
;
;	All are declared with RPL = 0 so they can easily be used
;	as table indices.
;
;	Do not change anything without consulting files which include
;	this one. Add new selectors to the end and update the
;	gdt declaration and initialisation code elsewhere.
;

;	Define LDT and GDT table bits

TAB_LDT 	equ	4h
TAB_GDT 	equ	0h


;	Define GDT selectors

GDTD_GSEL	equ	08h OR TAB_GDT	; gdt data alias
IDTD_GSEL	equ	10h OR TAB_GDT	; idt data alias
LDT_GSEL	equ	18h OR TAB_GDT	; ldt
LDTD_GSEL	equ	20h OR TAB_GDT	; ldt data alias
TSS_GSEL	equ	28h OR TAB_GDT	; tss
TSSD_GSEL	equ	30h OR TAB_GDT	; tss data alias
RM_IDT_GSEL	equ	38h OR TAB_GDT	; real mode idt (locn 0)
ROMDATA_GSEL	equ	40h OR TAB_GDT	; maps 40:0 - DON'T CHANGE!!
VDMC_GSEL	equ	48h OR TAB_GDT	; VDM Code selector
VDMD_GSEL	equ	50h OR TAB_GDT	; VDM Data Selector
VDMS_GSEL	equ	58h OR TAB_GDT	; VDM stack selector
MONO_GSEL	equ	60h OR TAB_GDT	; monochrome display memory
COLOUR_GSEL	equ	68h OR TAB_GDT	; colour display memory
EGA1_GSEL	equ	70h OR TAB_GDT	; first EGA sel
EGA2_GSEL	equ	78h OR TAB_GDT	; second EGA sel
DATA32_GSEL	equ	80h OR TAB_GDT	; large linear addr data sel
DEBC_GSEL	equ	88h or TAB_GDT	; debugger work descr
DEBD_GSEL	equ	90h or TAB_GDT	; debugger work descr
DEBW1_GSEL	equ	98h or TAB_GDT	; debugger work descr
DEBW2_GSEL	equ	0A0h or TAB_GDT ; debugger work descr
K_PUTC286	equ	0A8h OR TAB_GDT ; 286 call gate to kputc
K_GETC286	equ	0B0h OR TAB_GDT ; 286 call gate to kgetc
RCODE_GSEL	equ	0B8h OR TAB_GDT ; R_CODE code segment selector
RCODEA_GSEL	equ	0C0h OR TAB_GDT ; data alias for R_CODE segment
RSS_GSEL	equ	0C8h OR TAB_GDT ; real mode SS alias
VM1_GSEL	equ	0D0h OR TAB_GDT ; scratch for vm handler
VM2_GSEL	equ	0D8h OR TAB_GDT ; scratch for vm handler
MBSRC_GSEL	equ	0E0h OR TAB_GDT ; source sel for move blk
MBTAR_GSEL	equ	0E8h OR TAB_GDT ; target sel for move blk
PAGET_GSEL	equ	0F0h OR TAB_GDT ; page table area sel
VDMCA_GSEL	equ	0F8h OR TAB_GDT ; VDM code segment alias
EMM1_GSEL	equ	100h OR TAB_GDT ; EMM scratch selector
EMM2_GSEL	equ	108h OR TAB_GDT ; EMM scratch selector
DIAG_GSEL	equ	110h OR TAB_GDT ; diagnostic port selector
PAGED_GSEL	equ	118h OR TAB_GDT ; page directory area sel
RMS_GSEL	equ	120h or TAB_GDT	; small protected mode base stack
R1CODE_GSEL	equ	128h or TAB_GDT	; R_CODE1 code selector
R1CODEA_GSEL	equ	130h or TAB_GDT	; R_CODE1 code selector
DEB1_GSEL	equ	138h or TAB_GDT	; deb386 word descr
DEB2_GSEL	equ	140h or TAB_GDT	; deb386 word descr
DEB3_GSEL	equ	148h or TAB_GDT	; deb386 word descr
DEB4_GSEL	equ	150h or TAB_GDT	; deb386 word descr
DEB5_GSEL	equ	158h or TAB_GDT	; deb386 word descr
DEBX_GSEL	equ	160h or TAB_GDT	; deb386 descr for 4G address space

.list	; end of VDMSEL.INC
