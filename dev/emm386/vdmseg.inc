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
;   Module:   VDMSEG.INC - Segment Ordering and attributes for VDM
;
;   Version:  0.04
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
;   06/03/86  C-SBP	changed order and added STACK and STACK0,STACK0_SIZE
;   06/06/86  C-SBP	changed CODE to _TEXT and DATA to _DATA
;   06/28/86  0.02	Name change from CEMM386 to CEMM (SBP).
;   07/05/86  0.04	Re-organized to allow code seg move (SBP).
;   10/12/88  3.32 (*D) VCPI added (DJM).
;
;******************************************************************************
;******************************************************************************
; SEGMENTS
;******************************************************************************

	; CODE area containing EMM/VDISK headers and writeable real mode code
R_CODE	segment para use16 public 'R_CODE'
R_CODE	ends

	;* Debugger Code and Data segments (if linked in)
dcode	segment public use16 word 'DCODE'
dcode	ends

ddata	segment public use16 word 'DDATA'
ddata	ends

	; Base memory stack
R_STACK	segment word use16 public 'R_STACK'
R_STACK	ends

R1_CODE	segment para use16 public 'R1_CODE'
R1_CODE	ends

	; Ring 0 stack in VDM
STACK	segment para use16 public 'STACK'
STACK	ends

	; Global Descriptor Table
GDT	segment para use16 common
GDT	ends

	; Local Descriptor Table
LDT	segment para use16 common
LDT	ends

	; Interrupt Descriptor Table
IDT	segment para use16 common
IDT	ends

ifdef TSSQLEO
	; Task State Segment
TSS	segment para use16 common
TSS	ends
endif

SGDT	segment para use16 common
SGDT	ends

SIDT	segment para use16 common
SIDT	ends

	; Contains initialized variables and variables used at startup.
_DATA	segment para use16 public 'DATA'
_DATA	ends

	; The code comprising the system.  Must be READ-ONLY in real mode.
_TEXT	segment para use16 public 'CODE'
_TEXT	ends

	; Initialization code and variables.  Used for data later.
	; Must be last segment.
LAST	segment para use16 public 'LAST'
LAST	ends

L_STACK	segment word use16 stack 'L_STACK'
L_STACK	ends

ABS0	segment USE16 at 0000h
	org 4*5h
int5	dw	?
	dw	?
	org 4*10h
int10	dw	?
	dw	?
	org 4*11h
int11	dw	?
	dw	?
	org 4*13h																															;LEO
int13	dd	?
	org 4*15h
int15	dd	?
	org 19h*4
int19	dd	?
	org 1Fh*4
int1F	dw	?
	dw	?
	org 21h*4
int21	dd	?
	org 2Fh*4
int2f	dd	?
	org 43h*4
int43	dw	?
	dw	?
	org 4*4Bh
int4B	dd	?
	org 4*6dh
int6d	dw	?
	dw	?
	org 67h*4
int67	dw	?
	dw	?
	org	47Bh
DBSflag	db	?
fDBSactive	equ	00100000b
fDBSchain	equ	00001000b
ABS0	ends

	;***	ROM BIOS data area, set up addresses
romdata segment use16 at 40h
	org	0Eh
pExtBIOS	dw	?
	org	10h
EqStatus	dw	?
	org	13h
Int12hMem	dw	?
	org	17h
KeyState	db	?
BreakState	db	?
AltKey		db	?
KbHead		dw	?
KbTail		dw	?
KbBuffer	dw	16 dup (?)
KbBufferEnd	label	word
	org	49h
VidMode 	db	?
	org	65h
VidReg		db	?
	org	71h
fBreak		db	?
fReset		dw	?
	org	80h
KbStart 	dw	?
KbEnd		dw	?
	org	97h
ATKbFlags	db	?
romdata ends

.list	; end of VDMSEG.INC
