.386p
page	58,132
;******************************************************************************
	title	InitDeb - initialize debugger
;******************************************************************************
;
;   (C) Copyright MICROSOFT Corp. 1986-1991
;   (C) Copyright COMPAQ Computer Corp. 1986-1991
;
;   Title:    EMM386.EXE - MICROSOFT Expanded Memory Manager 386 Driver
;
;   Module:   InitDeb - initialize debugger
;
;   Version:  2.00
;
;   Date:     June 16,1986
;
;   Author:   Steve Preston
;
;******************************************************************************
;
;   Change log:
;
;     DATE    REVISION			DESCRIPTION
;   --------  --------	-------------------------------------------------------
;   06/16/86  Original	from VDM MAIN.ASM module
;   06/28/86  0.02	Name changed from CEMM386 to CEMM (SBP).
;   07/06/86  0.04	Changed assume to _DATA (SBP).
;   05/18/87  2.00	Changed GDT_Seg to R_CODE (SBP).
;
;******************************************************************************
;
;   Functional Description:
;
;	This routine is linked in when linking with the kernel debugger.
;    InitDeb calls the debugger initialization routine.
;
;******************************************************************************
.lfcond 				; list false conditionals
;******************************************************************************
;	P U B L I C S
;******************************************************************************

	public	InitDeb
	public	ReInitDeb

;******************************************************************************
;	D E F I N E S
;******************************************************************************
	include vdmseg.inc
	include vdmsel.inc
	include desc.inc
	include emmfunct.inc
	include emm386.inc
;
; Definition of the packet used in debug initialization. A pointer to
; this structure is passed to Debug_Entry.
;
DebugInit	struc
	CSseg		dw	?		;Real mode code segment
	DSseg		dw	?		;Real mode data segment
	CSsel		dw	?		;Prot mode code selector
	DSsel		dw	?		;Prot mode data selector
	SpareSel1	dw	?		;Prot mode alias selector 1
	SpareSel2	dw	?		;Prot mode alias selector 2
	GDTalias	dw	?		;Prot mode GDT r/w alias
	ProtIDTaddr	dq	?		;Prot mode IDT base & limit
	RealIDTaddr	dq	?		;Real mode IDT base & limit
	BrkFlag 	db	?		;TRUE if break to debugger
	ComFlag 	db	?		;TRUE if com1, FALSE if com2
DebugInit	ends


;******************************************************************************
;	E X T E R N A L   R E F E R E N C E S
;******************************************************************************
ifdef	BugMode
dcode	segment
extrn	_Debug_Entry:far	; (debinit.asm)
dcode	ends
endif

R_CODE	SEGMENT
extrn	GDT_Seg:word
extrn	IDT_Seg:word
extrn	GenFlags:dword
R_CODE ENDS

;******************************************************************************
;	S E G M E N T	D E F I N I T I O N S
;******************************************************************************

ifdef	BugMode
_DATA	SEGMENT
InitData	DebugInit	<>
_DATA ENDS
endif

R_CODE	segment
	public	DebugEntry
DebugEntry	dd	R_CODE:NoDebugEntry

	public	DebBreakStr
DebBreakStr	label	byte
ifdef DEBUG
		db	'EMM386 Initialization',13,10
endif
		db	0
R_CODE	ends

LAST SEGMENT

ifndef BugMode

MyDebugEntry	dd	0

debugddname	db	'$debugdd',0
emm386name	db	'emm386',0

debsegtable	label	word
	dw	R_CODE, 	R_CODE, 	RCODE_GSEL,	5040h
	dw	R_STACK,	R_STACK,	RMS_GSEL,	5041h
	dw	R1_CODE,	R1_CODE,	R1CODE_GSEL,	5040h
	dw	STACK,		VDMS_GSEL,	0,		5001h
	dw	GDT,		GDT,		GDTD_GSEL,	5041h
	dw	LDT,		LDT,		LDTD_GSEL,	5041h
	dw	IDT,		IDT,		IDTD_GSEL,	5041h
	dw	SGDT,		SGDT,		0,		5011h
	dw	SIDT,		SIDT,		0,		5011h
	dw	_DATA,		VDMD_GSEL,	0,		5001h
	dw	_TEXT,		VDMC_GSEL,	0,		5000h
	dw	LAST,		LAST,		0,		5011h
	dw	L_STACK,	L_STACK,	0,		5011h
debsegtablelen	equ	($ - offset LAST:debsegtable)/8
endif
	assume cs:LAST, ds:_DATA, es:_DATA

;******************************************************************************
;	InitDeb - initialize kernel debugger
;
;
;    ENTRY:	Real Mode
;		DS = _DATA
;		AL = 00h => dont't break on debug init
;		AL = FFh => break on debug init
;
;    EXIT:	Real Mode
;		Kernel debugger initialized
;
;    USED:	none
;
;	NOTE:  this routine assumes that the GDT is addressible in real mode
;
;******************************************************************************
InitDeb proc	near
;
ifdef	BugMode
	pusha
	push	ds
	push	es
;
	push	ds
	pop	es		; ES = data
;
	mov	di, offset _DATA:InitData

	mov	bx, dcode
	mov	[di].CSseg, bx
	mov	bx, ddata
	mov	[di].DSseg, bx
	mov	[di].CSsel, DEBC_GSEL
	mov	[di].DSsel, DEBD_GSEL
	mov	[di].SpareSel1, DEBW1_GSEL
	mov	[di].SpareSel2, DEBW2_GSEL
	mov	[di].GDTalias, GDTD_GSEL
	mov	[di].BrkFlag, al		; ? break on entry ?
	mov	[di].ComFlag, FALSE		; com2

	sidt	fword ptr [di].RealIDTaddr

	push	ds
	push	di
	mov	ax, seg R_CODE
	mov	ds,ax			; DS -> R_CODE
	ASSUME	DS:R_CODE

	mov	ax, [GDT_Seg]
	mov	ds, ax			; DS -> GDT

	db	66h
	lgdt	fword ptr ds:[GDTD_GSEL]

	mov	si, IDTD_GSEL
	mov	cx, 6
	lea	di, [di].ProtIDTaddr
	cld
	rep movsb
	pop	di
	pop	ds
	ASSUME	DS:_DATA

	call	_Debug_Entry
;
; and return
;
	pop	es
	pop	ds
	popa

else

    ASSUME   DS:NOTHING, ES:NOTHING, SS:NOTHING, GS:NOTHING, FS:NOTHING

; Initialize DEBX_GSEL GDT Selector and Deb386

	pusha
	push	ds
	push	es

	mov	ax,cs
	mov	ds,ax
    ASSUME   DS:LAST
	mov	dx,offset LAST:debugddname	; ds:dx->db '$DebugDD',0
	mov	ax,3D00h
	int	21h
	jc	no_debdd

	mov	bx,ax
	mov	ax,R_CODE
	mov	ds,ax				; DS -> R_CODE
    ASSUME   DS:R_CODE
	mov	dx,offset R_CODE:DebugEntry	; ds:dx->dd ?
	mov	cx,3001h
	mov	ax,440Ch
	int	21h

	mov	ah,3Eh
	int	21h
	jmp	short deb_init

no_debdd:
	mov	ax,R_CODE
	mov	ds, ax
    ASSUME   DS:R_CODE
	mov	ax,3568h			; get debugger int vector (68h)
	int	21h
	mov	ax,es
	or	ax,bx				; Q: non-zero vector?
	jz	deb_done			;  N: exit

	mov	ah,43h				; identify
	int	68h
	cmp	ax,0f386h			; Q: debugger exists?
	jne	short deb_done			;  N: exit

	mov	word ptr [DebugEntry],offset R_CODE:Int68DebugEntry

deb_init:
    ASSUME   DS:R_CODE

	bts	[GenFlags], fDebugActiveBit
	mov	ax, word ptr [DebugEntry]
	mov	word ptr [MyDebugEntry], ax
	mov	ax, word ptr [DebugEntry+2]
	mov	word ptr [MyDebugEntry+2], ax

	mov	es,[IDT_Seg]				
	xor	di,di				; ES:DI <-- IDT

	mov	ds,[GDT_Seg]
    ASSUME   DS:NOTHING
	xor	si,si				; DS:SI <-- GDT

	mov	bx,DEBX_GSEL
	and	bl,SEL_LOW_MASK
	mov	word ptr [bx],0ffffh		; Low Limit = 0ffffh
	mov	word ptr [bx + 2],0
	mov	byte ptr [bx + 4],0		; Base = 0
	mov	byte ptr [bx + 5],D_DATA0	; Ring 0 Data
	mov	byte ptr [bx + 6],8fh		; Page Granularity/High Limit
	mov	byte ptr [bx + 7],0

	mov	bx,DEBX_GSEL			; BIG selector for all addresses
	mov	cx,DEB1_GSEL			; start of 5 working Selector
	mov	dx,GDTD_GSEL
	mov	ax,4400h			; Initialize Deb386
	call	[MyDebugEntry]

	mov	ax,LAST
	mov	es,ax
	mov	ds,ax
	mov	di,offset LAST:emm386name	; (ES:DI) = module name
	mov	si,offset LAST:debsegtable	; (DS:SI) = segment table
	mov	cx,debsegtablelen
deb_sym_loop:
	lodsw
	mov	bx,ax
	sub	bx,R_CODE			; (BX) = segment number
	push	cx
	lodsw
	mov	cx,ax				; (CX) = segment/selector
	lodsw
	mov	dx,ax				; (DX) = selector
	lodsw					; (AX) = function/type
	call	MyDebugEntry
	pop	cx
	loop	deb_sym_loop

deb_done:
	pop	es
	pop	ds
	popa
endif
	ret
InitDeb endp


ifdef DEBUG

;******************************************************************************
;	DebDefineSeg - Define EMM386 segment to debugger
;
;
;    ENTRY:	Real/Virtual Mode
;		AL = segment type for debugger
;		BX = segment number as fixed up by loader
;		CX = segment address
;		DX = segment selector
;
;    EXIT:	Real Mode
;		Kernel debugger initialized
;
;    USED:	AX
;
;******************************************************************************

	public	DebDefineSeg

DebDefineSeg	proc	near

	push	es
	push	di

	mov	ah, 50h
	sub	bx, R_CODE		; convert to segment #

	mov	di,LAST
	mov	es,di
	mov	di,offset LAST:emm386name	; (ES:DI) = module name

	call	[MyDebugEntry]

	pop	di
	pop	es
	ret

DebDefineSeg	endp

endif

LAST	ends

R_CODE	segment
    ASSUME   CS:R_CODE

ReInitDeb proc near
    ASSUME   DS:NOTHING,ES:NOTHING,SS:NOTHING,GS:NOTHING,FS:NOTHING
	pusha
	mov	bx,DEBX_GSEL			; BIG selector for all addresses
	mov	cx,DEB1_GSEL			; start of 5 working Selector
	mov	dx,GDTD_GSEL
	mov	ax,4a00h			; Reinitialize Deb386
	call	[DebugEntry]
	popa
	ret
ReInitDeb endp

Int68DebugEntry proc far
    ASSUME   DS:NOTHING,ES:NOTHING,SS:NOTHING,GS:NOTHING,FS:NOTHING
	int	68h
NoDebugEntry:
	ret
Int68DebugEntry endp

R_CODE	ends

	END
