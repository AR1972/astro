.386p
page	58,132
;******************************************************************************
	title	MODESW.ASM - 386 Mode Switching Support Routines
;******************************************************************************
;
;   (C) Copyright MICROSOFT Corp. 1986-1991
;   (C) Copyright COMPAQ Computer Corp. 1986-1991
;
;   Title:    EMM386.EXE - MICROSOFT Expanded Memory Manager 386 Driver
;
;   Module:   MODESW.ASM - PC/AT Mode Switching Support Routines
;
;   Version:  2.02
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
;   01/31/86  Original
;   02/10/86  A-WCC	Removed STI in Enable/Disable A20 to keep debugger sane
;   05/12/86  B-RRH	Cleanup and segment reorganization
;   06/21/86  0.02	Added CLD.
;   06/28/86  0.02	Name changed from CEMM386 to CEMM (SBP).
;   07/05/86  0.04	Removed Real286
;   07/06/86  0.04	changed assume to _DATA (SBP).
;   05/08/87  2.00	moved to R_CODE segment (SBP) and added
;			GoProtMode, GoRealMode, GoVirtualMode
;   07/13/87  2.01	Fixed bug-> was not setting DS -> R_CODE before
;			calling CheckLock (SBP).
;   08/05/87  2.02	Fixed bug-> GoProt was not clearing ECX before saving
;			stack segment (SBP).
;   07/22/88  3.31 (*C) Fix 8042 handling for non-password machines (RDV).
;   01/15/89  4.00 (*D) Added generic 8042 detect (RDV)
;
;******************************************************************************
;
;   Functional Description:
;
;	This file contains routines for switching the Deskpro 386 between
;	real, protected, and virtual modes.  In general these routines
;	preserve the current register contents and maintain the current stack.
;
;******************************************************************************
.lfcond 				; list false conditionals

	public	GoProtMode
	public	GoRealMode
	public	GoVirtualMode
	public	EnableA20
	public	DisableA20
;******************************************************************************
;	D E F I N E S
;******************************************************************************

	include VDMseg.inc
	include VDMsel.inc
	include desc.inc
	include oemdep.inc
	include emm386.inc
	include emmfunct.inc
	include	emmdata.inc
	include	xmm.inc
	include	vm386.inc

;******************************************************************************
;	E X T E R N A L   R E F E R E N C E S
;******************************************************************************
R_CODE	SEGMENT
	extrn	SelToSeg:near
	extrn	XMMcontrol:dword					;HKN
ifdef MSFLAG
	extrn 	GetA20cnt:near
	extrn 	SetA20cnt:near
endif
R_CODE	ENDS

	page
R_CODE	SEGMENT
	assume cs:R_CODE,ds:R_CODE,es:R_CODE

;******************************************************************************
;	GoProtMode - go to 386 protected mode
;
;	The following CPU registers are changed to have values
;	appropriate to protected mode operation:
;
;		CS, DS, ES, SS, TR, LDT, Flags, MSW, GDT, IDT
;
;    ENTRY:	Real Mode
;		CLI
;		R_CODE:[PageD_Addr] = dword physical address of page directory
;		R_CODE:[GDT_Ptr] = GDT pointer for LGDT
;		R_CODE:[IDT_Ptr] = IDT pointer for LIDT
;
;    EXIT:	PROTECTED MODE
;		CLI
;		CS = RCODE_GSEL
;		DS,ES,FS,GS = RCODEA_GSEL
;		SS = selector for entry real mode SS segment
;		A20 enabled
;		high system memory LOCKED
;
;    USED:	flags
;
;******************************************************************************
GoProtMode	proc	near
	push	eax
	push	bx
	push	ecx

;QLEO	call	CheckLOCK		; check on initial state of table lock
;
;  A20 line must be on while in protected mode
;
	test	cs:[Current_State],fState_A20Ena;Q: A20 enabled?
ifdef MSFLAG
	jnz	GPM_getcount		; Y: don't need to enable it!
else
	jnz	short GPMcont		; Y: don't need to enable it!
endif
	call	EnableA20		; N: enable A20 address line
	jc	short GPMexit		; could not enable A20

GPMcont:
;
;    load CR3 register for paging
;
	mov	eax,cr3
	and	eax,0FFFh		; clear address
	or	eax,cs:[PageD_Addr]	; EAX = 32-bit address of Page Dir
	mov	cr3,eax
;
;Disable NMIs ...
;
	mov	al,DISABLE_NMI
	out	NMI_CMD,al

;
;   load gdt and ldt base registers  (DB 66h needed for 32 bit address)
;
	db	66h
	lgdt	fword ptr CS:[GDT_ptr]
	db	66h
	lidt	fword ptr CS:[IDT_ptr]
;
;   go protected and enable paging - turn on bits in CR0
;
	mov	eax,cr0

	or	eax,(80000000h + MSW_PROTECT) ; or EAX,imm32
		; - enable PE bit - PROT MODE, enable PG bit - PAGING

	mov	cr0,eax

;	far jump to flush prefetch, and reload CS

	db	0eah			; far jmp opcode
	dw	offset R_CODE:pm1	; offset
	dw	RCODE_GSEL		; selector
pm1:
;
;   We are now protected, set the Task Register and LDT Register
;
	mov	ax,TSS_GSEL
	ltr	ax

	xor	ax, ax			; LDT is null, not needed
	lldt	ax
;
;  Insure that CR2 is 0: This allows determination of Page Faults
;
	xor	ecx,ecx
	mov	cr2,ecx
;
; set up SS to entry stack
;
	;
	; save entry SS,SP
	;
;QLEO	xor	ecx,ecx		; clear high word
	mov	bx,sp		; save SP
	mov	cx,ss		; and SS

	;
	;  reset SS,SP -> entry stack
	;  set base of RSS_GSEL segment -> real mode SS segment
	;	entry: CX = segment of entry stack
	;	       BX = SP for entry stack
GPM_setRSS:
	mov	ax,GDTD_GSEL
	mov	ds,ax			; DS -> GDT
	shl	ecx,4			; CX = low word of base addr
	mov	DS:[RSS_GSEL][2],cx	; set low word of base addr
	shr	ecx,16			; CH = high byte of base addr
	mov	ds:[RSS_GSEL][4],cl	; set high byte of base
	mov	ds:[RSS_GSEL][7],ch	; set high byte of base

	mov	ax,RSS_GSEL
	mov	ss,ax		; set SS and
	mov	sp,bx		; entry SP

;
;...Enable NMIs again
;
GPM_ENMI:
	mov	al,ENABLE_NMI
	out	NMI_CMD,al

;
;  set DS,ES -> RCODE segment
;
	mov	ax,RCODEA_GSEL
	mov	ds,ax
	mov	es,ax
	mov	fs,ax
	mov	gs,ax
	clc

GPMexit:
	pop	ecx
	pop	bx
	pop	eax
	ret

ifdef	MSFLAG
GPM_getcount:
	call	GetA20cnt
	jmp	GPMcont
endif

;
GoProtMode	endp

;******************************************************************************
;	GoRealMode - go to 386 real mode
;
;	The following CPU registers are changed to have values
;	appropriate to real mode operation:
;
;		CS, DS, ES, SS, Flags, MSW, IDT
;
;    ENTRY:	PROTECTED MODE
;		CLI
;		CS = RCODE_GSEL
;		SS - base addr pts to a real mode segment (para boundary)
;		A20 enabled
;
;    EXIT:	REAL MODE
;		CLI
;		CS = R_CODE
;		DS,ES,FS,GS = R_CODE
;		SS = same segment as entry SS
;		A20 disabled
;		high system memory LOCKED
;
;
;    USED:	flags
;
;******************************************************************************
GoRealMode	proc	near
	push	eax
	push	bx
;
;
;   reset TSS busy bit before returning to Real Mode
;
	mov	ax, GDTD_GSEL
	mov	es, ax			; ES:0 = ptr to gdt
	and	byte ptr ES:[TSS_GSEL + 5], 11111101B

;
;	First save return ss:sp. We have to translate
;	the current ss (a selector) into a segment number.
;	Calculate a real mode segment corresponding to the
;	current protected mode stack selector base address.
;
;	We get the base address from the descriptor table,
;	and convert it to a paragraph number.
;
	mov	bx,ss			; bx = selector for stack
	call	SelToSeg		; AX = segment number for SS
	mov	bx,ax			; BX = setup stack segment
;
;
;  Intel shows DS,ES,FS,GS,and SS set up to make sure 'Real Mode' type
;  access rights, and limit are installed.  In this program, that happens
;  to already be the case, but for general purposeness, VDMD_GSEL fits
;  the bill.
;
	mov	ax,VDMD_GSEL		; selector with real mode attributes
	mov	ds,ax
	mov	es,ax
	mov	ss,ax
	mov	fs,ax
	mov	gs,ax

;
;Disable NMIs ...
;
	mov	al,DISABLE_NMI
	out	NMI_CMD,al

;
;    reset the PE bit ...
;
	mov	eax,cr0			;  get CR0
	and	eax,07FFFFFFEh		; force real mode and shut down paging
	mov	cr0,eax			; set CR0

					; flush prefetched instructions with:
	db	0EAh			; Far Jump opcode
	dw	offset R_CODE:rl386_b	; destination offset
	dw	R_CODE			; destination segment
rl386_b:
	lidt	fword ptr cs:[real_idt]

	mov	eax,cr3			; get CR3
	mov	cr3,eax			; set CR3 => clear TLB

	mov	ss,bx			; ss = real mode stack segment
	mov	ax,R_CODE
	mov	ds,ax
	mov	es,ax
	mov	fs,ax
	mov	gs,ax

;
;...Enable NMIs again
;
	mov	al,ENABLE_NMI
	out	NMI_CMD,al

ifdef	MSFLAG
	and	[Current_State],NOT fState_Active
						; MAchine is in real mode; BUGBUG
	call	SetA20cnt			; restore XMM enable count
else
	test	[GenFlags],fShutDown            ;Q: Is this a shutdown?	   910316
	jnz	short GRMexit			; Y: don't bother with A20 910316

	test	[Current_State],fState_A20Ena  	;Q: Is A20 enabled?
	jnz	short GRMexit			; Y: don't disable it

	test	[GenFlags],fXMM			;Q: Need to keep XMS coherent?
	jz	short GRMDisableA20		; N: XMM not being used
	call	EnableA20			; Y: enable count coherency

GRMDisableA20:
	call	DisableA20  			; disable A20 line

GRMexit:

endif
	pop	bx
	pop	eax
	ret
;
GoRealMode	endp

;******************************************************************************
;	GoVirtualMode - go to 386 virtual mode
;
;	The following CPU registers are changed to have values
;	appropriate to real mode operation:
;
;		CS, DS, ES, SS, Flags, MSW, IDT
;
;    ENTRY:	PROTECTED MODE
;		CLI
;		CS = RCODE_GSEL
;		SS - base addr pts to a real mode segment (para boundary)
;		A20 enabled
;
;    EXIT:	VIRTUAL MODE
;		CLI
;		CS = R_CODE
;		DS,ES,FS,GS = R_CODE
;		SS = same segment as entry SS
;		high system memory LOCKED
;
;
;    USED:	flags
;
;******************************************************************************
GoVirtualMode	proc	near
	push	ax
	push	bx
	push	cx
;
; reset NT bit so IRET won't attempt a task switch
;
	pushf
	pop	ax
	and	ax,0FFFh
	push	ax
	popf

;	Calculate a real mode segment corresponding to the
;	current protected mode stack selector base address.
;
;	We get the base address from the descriptor table,
;	and convert it to a paragraph number.
;
	mov	bx,ss			; bx = selector for stack
	call	SelToSeg		; AX = segment number for SS
	mov	bx,ax			; BX = setup stack segment

	mov	cx,sp			; save sp here
;
; build stack frame for IRET into virtual mode
;
	push	0
	push	seg R_CODE		; GS
	push	0
	push	seg R_CODE		; FS
	push	0
	push	seg R_CODE		; DS	(_DATA for variable access)
	push	0
	push	seg R_CODE		; ES

	push	0
	push	bx			;* virtual mode SS

	push	0
	push	cx			;* virtual mode ESP

	push	FLAGS_VM		; EFlags high, VM bit set
	push	3000h			;* EFlags low, NT = 0, IOPL=3, CLI

	push	0
	push	seg R_CODE			; CS
	push	0
	push	offset R_CODE:VM_return 	; IP

	iretd		; 32 bit operand size override

;
;	Enter Virtual Mode here
;
VM_return:
	or	[Current_State],fState_Active; set active flag; BUGBUG

;
; Need to maintain A20 coherency with the XMM
;
	test	[GenFlags],fXMM			;Q: XMM present?
	jz	short VMexit			; N: no coherency problem

	test	[Current_State],fState_A20Ena	;Q: Virtual A20 disabled?
	jnz	short VMexit			; N: virtual & XMM are in sync
	call	DisableA20			; Y: virtually disable A20

VMexit:
	pop	cx
	pop	bx
	pop	ax
	ret
;
GoVirtualMode	endp

ifdef 900722
ifdef MSFLAG

;******************************************************************************
;	Prot_to_Virt - go to 386 virtual mode
;
;	The following CPU registers are changed to have values
;	appropriate to real mode operation:
;
;		CS, DS, ES, SS, Flags, MSW, IDT
;
;    ENTRY:	PROTECTED MODE
;		CLI
;		CS = RCODE_GSEL
;		SS - base addr pts to a real mode segment (para boundary)
;
;    EXIT:	VIRTUAL MODE
;		CLI
;		CS = R_CODE
;		DS,ES,FS,GS = R_CODE
;		SS = same segment as entry SS
;		high system memory LOCKED
;
;
;    USED:	flags
;
;******************************************************************************
Prot_to_Virt	proc	near
	push	ax
	push	bx
	push	cx
;
; reset NT bit so IRET won't attempt a task switch
;
	pushf
	pop	ax
	and	ax,0FFFh
	push	ax
	popf

;	Calculate a real mode segment corresponding to the
;	current protected mode stack selector base address.
;
;	We get the base address from the descriptor table,
;	and convert it to a paragraph number.
;
	mov	bx,ss			; bx = selector for stack
	call	SelToSeg		; AX = segment number for SS
	mov	bx,ax			; BX = setup stack segment

	mov	cx,sp			; save sp here
;
; build stack frame for IRET into virtual mode
;
	push	0
	push	seg R_CODE		; GS
	push	0
	push	seg R_CODE		; FS
	push	0
	push	seg R_CODE		; DS	(_DATA for variable access)
	push	0
	push	seg R_CODE		; ES

	push	0
	push	bx			;* virtual mode SS

	push	0
	push	cx			;* virtual mode ESP

	push	FLAGS_VM		; EFlags high, VM bit set
	push	3000h			;* EFlags low, NT = 0, IOPL=3, CLI

	push	0
	push	seg R_CODE			; CS
	push	0
	push	offset R_CODE:PV_return 	; IP

	iretd		; 32 bit operand size override

;
;	Enter Virtual Mode here
;
PV_return:

	or	[Current_State],fState_Active; set active flag; BUGBUG

	pop	cx
	pop	bx
	pop	ax
	ret
;
Prot_to_Virt	endp


;******************************************************************************
;	Virt_to_Prot - go form 386 virtual mode to protect mode
;
;
;    ENTRY:	VIRTUAL MODE
;		CLI
;		CS = RCODE_GSEL
;		SS - base addr pts to a real mode segment (para boundary)
;		A20 enabled
;
;    EXIT:	PROTECT MODE
;		CLI
;		CS = R_CODE
;		DS,ES,FS,GS = R_CODE
;		SS = same segment as entry SS
;
;    USED:
;
;******************************************************************************


Virt_to_Prot	proc	near

	push	eax
	push	bx
	push	ecx
	push	si


	arpl	ax,ax

	;
	; VM1_GSEL -> client's stack
	; VM2_GSEL -> client's code
	;
;
; setup SS,SP,BP to point to client's stack before going to real mode
;
	mov	si, bp
	push	ss
	pop	ds				; DS:SI -> Ring 0 stack	VMTF
	push	VM1_GSEL
	pop	ss
	mov	sp, [si.VTFOE+VMTF_ESP]		; ES:DI -> client's stack


;
;  set DS,ES,FS,GS -> RCODE segment
;
	mov	ax,RCODEA_GSEL
	mov	ds,ax
	mov	es,ax
	mov	fs,ax
	mov	gs,ax


	pop	si
	pop	ecx
	pop	bx
	pop	eax

	ret

Virt_to_prot	endp

endif
endif

;******************************************************************************
;***	EnableA20 - switch 20th address line
;
;	This routine is used to enable the 20th address line in
;	the system.
;
;	ENTRY	none		;ds = DATA
;	EXIT	A20 line enabled
;	USES	ax, flags modified
;
;******************************************************************************
	assume	ds:nothing
EnableA20 proc near

	mov	ah,0dfh 		; code for enable
	test	cs:[GenFlags],fXMM	;Q: Is XMM installed
	jz	short a20common		; N: jump to common code
	mov	ah,XMM_LOCAL_ENABLE_A20 ; Y: enable function
	jmp	SHORT do_xms_a20

EnableA20 endp

;******************************************************************************
;***	DisableA20 - switch 20th address line
;
;	This routine is used to disable the 20th address line in
;	the system.
;
;	ENTRY	none		;ds = DATA
;	EXIT	A20 line disabled
;	USES	ax, flags modified
;******************************************************************************
DisableA20 proc near

	mov	ah,0ddh 		; code for disable
	test	cs:[GenFlags],fXMM	;Q: Is XMM installed
	jz	short a20common		; N: jump to common code
	mov	ah,XMM_LOCAL_DISABLE_A20; Y: disable function
	jmp	SHORT do_xms_a20

DisableA20 endp

;******************************************************************************
; a20common
;
;	This is entered via a jmp from one of the two procedural
;	entry points above.
;******************************************************************************
a20common proc near
	test	cs:[GenFlags],fP8042	;Q: Password 8042 system?
	jne	short a20j1		; Y: don't need to flush buffer
	call	empty_8042		; ensure 8042 input buffer empty
	jnz	short com1			; 8042 error return
a20j1:	mov	al,0d1h 		; 8042 cmd to write output port
	out	KYBD_CMD,al		; send cmd to 8042
	test	cs:[GenFlags],fP8042	;Q: Password 8042 system?
	jne	short a20j2		; Y: don't need to flush buffer
	call	empty_8042		; wait for 8042 to accept cmd
	jnz	short com1			; 8042 error return
a20j2:	mov	al,ah			; 8042 port data
	out	KYBD_DATA,al		; output port data to 8042
	test	cs:[GenFlags],fP8042	;Q: Password 8042 system?
	jne	short a20j3		; Y: don't need to flush buffer
	call	empty_8042
	jnz	short com1
a20j3:	mov	al,NULL_8042_CMD	; output null cmd
	out	KYBD_CMD,al
	test	cs:[GenFlags],fP8042	;Q: Password 8042 system?
	jne	short com1		; Y: don't need to flush buffer
	call	empty_8042		; and wait for it to finish
com1:
	clc
	ret

do_xms_a20:
	push	dx			;QLEO: HIMEM messes up DX
	call	cs:[XMMcontrol]
	pop	dx			;QLEO: need to fix HIMEM.SYS
	or	ax,ax
	jz	SHORT XMMerror
	clc
	ret
XMMerror:
	stc
	ret
a20common endp

;******************************************************************************
;***	empty_8042 - wait for 8042 input buffer to drain
;
;	ENTRY	none
;	EXIT	al=0, ZF => 8042 input buffer empty
;		al=2, NZ => timeout, input buffer full
;	USES	none
;******************************************************************************
empty_8042 proc near
	push	cx			; save it
	sub	cx,cx			; cx = 0, timeout loop counter
emp1:
	in	al,KYBD_STATUS		; read 8042 status port
	and	al,IN_BUF_FULL		; test buffer full bit
	loopnz	emp1
	pop	cx
	ret
empty_8042 endp

ifdef 900722
;******************************************************************************
;	CheckLOCK - Check current state of Table lock
;
;    ENTRY:	Real Mode
;
;    EXIT:	Real Mode
;		CS:[LockState] = LOCK_ROM if ROM is locked
;			       = UNLOCK_ROM if ROM is unlocked
;
;    USED:	flags
;
;******************************************************************************
CheckLOCK	proc	near
	push	ax
	push	bx
	push	es
;
	mov	cs:[LockState],LOW LOCK_ROM; default is locked
	mov	ax,X_HI_MEM_SEG
	mov	es,ax			; ES -> ROM
	mov	bx,X_MT_386		; ES:BX -> machine type byte
	mov	ax,ES:[bx]		; AX = ROM contents
	xor	ES:[bx],0FFFFh		; flip all bits in ROM
	xor	ax,0FFFFh		; AX = "flipped" value
	cmp	ax,ES:[bx]		;Q: flipped value in ROM ?
	jne	short gv_locked		;   N: ROM is LOCKED
	mov	cs:[LockState],LOW UNLOCK_ROM ;Y: ROM is UNLOCKED
gv_locked:
	xor	ES:[bx],0FFFFh		; restore ROM contents (if changed)
;
	pop	es
	pop	bx
	pop	ax
	ret
CheckLOCK	endp
endif

R_CODE	ENDS

	end
