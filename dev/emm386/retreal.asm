.386p
page	58,132
;******************************************************************************
	title	RetReal - Return-To-Real routine(s) for the 386
;******************************************************************************
;
;   (C) Copyright MICROSOFT Corp. 1986-1991
;   (C) Copyright COMPAQ Computer Corp. 1986-1991
;
;   Title:    EMM386.EXE - MICROSOFT Expanded Memory Manager 386 Driver
;
;   Module:   RetReal - Return-To-Real routine(s) for the 386
;
;   Version:  2.00
;
;   Date:     February 20, 1986
;
;   Author:   Caldwell Crosswy
;
;******************************************************************************
;
;   Change log:
;
;     DATE    REVISION			DESCRIPTION
;   --------  --------	-------------------------------------------------------
;   02/20/86  Original
;   05/12/86  A-RRH	Cleanup and segment reorganization
;   06/01/86		Removed Real386a (loadall version) and left only
;			RetReal via PE bit (SBP).
;   06/21/86  0.02	Saved Eax (SBP).
;   06/28/86  0.02	Name changed from CEMM386 to CEMM (SBP).
;   07/02/86  0.03	Reset TSS busy bit (SBP).
;   07/05/86  0.04	Added Real_Seg label for _TEXT fixup (SBP).
;   07/06/86  0.04	changed assume to _DATA (SBP).
;   05/13/87  2.00	moved the mode switching code to the routine GoRealMode
;			in MODESW.ASM. Also moved RR_GoReal and JumpReal
;			to here from RR_TRAP.ASM. (SBP)
;
;******************************************************************************
;
;   Functional Description:
;
;	This module contains routines for returning to real mode from
;	protected mode.
;		RR_GoReal - This entry is used by the port 84/85 return to
;				real mode trap.  It exits thru JumpReal and
;				continues execution of the system in real
;				mode.
;		JumpReal - This entry is called to continue the system in
;				real mode.
;		RetReal - Goes from Protected Mode to real mode using the
;				ring 0 stack.
;
;******************************************************************************
.lfcond 				; list false conditionals
	page
;******************************************************************************
;			P U B L I C   D E C L A R A T I O N S
;******************************************************************************
;
	public	RR_GoReal
;LEO	public	JumpReal
	public	RetReal

	page
;******************************************************************************
;			I N C L U D E	F I L E S
;******************************************************************************

include vdmseg.inc
include vdmsel.inc
include	oemdep.inc
include	vm386.inc
include	emm386.inc
include	emmfunct.inc
include emmdata.inc
;
;******************************************************************************
;			E X T E R N A L   R E F E R E N C E S
;******************************************************************************
;

_TEXT	segment
Extrn	ExitVirtual:far
_TEXT	ends

R_CODE	segment
Extrn	Current_State:word
Extrn	GoRealMode:near
R_CODE	ends

;******************************************************************************
;			L O C A L   C O N S T A N T S
;******************************************************************************
;

RR_MASK equ	NOT	(FLAGS_IF+FLAGS_TF)	; mask off IF and TF bits

	page
R_CODE	SEGMENT
	ASSUME	CS:R_CODE, DS:R_CODE, ES:R_CODE, GS:R_CODE
;
;******************************************************************************
;			L O C A L   D A T A   A R E A
;******************************************************************************
;
RR_Jump 	label	dword		; ret addr for instr after out 84
RR_JOff 	dw	0
RR_JSeg 	dw	0

RR_DS		dw	0		; DS for return
RR_SS		dw	0		; SS for return
RR_SP		dw	0		; SP for return

RR_AX		dw	0		; save AX here

RR_Flags	dw	0		; low word of flags for return

	page
;******************************************************************************
;			C O D E
;******************************************************************************

;******************************************************************************
;	RR_GoReal - return client to real after 84/85 trap
;
;    This is the return to real code.  First we return to real mode.
;    Then we set up the stack, restore the registers and return to
;    the instruction following the out to 85h.
;
;		*** JMP TO HERE
;
; ENTRY:	PROTECTED MODE
;		SS:[BP] -> points to saved EBP on GP fault stack frame
;		SS:[SP] -> bottom of pushad
;
;			=> to unwind:
;				popad
;				pop	ebp	; entry BP pts to here
;				add	sp,4	; throw away error code
;				iret
;
; EXIT:		REAL MODE
;		R_CODE:[Current_State] = active bit reset => CEMM inactive
;		continues execution of process specified in GP fault stack
;		frame.
;******************************************************************************
;
RR_GoReal:
	;
	; now leaving Virtual MODE and Deactivating CEMM
	;
	Pcall	VDMC_GSEL, _TEXT:ExitVirtual
	; return to real mode
	call	RetReal
	iret			;LEO
ifdef 900226			;LEO
	; fall into jumpreal code

;******************************************************************************
;
; NAME:	JumpReal - jump into faulting code and continuing executing in Real
;	mode.   When a virtual mode process causes a GP fault, then wishes
;	to continue executing in Real mode afterwards, VDM returns to real
;	mode then calls this routine to "unwind" the stack and continue
;	the process in real mode.
;
;		THIS IS A FAR JUMP ***
;
; ENTRY:	REAL MODE
;		SS:[BP] -> points to saved EBP on GP fault stack frame
;		SS:[SP] -> bottom of pushad
;
;			=> to unwind:
;				popad
;				pop	ebp	; entry BP pts to here
;				add	sp,4	; throw away error code
;				iret
;
; EXIT:		REAL MODE
;		R_CODE:[Current_State] = active bit reset => CEMM inactive
;		continues execution of process specified in GP fault stack
;		frame.
;
;******************************************************************************
JumpReal	label	far
	push	cs
	pop	ds		; set DS= CS = R_CODE

	push	ax
	mov	al,DISABLE_NMI
	out	NMI_CMD,al		; disable NMIs
	pop	ax

	; set state to inactive

	and	[Current_State], NOT fState_Active

					; set up return address
	mov	bx,[bp.VTFOE+VMTF_EIP]	; get return IP
	mov	[RR_JOff],bx		; save it
	mov	bx,[bp.VTFOE+VMTF_CS]	; get return CS
	mov	[RR_JSeg],bx		; save it
;
	mov	bx,[bp.VTFOE+VMTF_EFLAGS]	; get flags
	mov	[RR_Flags],bx			; and save
	and	[bp.VTFOE+VMTF_EFLAGS],RR_MASK	; mask off certain bits
;
	mov	bx,[bp.VTFOE+VMTF_DS]	; get DS
	mov	[RR_DS],bx		; save it

	mov	bx,[bp.VTFOE+VMTF_SS]	; get SS
	mov	[RR_SS],bx		; save it
	mov	bx,[bp.VTFOE+VMTF_ESP]	; get SP
	mov	[RR_SP],bx		; save it
;
; restore regs from stack
;
	popad
	pop	ebp				; now SP -> error code

	add	sp,4+VMTF_ES		; skip error code and GP fault stack
					;   up to ES

	pop	es			; reset ES for return
	add	sp,6			; skip high word of ES segment
					; and DS dword

	pop	fs				; reset FS for return
	add	sp,2			; skip high word of segment

	pop	gs				; reset GS for return
;
;  now set flags, DS, stack, and jump to return.
;
	test	[RR_Flags],FLAGS_IF	;Q: IF bit set in return flags ?
	jz	SHORT RR_CLIexit	;  N: then just return
	push	[RR_Flags]		;  Y: enable interrupts on return
	popf				; set flags
	push	[RR_DS]
	pop	ds			; set DS for exit

	mov	ss,CS:[RR_SS]		; restore SS
	mov	sp,CS:[RR_SP]		; restore SP

	mov	CS:[RR_AX],ax
	mov	al,ENABLE_NMI
	out	NMI_CMD,al		; enable NMIs
	mov	ax,CS:[RR_AX]

	sti				; enable ints
	jmp	CS:[RR_Jump]		;  and return

RR_CLIexit:
	push	[RR_Flags]		; leave interrupts disabled on return
	popf				; set flags
	push	[RR_DS]
	pop	ds			; set DS for exit

	mov	ss,CS:[RR_SS]		; restore SS
	mov	sp,CS:[RR_SP]		; restore SP

	mov	CS:[RR_AX],ax
	mov	al,ENABLE_NMI
	out	NMI_CMD,al		; enable NMIs
	mov	ax,CS:[RR_AX]

	jmp	CS:[RR_Jump]		; far jump for return
;

;******************************************************************************
;***	RetReal - cause a 386 mode switch to real mode
;
;	ENTRY	Ring 0 protected mode
;		CLI - interrupts disabled
;		SS = Ring 0 stack
;
;	EXIT	Real Mode
;		CS = _TEXT
;		DS = R_CODE
;		ES = FS = GS = R_CODE
;		SS = Ring 0 Stack segment
;		A20 disabled
;		high system memory LOCKED
;
;	USES	flags
;
;	DESCRIPTION
;	    This routine switches to real mode and an internal stack.
;	It is usually followed by a call to the routine JumpReal which
;	continues the machine in real mode.
;		NOTE: this routine requires that the Ring0 stack be
;			accessible in Real mode.
;
RetReal	proc near

	call	GoRealMode
	ret				; *** RETURN ***
RetReal	endp

endif	;LEO

;=============================================================================
;==
;==  RetReal:  This routine will switch the processor back into real mode.
;==  	       It translates from the protected mode stack to the user stack
;==	       but only maintains the CS:IP and flags to return to the user
;==	       code.
;==
;==  Entry: (Protected Mode)
;==	    SS:SP = points to IP and a PUSHAD
;==	    SS:BP = points to Virtual Mode Stack Frame
;==	       GS = R_CODE
;==
;==  Exit:  (Real Mode)
;==	    SS:SP = ready for an IRET to return to the user code in real mode.
;==	    General Registers = same as user registers when trapped.
;==	    Segment Registers = same as user registers when trapped.
;==
;=============================================================================
RetReal	proc near
;
;  Deactivate CEMM
;
	and	gs:[Current_State],not fState_Active

;
;  Setup RSS_GSEL in GDT to have same base as user stack
;
	mov	ax,GDTD_GSEL		; ES access to GDT
	mov	es,ax

	movzx	ebx,[bp][VTFOE].VMTF_SS	; get base address of user stack
	shl	ebx,4
	mov	es:[RSS_GSEL][2],bx	; bits 0-15 of base address
	shr	ebx,16
	mov	es:[RSS_GSEL][4],bl	; bits 16-23
	mov	es:[RSS_GSEL][7],bh	; bits 24-31
;
;  Setup user stack with proper return address and CS:IP & flags inorder
;  to return to the user code.
;
	mov	bx,[bp][VTFOE].VMTF_ESP	; get offset for user stack

	mov	ax,RSS_GSEL		; access user stack via DS
	mov	es,ax

	mov	ax,[bp][VTFOE].VMTF_EFLAGS ; get user flags
	sub	bx,2			    ; and place it on the user stack
	mov	es:[bx],ax

	mov	ax,[bp][VTFOE].VMTF_CS	; get code segment
	sub	bx,2			; and place it on the user stack
	mov	es:[bx],ax

	mov	ax,[bp][VTFOE].VMTF_EIP; get instruction pointer
	sub	bx,2			; and place it on the user stack
	mov	es:[bx],ax

	movzx	esp,sp			; access only 64K
	mov	ax,[esp]		; get return address
	sub	bx,2			; and place it on the user stack
	mov	es:[bx],ax
;
;  Save all segment registers for real mode
;
	mov	ax,[bp][VTFOE].VMTF_DS	; get user DS
	mov	gs:[UserDS],ax

	mov	ax,[bp][VTFOE].VMTF_ES	; get user ES
	mov	gs:[UserES],ax

	mov	ax,[bp][VTFOE].VMTF_FS	; get user FS
	mov	gs:[UserFS],ax

	mov	ax,[bp][VTFOE].VMTF_GS	; get user GS
	mov	gs:[UserGS],ax
;
;  Save user stack values
;
	mov	gs:[UserSS],RSS_GSEL
	mov	gs:[UserSP],bx
;
;  Restore general registers
;
	add	sp,2			; throw away return address
	popad				; restore general registers
	pop	ebp			; restore EBP
;
;  Switch to user stack
;
	mov	ss,gs:[UserSS]
	mov	sp,gs:[UserSP]
;
;  Return to real mode
;
	call	GoRealMode
;
;  Restore segment resiters
;
	mov	ds,gs:[UserDS]
	mov	es,gs:[UserES]
	mov	fs,gs:[UserFS]
	mov	gs,gs:[UserGS]

	ret
RetReal	endp

R_CODE	ends
	end
