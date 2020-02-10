.386p
page 58,132
;******************************************************************************
	title	EMM - Expanded Memory Manager interface for CEMM
;******************************************************************************
;
;   (C) Copyright MICROSOFT Corp. 1986-1991
;   (C) Copyright COMPAQ Computer Corp. 1986
;
;    Title:	EMM386.EXE - MICROSOFT Expanded Memory Manager 386 Driver
;
;    Module:	EMM - Expanded Memory Manager interface
;
;    Version:	2.05
;
;    Date:	June 14, 1986
;
;    Author:	Steve Preston
;		Leo Cohen (designed and implemented memory management and
;			   mapping functions to improve performance) LC
;
;******************************************************************************
;
;	Change Log:
;
;	DATE	 REVISION	Description
;	-------- --------	--------------------------------------------
;	06/14/86 original
;	06/28/86 0.02		Name change from CEMM386 to CEMM (SBP).
;	06/29/86 0.02		Protect port 84/85 from ints (SBP).
;	07/05/86 0.04		moved EMM_rEntry to R_CODE (SBP).
;	07/06/86 0.04		Changed assume to _DATA (SBP).
;	07/08/86 0.04		Changed EMM_pEntry to call EMM functions
;				directly (SBP).
;	07/10/86 0.05		jmp $+2 before "POPF" (SBP).
;	07/10/86 0.05		added EMM_Flag (SBP).
;	01/12/87 0.09		Added check for AutoMode in EMMr_Link (SBP).
;	05/13/87 2.00		changed errhndlr call (SBP).
;	05/15/87 2.00		changed AUTO mode logic (SBP).
;	05/15/87 2.00		int67 entry called as FAR and ALWAYS from
;				protected mode (SBP).
;	09/05/87 2.05		Fixed AutoUpdate to always update AutoState
;				- even when not in AutoMode (SBP).
;	02/25/88 3.30 (*B)	Fixed rel. jumps because of increased size(RDV)
;	10/12/88 3.32 (*D)	Add VCPI code (DJM).
;
;******************************************************************************
;   Functional Description:
;	The module contains code for calling the EMM functions and a routine
;   for managing the AUTO mode of CEMM.
;	There are two EMM entry points in this module; one for real/virtual
;   mode entry and one for protected mode (IDT entry points here).   When
;   CEMM is Active (system in Virtual mode),  INT 67H calls transition to
;   protected mode and the EMM_pEntry entry point.  EMM_pEntry calls the
;   appropriate EMM function.  All EMM functions are executed in protected
;   mode.  When CEMM is Inactive, CEMM enters protected mode, calls the EMM
;   function, then returns to real mode.
;
;******************************************************************************
.lfcond
	page
;******************************************************************************
;			P U B L I C   D E C L A R A T I O N S
;******************************************************************************
;
	public	EMM_pEntry		; protected mode entry point
	public	EMM_rEntry		; real mode entry point
	public	AlterAndCallEntry
	public	AlterandCallReturn

	page
;******************************************************************************
;			L O C A L   C O N S T A N T S
;******************************************************************************
	include vdmseg.inc
	include vdmsel.inc
	include vm386.inc
	include oemdep.inc
	include desc.inc
	include emm386.inc
	include emmfunct.inc
	include emmdata.inc
	include winemm.inc

ifdef BETA
	include emm.pub
endif
;******************************************************************************
;			E X T E R N A L    R E F E R E N C E S
;******************************************************************************
;

ifdef	BugMode
DCODE	segment
extrn	_Trap03:far
DCODE	ends
 endif

_DATA	segment
	extrn	pLastVMTF:word
	extrn	LastVMTF:word
_DATA	ends

_TEXT	segment
	extrn	EMSDispatcher:far
	extrn	ReflectInterrupt:near
	extrn	EnterVirtual:far
	extrn	ExitVirtual:far
	extrn	VCPIDispatcher:far
_TEXT	ends

R_CODE	segment

	extrn	ChkA20:near
	extrn	GoProtMode:near
	extrn	GoRealMode:near
	extrn	GoVirtualMode:near
	extrn	Devname:byte
;;	extrn	chk_machine_state:far
	extrn	EMM_rFarEntry:dword
R_CODE	ends


page
;******************************************************************************
;			S E G M E N T	D E F I N I T I O N
;******************************************************************************

;******************************************************************************
;
;	_TEXT Code Segment
;
;******************************************************************************
_TEXT	segment
	assume	cs:_TEXT, ds:_DATA, es:ABS0, gs:R_CODE
align 16
;******************************************************************************
;	EMM_pEntry - protected mode entry point for EMM function calls
;
;	ENTRY:	Protected mode
;		[TrapFlags],fI67trap is set => don't reflect interrupt
;		SS:[SP] pointing to virtual mode INT stack frame
;
;	EXIT:	Real or Virtual Mode
;
;	USED:	none
;
;******************************************************************************
EMM_pEntry	proc	near
	push	ebp

;
;  The DS, ES and GS registers are setup for CEMM's data areas and 4 GB.
;
	mov	bp,VDMD_GSEL
	mov	ds,bp
	mov	bp,RCODEA_GSEL
	mov	gs,bp
	mov	bp,DATA32_GSEL
	mov	es,bp
	assume	ds:_DATA,es:ABS0,gs:R_CODE

	mov	bp,gs:[TrapFlags]

	btr	bp,fI67trapBit		;Q: Has real mode ISR been called yes?
	jc	short EpEdispatch	; Y: jump to dispatcher

	cmp	ah,56h			;Q: Alter map and call?
	je	short EpEreflect	; Y: relect interrupt

	or	bp,fI67noReflect	; assume not reflected
	mov	gs:[TrapFlags],bp	; save flags

;
;  Check to see if INT 67 in IVT is hooked
;
	mov	bp,seg R_CODE
	shl	ebp,16
	mov	bp,offset R_CODE:EMM_rEntry
	cmp	ebp,dword ptr es:[int67]	;Q: Is it hooked?
	jne	short EpEreflect		; Y: must reflect
	movzx	ebp,sp				; N: set bp

EpEcont:
        test    [bp][VTFO].VMTF_EFLAGS,FLAGS_IF ;Q: Did client have IF set?
        jz      short EpEints	                ; N: don't turn them on
	sti					; Y: let them go
EpEints:
;
;  The ES,DS registers are pushed on the stack.
;
	push	[bp][VTFO].VMTF_ES
	push	[bp][VTFO].VMTF_DS
;
;  Check to see if its a VCPI or EMS function and call appropriate dispatcher.
;
	cmp	ah,VCPI_FUNCTION_OPCODE
	je	short EpE_VCPI_function

	PCall	VDMC_GSEL,_TEXT:EMSDispatcher

	add	sp,4
	pop	ebp
	iretd

EpEdispatch:
	mov	gs:[TrapFlags],bp	; save flags
	movzx	ebp,sp
	jmp	short EpEcont

EpEreflect:
	movzx	ebp,sp				; N: set bp
	and	gs:[TrapFlags],not fI67noReflect	; reflected
	push	67h
	jmp	ReflectInterrupt


EPE_VCPI_function:
	cli
	PCall	VDMC_GSEL,_TEXT:VCPIDispatcher
	add	sp,4
	pop	ebp
	iretd

ifdef QEMS
	movzx	ebp,sp

;
;  If the real mode entry point has not been called then call it now.  This
;  enables any routines that patch out int 67h to be called.  If the EMM_Flag
;  is 0, then the real mode entry point has NOT been called yet.
;
	test	gs:[TrapFlags],fI67trap
	jnz	short EpE_clear_flag
;
;QLEO: Fast EMS function response
;
	cmp	ah,56h				;Q: Alter map and call?
	je	short EpEreflect		; Y: relect interrupt

	push	eax
	mov	ax,seg R_CODE
	shl	eax,16
	mov	ax,offset R_CODE:EMM_rEntry
	cmp	eax,dword ptr es:[int67]
	pop	eax
	je	short EpEnotReflected

EpEreflect:
	and	gs:[TrapFlags],not fI67noReflect	; reflected
	push	67h
	jmp	ReflectInterrupt

EpEnotReflected:
	or	gs:[TrapFlags],fI67noReflect	; not reflected
EpE_clear_flag:
	and	gs:[TrapFlags],not fI67trap

		; The ES,DS registers are pushed on the stack.
	push	[bp.VTFO+VMTF_ES]
	push	[bp.VTFO+VMTF_DS]

		; Check to see if its a VCPI function or EMS function.
		; Call the appropriate dispatcher.
	cmp	ah,VCPI_FUNCTION_OPCODE
	je	short EpE_VCPI_function

EpE_EMS_function:
        test    gs:[GenFlags],fNoINT+fNoEMSInt  ;Q: Allow interrupts?
        jnz     short EpEDispatch               ; N: don't turn them on
        test    [bp][VTFO].VMTF_EFLAGS,FLAGS_IF ;Q: Did client have IF set?
        jz      short EpEDispatch               ; N: don't turn them on
	sti					; Y: allow interrupts!

EpEDispatch:
	PCall	VDMC_GSEL,_TEXT:EMSDispatcher
;910521	cli

	add	sp,4
	pop	ebp
	iretd
endif
EMM_pEntry	endp

_TEXT	ends

page
;******************************************************************************
;	R_CODE SEGMENT
;******************************************************************************
R_CODE	segment
	assume	cs:R_CODE, ds:R_CODE, es:R_CODE

;******************************************************************************
; EMM_rEntry - real/virtual mode entry point for EMM function calls
;
; ENTRY:
;	real/virtual mode
;
; EXIT:
;	real/virtual mode
;
; DESCRIPTION:
;	If CEMM is off then an appropriate error code is returned in AH.
;	If CEMM is in auto mode and off then the EMS call is done from here
;	and CEMM is returned in either auto off state or ON state.  If CEMM
;	is on this this call simply reflects it back to the protected entry
;	point.
;******************************************************************************
EMM_rEntry	proc	near

	push	gs
	push	cs
	pop	gs	; gs = R_CODE
	call	cs:[EMM_rFarEntry]
	pop	gs
	iret

EMM_rEntry	endp

;******************************************************************************
; AlterAndCallEntry
;
; ENTRY
;	none
; EXIT
;	AL - the special Alter And Call subfunction number
; DESCRIPTION
;	This routine is where a user subroutine returns to during execution
;	of the AlterAndCall function.  This routine sets a special subfunction
;	number and then performs an interrupt.
;******************************************************************************
AlterAndCallEntry	proc	far
	mov	ax,56FFh
	int	67h
AlterandCallReturn:
		; This code should never return through the int 67h to here.
	hlt
AlterAndCallEntry	endp

R_CODE	ends

END
