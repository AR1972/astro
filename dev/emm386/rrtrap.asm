.386p
page	58,132
;******************************************************************************
	title	RRTRAP.ASM - Return To Real Trap
;******************************************************************************
;
;   (C) Copyright MICROSOFT Corp. 1986-1991
;   (C) Copyright COMPAQ Computer Corp. 1986-1991
;
;   Title:    EMM386.EXE - MICROSOFT Expanded Memory Manager 386 Driver
;
;   Module:   RRTRAP.ASM - Return to Real Trap
;
;   Version:  2.00
;
;   Date:     June 1, 1986
;
;   Author:   Steve Preston
;
;******************************************************************************
;
;   Change log:
;
;     DATE    REVISION			DESCRIPTION
;   --------  --------	-------------------------------------------------------
;   06/01/86  Original
;   06/28/86  0.02	Name changed from CEMM386 to CEMM (SBP).
;   07/03/86  0.03	Changed to P84/85 Handlers (SBP).
;   07/06/86  0.04	Moved JumpReal to R_CODE and far label (SBP).
;   07/06/86  0.04	Changed assume to _DATA (SBP).
;   05/18/87  2.00	Movde JumpReal and RR_GoReal to RETREAL.ASM (SBP).
;   06/07/87  2.00	Expaned port 84h/85h trapping to include a general
;			CEMM call interface.
;
;******************************************************************************
;
;   Functional Description:
;	This module traps ports 84h and 85h and watches for specific output
;   sequences.  COMPAQ ROMS output values to port 84h and 85h during
;   power up and other diagnostics.   Port 85h receives a major code indicating
;   the ROM type and possibly the diagnostic mode.  Port 84h receives the minor
;   code indicating the section of diagnostic code executing.  CEMM uses these
;   ports as a function call interface for accessing protected mode functions
;   from virtual mode.  To access the CEMM protected mode functions via this
;   interface, the user program must output a minor code (function class) to
;   port 84h, then output the CEMM major code to port 85h.
;	The following major codes (port 85h) are currently in use.
;		00 -> system ROM
;		01 -> system ROM
;		02 -> CEMM
;		05 -> Merlin ROM
;
;	CEMM uses the following major/minor codes
;	85h = 00		=> ROM code
;		84h = 0F	=> CEMM returns to real mode
;
;	85h = 02		=> CEMM functions
;		84h = 00	=> Weitek functions
;		84h = 01	=> Diagnostic functions
;
;******************************************************************************
.lfcond 				; list false conditionals
	page
;******************************************************************************
;			P U B L I C   D E C L A R A T I O N S
;******************************************************************************
;
	public	P84_Handler
	public	P85_Handler
	public	PMF_RetReal

ifdef	QHKN
	public	Debug_GetPTE
endif
;
;******************************************************************************
;			E X T E R N A L   R E F E R E N C E S
;******************************************************************************
	include VDMseg.inc
	include VDMsel.inc
	include desc.inc
	include VM386.INC
	include	EMM386.INC
	include page.inc
	include	emmfunct.inc
	include	emmdata.inc
;
_TEXT	SEGMENT
	extrn	UpdateHMA:near
	extrn	RestoreIVT:near
	extrn	FlushDMAState:near
_TEXT	ENDS

R_CODE	SEGMENT
	extrn	RR_GoReal:far
ifdef	QHKN
	extrn	Debug_PhysIO:far
endif

R_CODE	ENDS

	page
;******************************************************************************
;			L O C A L   C O N S T A N T S
;******************************************************************************
;
;  On entry of Protected Mode functions, BP points to the saved BP just
;  below the VMTF stack frame after a GP fault.  The following stack
;  frame is used to access items on the entry stack to the P85 handler
;  PMFS_OFF is subtracted from BP to allow use of the PMF_Stack struc.
;
PMF_Stack	struc
PMFS_DX		dw	?		; user DX
PMFS_BX		dw	?		; saved BX
PMFS_ESI 	dw	?		; user ESIlo
		dw	?		; user ESIhi
PMFS_EBX 	dw	?		; user EBXlo
		dw	?		; user EBXhi
PMFS_EBP 	dw	?		; user EBPlo
		dw	?		; user EBPhi
PMFS_ERR	dw	?		; VM Error Code (low)
		dw	?		; VM Error Code (high)
PMFS_EIP	dw	?		; VM EIP (low)
		dw	?		; VM EIP (high)
PMFS_CS		dw	?		; VM CS
		dw	?		;   (padding)
PMFS_EFLAGS	dw	?		; VM EFLAGS (low)
PMFS_EFLAGShi	dw	?		; VM EFLAGS (high)
PMFS_ESP	dw	?		; VM ESP (low)
		dw	?		; VM ESP (high)
PMFS_SS		dw	?		; VM SS
		dw	?		;   (padding)
PMFS_ES		dw	?		; VM ES
		dw	?		;   (padding)
PMFS_DS		dw	?		; VM DS
     		dw	?		;   (padding)
PMFS_FS		dw	?		; VM FS
		dw	?		;   (padding)
PMFS_GS		dw	?		; VM GS
		dw	?		;   (padding)
PMF_Stack	ends

PMFS_OFF	equ	(PMFS_EBP - PMFS_DX)

;
;******************************************************************************
;			S E G M E N T	D E F I N I T I O N
;******************************************************************************

;------------------------------------------------------------------------------
;	_TEXT code
;------------------------------------------------------------------------------
_TEXT	segment
	assume	cs:_TEXT, ds:_DATA, es:_DATA, ss:_DATA

;******************************************************************************
; CEMM protected mode functions dispatch table
;******************************************************************************
CEMM_PMF_Table	label	word
	dw	offset	_TEXT:UpdateHMA		; 84h = 00		;LEO
ifdef	QHKN
	dw	offset	_TEXT:PMF_Diag		; 84h = 01
endif
CEMM_PMF_MAX	equ	($-CEMM_PMF_Table)/2 - 1	; max function code

	page
;******************************************************************************
;   P84_Handler - I/O Trap handler for port 84h
;
;   ENTRY: Protected Mode Ring 0
;		AL = byte to output to port.
;		BX = 2 * port addr
;		DX == 0 => input
;		   <> 0 => output
;		DS = _DATA
;		SS:SP pts to: IP, saved DS, saved DX, IP ,
;			      saved DX,saved BX,saved ESI,saved EBX,saved EBP,
;			then GP fault stack frame with error code.
;		SS:BP = points to stack frame on entry to GP fault handler
;
;   EXIT: Protected Mode Ring 0
;		CLC => I/O emulated.
;		STC => I/O NOT emulated.
;
;   USED:  Flags
;   STACK:
;******************************************************************************
P84_Handler	proc	near
;
	or	dx,dx		;Q: Output ?
;QLEO	jz	SHORT P84_Bye	;  N: then leave
	jz	short P84exit	;  N: then leave
	mov	[RR84save],al	;  Y: save value written to 84
	mov	[RR_Last],84h	; save this RR port #
P84_Bye:
	out	84h,al		; emulate it now - save time later
	clc			; don't bother to emulate it
	ret
P84exit:
	stc
	ret
;
P84_Handler	endp
	page
;******************************************************************************
;   P85_Handler - I/O Trap handler for port 85h
;
;   ENTRY: Protected Mode Ring 0
;		AL = byte to output to port.
;		BX = 2 * port addr
;		DX == 0 => input
;		   <> 0 => output
;		DS = _DATA
;		SS:SP pts to: IP, saved DS, saved DX, IP ,
;			      saved DX,saved BX,saved ESI,saved EBX,saved EBP,
;			then GP fault stack frame with error code.
;		SS:BP = points to stack frame on entry to GP fault handler
;
;   EXIT: If output to 85h => return to Real
;		RRTrap emulates the output to 85h
;		RRTrap returns to real, fixes the segments and stack, and
;		       returns to the instruction past the output in real mode.
;	  If output to 85h  => Weitek functions
;		call Weitek functions handler
;	  else,
;		Protected Mode Ring 0
;		CLC => I/O emulated.
;		STC => I/O NOT emulated.
;
;   USED:  Flags
;   STACK:
;******************************************************************************
P85_Handler	proc	near
;
	or	dx,dx		;Q: Output ?
;QLEO	jz	SHORT P85_Bye	;  N: then leave
	jz	short P85exit	;  N: then leave
	mov	[RR85save],al	;  Y: save value for 85h
	out	85h,al		;     emulate output
	cmp	[RR_Last],84h	;Q: was port 84h last RR port output ?
	jne	SHORT P85_Exit	;  N: save port # and leave
				;  Y: check for valid major function #
	; check for ROM return to real
	;
	cmp	al,RR85_Value		;Q: ROM Return to Real major code ?
	jne	SHORT P85_ChkCEMM	;  N: check for CEMM code
	cmp	[RR84Save],RR84_Value	;  Y: Q: was 84h value RR value ?
	jne	SHORT P85_Exit		;       N: save port # and leave
	jmp	SHORT PMF_RetReal	;       Y: return to real

	;
	; check for CEMM functions
	;
P85_ChkCEMM:
	cmp	al,CEMM_85_Value	;Q: CEMM code ?
	jne	SHORT P85_Exit		;  N: leave
	cmp	[RR84Save],CEMM_PMF_MAX	;  Y: Q: valid CEMM minor code ?
	ja	SHORT P85_Exit		;       N: leave
	xor	bx,bx			;       Y: dispatch function
	mov	bl,[RR84Save]		; BX = minor code
	shl	bx,1			; BX = word offset in table
ifndef	QHKN
	or	bx, bx
	jnz	SHORT P85_PMF_err
endif
	call	CS:CEMM_PMF_Table[bx]	; Q:CEMM PMF handler return OK
	jc	SHORT P85_PMF_err	;   N: set user's CF
					;   Y: reset user's CF
	and	WORD PTR [bp.PMFS_EFLAGS - PMFS_OFF],NOT FLAGS_CF
	jmp	SHORT P85_exit
P85_PMF_err:
	or	WORD PTR [bp.PMFS_EFLAGS - PMFS_OFF],FLAGS_CF	; user STC
;
; all done - save this port as last 84/85h port written
;
P85_Exit:
	mov	[RR_Last],85h	; save this RR port addr
P85_Bye:
	clc			; already emulated
	ret
P85exit:
	stc
	ret
;
P85_Handler	endp

;******************************************************************************
;  PMF_RetReal - port 84/85 return to real functions
;	return processor to real mode and continues the process executing.
;   ENTRY: Protected Mode Ring 0
;		DS = _DATA
;		SS:SP pts to: IP, saved DS, saved DX, IP ,
;			      saved DX,saved BX,saved ESI,saved EBX,saved EBP,
;			then GP fault stack frame with error code.
;		SS:BP = points to stack frame on entry to GP fault handler
;
;   EXIT: Real Mode
;
;******************************************************************************
PMF_RetReal:

	call	FlushDMAState		; write out the virtual dma regs

	mov	dx,DATA32_GSEL		; need access to entire address space
	mov	fs,dx
;
;  If CPU shutdown is detected - restore IVT
;
	test	gs:[GenFlags],fShutDown	;Q: Is the CPU shutting down?
	jz	short PRRcont		; N: continue
	call	RestoreIVT		; Y: restore IVT
PRRcont:
	;
	;   reset registers from the stack
	;
	add	sp,8			; skip IP,DS,DX, and IP
	pop	dx
	pop	bx			; last pushed by OUT emulator
	;
	;   now back to stack presented by VmFault's jmp to instr handler
	;	go to GP fault+pushad stack and then to return to real code
	;
	pop	esi
	pop	ebx
	pushad
	PJmp	RCODE_GSEL,R_CODE:RR_GoReal  ;	  return to real

;LEO ;******************************************************************************
;LEO ;  PMF_Weitek - CEMM Protected Mode Functions for Weitek Coprocessor
;LEO ;   ENTRY: Protected Mode Ring 0
;LEO ;		AL = byte to output to port.
;LEO ;		DS = _DATA
;LEO ;		SS:SP pts to: IP, saved DS, saved DX, IP ,
;LEO ;			      user DX,saved BX,user ESI,user EBX,user EBP,
;LEO ;			then GP fault stack frame with error code.
;LEO ;		SS:BP = points to stack frame on entry to GP fault handler
;LEO ;
;LEO ;		USER BX = 0 => turn Weitek page table mapping on
;LEO ;		        = 1 => turn Weitek page table mapping off
;LEO ;   EXIT:
;LEO ;		Protected Mode Ring 0
;LEO ;
;LEO ;   USED:  Flags
;LEO ;   STACK:
;LEO ;
;LEO ;******************************************************************************
;LEO PMF_Weitek	proc	near
;LEO 	push	bx
;LEO 	push	es
;LEO	mov	bx,word ptr [bp.PMFS_EBX - PMFS_OFF] ; get BX from stk
;LEO	push	PAGET_GSEL
;LEO	pop	es				; ES -> page tables
;LEO	PCall	RCODE_GSEL,R_CODE:WeitekPageMap	; go map it
;LEO	pop	es
;LEO	pop	bx
;LEO	ret
;LEO PMF_Weitek	endp

ifdef	QHKN
;******************************************************************************
;  PMF_Diag - CEMM Protected Mode Functions for Diagnostics/Testing
;   ENTRY: Protected Mode Ring 0
;		DS = _DATA
;		SS:SP pts to: IP, saved DS, saved DX, IP ,
;			      user DX,saved BX,user ESI,user EBX,user EBP,
;			then GP fault stack frame with error code.
;		SS:BP = points to stack frame on entry to GP fault handler
;
;		BX = function #
;		   = 0 => Get Page Table Entry  - see ELIMFUNC.ASM
;		   = 1 => Do physical IO  - see ELIMFUNC.ASM
;   EXIT:
;		Protected Mode Ring 0
;		STC => error - invalid function
;
;   USED:  Flags
;   STACK:
;
;******************************************************************************
PMF_Diag	proc	near
	mov	bx,word ptr [bp.PMFS_EBX - PMFS_OFF] ; get BX from stk
	cmp	bl,0			;Q: GetPTE call ?
	jne	SHORT PMFD_chk1		;  N: check for function 1
					;  Y: get args from stack
	mov	dx,word ptr [bp.PMFS_DX - PMFS_OFF] ; get DX from stk
	call	Debug_GetPTE		; get PTE
	jmp	SHORT PMFD_exit

PMFD_chk1:
	cmp	bl,1				;Q: PhysIO call ?
	jne	SHORT PMFD_err			;  N: report error
						;  Y: get args from stack
	mov	si,word ptr [bp.PMFS_ESI - PMFS_OFF] ; get SI from stk
	mov	dx,word ptr [bp.PMFS_DX - PMFS_OFF] ; get DX from stk
	PCall	RCODE_GSEL,R_CODE:Debug_PhysIO	;     do it
PMFD_exit:
	ret

PMFD_err:
	stc
	ret
PMF_Diag	endp

	page
;******************************************************************************
;	Debug_GetPTE  - Returns the Page Table Entry for a Linear Address
;
;	NOTE: THIS IS A FAR ROUTINE
;
;	ENTRY:	PROTECTED mode
;		ECX = linear address
;		DX = 0  => don't map linear address first (for >1meg addrs)
;		DX = 1  => map linear address first
;
;	EXIT:	Same processor mode as entry
;		STC => error
;		CLC => no errors and
;			ECX = page table entry
;
;	USED: none
;
;******************************************************************************
Debug_GetPTE	proc	near
	push	eax
	push	esi
	push	ds

	mov	ax,PAGET_GSEL
	mov	ds,ax			; DS ->page tables

	or	dx,dx			;Q: map linear addr ?
	jz	SHORT DGPTE_mapped	;  N: don't bother
					;  Y: map it
DGPTE_mapped:

	mov	esi,ecx		; ESI = linear addr
	PDOFF	si		; ESI = page dir off for this addr
	shr	si,2		; ESI = page table # (zero based)
	cmp	si,TOTAL_PAGE_TABLES	;Q: valid CEMM page table  ?
	jae	SHORT DGPTE_err		;  N: report error
	shl	si,12			;  Y: SI = offset to begin of it's table

	PTOFF	cx		; ECX = page table off for this addr
	add	si,cx		; SI -> page table entry

	mov	ecx,[si]		; ECX <- DS:[SI] = page table entry
	clc	; OK ...

DGPTE_exit:
	pop	ds
	pop	esi
	pop	eax
	ret
DGPTE_err:
	stc
	jmp	DGPTE_exit

Debug_GetPTE	endp

endif

_TEXT	ends				; end of segment

	end				; end of module
