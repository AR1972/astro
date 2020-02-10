.386p
page	58,132
;******************************************************************************
	title	A20TRAP.ASM - I/O trap handlers for watching the A20 line.
;******************************************************************************
;
;   (C) Copyright MICROSOFT Corp. 1986-1991
;   (C) Copyright COMPAQ Computer Corp. 1986-1991
;
;   Title:    EMM386.EXE - MICROSOFT Expanded Memory Manager 386 Driver
;
;   Module:   A20TRAP.ASM - I/O trap handlers for watching the A20 line.
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
;   07/03/86  0.03	From ChkDisA20 routine in VMINST.
;   05/20/87  2.00	Fixed KybdWatch to save BH (SBP).
;   05/31/87  2.00	Added A20 state update (SBP).
;
;   09/26/89		Added P92_Handler for PS2s
;   05/14/90            Hook XMM for A20 management (MS only)
;
;******************************************************************************
;
;   Functional Description:
;	This module contains the I/O trap handlers for the A20 line watching
;   logic.
;
;
;******************************************************************************
.lfcond 				; list false conditionals
	page
;******************************************************************************
;			P U B L I C   D E C L A R A T I O N S
;******************************************************************************
;
	public	P64_Handler
	public	P60_Handler
	public	P92_Handler	; HKN
	public	Kybd_Watch
;
;******************************************************************************
;			I N C L U D E S  &  E Q U A T E S
;******************************************************************************
	include vdmseg.inc
	include vdmsel.inc
	include desc.inc
	include vm386.inc
	include	emm386.inc
	include emmfunct.inc
	include emmdata.inc
	include oemdep.inc

;******************************************************************************
;			E X T E R N A L   R E F E R E N C E S
;******************************************************************************
_TEXT	segment
	extrn	PortTrap:near
	extrn	PortClear:near
	extrn	UpdateHMA:near
ifdef MSFLAG
	extrn	UpdateHMAfar:far
endif
_TEXT	ends

ifdef 900720
ifdef MSFLAG
R_CODE	segment
	extrn	GoVirtualMode:near
	extrn	Virt_to_Prot:near
	extrn	Prot_to_Virt:near

PrevXmm		dd	?	; previous XMM handler's entry point
fCanChangeA20	db	1
EnableCount	dw	0	; A20 enable/disable counter
fGlobalEnable	dw	0	; Global A20 enable/disable call

R_CODE	ends
endif
endif

_DATA	segment
SysPortA	db	0
_DATA	ends

HALT	equ	0F4h	; HLT instruction
ERR_A20	equ	082h

;******************************************************************************
;			S E G M E N T	D E F I N I T I O N
;******************************************************************************

	page
;------------------------------------------------------------------------------
_TEXT	segment
	assume	cs:_TEXT,ds:_DATA,es:_DATA,gs:R_CODE,ss:STACK

;******************************************************************************
;   P60_Handler - I/O trap handler for port 60h - kybd data port
;
;   ENTRY: Protected Mode Ring 0
;		AL = byte to output to port.
;		BX == 2 * port address
;		DX == 0  => Emulate input
;		   <> 0  => Emulate output
;		DS = _DATA
;		GS = R_CODE
;		SS:BP = points to stack frame on entry to GP fault handler
;
;   EXIT:
;		CLC => I/O emulated.
;		STC => I/O NOT emulated.
;
;   USED:  BX,Flags
;   STACK:
;------------------------------------------------------------------------------
P60_Handler	proc	near
;
	or	dx,dx			;Q: Output ?
	jnz	SHORT P60_Dwrite	;  Y: check for write to output port

ifndef LC910611
	test	[GenFlags],fVir8042	;Q: Virtualize keyboard?
	jnz	short P60_VirKB		; Y: read value from virtual port 60h

P60noEmulate:
endif
	stc				; N: don't bother to emulate it
	ret

ifndef LC910611
;
;  Virtual keyboard input data register.  This routine will read the keyboard
;  only once after an IRQ1 has ocurred.  Any subsequent reads, before another
;  IRQ 1, will result in reading a virtual port 60h register (the contents of
;  which will remain the same in between IRQ 1s).
;
P60_VirKB:
	cmp	[p64data],KYBD_RD_OUTPUT;Q: Did client want to read keyboard scan code?
	jne	short P60noEmulate	; N: don't emulate

	cmp	[IRQ1Event],TRUE	;Q: Has an IRQ 1 ocurred since last read?
	je	short P60VreadKB	; Y: read physical 8042
	mov	al,[p60data]		; N: read virtual 8042
	jmp	P60_exit

P60VreadKB:
	mov	[IRQ1Event],FALSE	; mark this IRQ 1
	in	al,KbdDataPort		; read physical 8042
	mov	[p60data],al		; put in virtual port 60h
	jmp	P60_exit
endif

;
;  data write
;
P60_Dwrite:
ifndef LC910611
	cmp	[p64data],KYBD_WR_OUTPUT	;Q: write to output port?
	mov	[p64data],0			; data port write => no CMD

	je	SHORT P60_wr_out		;  Y: filter client's data
	stc					;  N: don't bother to emulate it
	ret
endif
;
;   here if Output Port write
;	Update page table wraparound area to reflect the current state
;	of A20.
;
P60_wr_out:
	push	ax				;
	push	bx
	push	ds
	push	RCODEA_GSEL
	pop	ds					; DS-> R_CODE
	ASSUME	ds:R_CODE

	and	[Current_State],NOT fState_A20Ena	; assume disabled
	test	al, ENABLE_A20			;Q: enabling A20 ?
	jz	SHORT P60_wr_okstate		;  N: state is ok
	or	[Current_State],fState_A20Ena	;  Y: set state to A20 enabled

P60_wr_okstate:
	call	UpdateHMA		; update page table wraparound area

	pop	ds
	ASSUME	ds:_DATA

	or	al, ENABLE_A20			;   to leave A20 enabled
	out	KbdDataPort,al			; "emulate" it

ifndef LC910611
	test	[GenFlags],fVir8042		;Q: Are we virtualizing the 8042?
	jnz	short @f			; Y: don't clear trap
endif
	xor	bx,bx
	mov	ax,KbdDataPort
	call	PortClear
@@:
	pop	bx
	pop	ax				; restore client's byte
P60_exit:
	clc					;  emulated
	ret
P60_Handler	endp

;******************************************************************************
;   P64_Handler - I/O trap handler for port 64h - kybd command port
;
;   ENTRY: Protected Mode Ring 0
;		AL = byte to output to port.
;		BX == 2 * port address
;		DX == 0  => Emulate input
;		   <> 0  => Emulate output
;		DS = _DATA
;		GS = R_CODE
;		SS:BP = points to stack frame on entry to GP fault handler
;
;   EXIT:
;		CLC => I/O emulated.
;		STC => I/O NOT emulated.
;
;   USED:  Flags
;   STACK:
;------------------------------------------------------------------------------
P64_Handler	proc	near
	push	ax
	push	bx

	or	dx,dx			;Q: Output ?
	jz	short P64_exit		;  N: leave
	mov	[p64data],al		;  Y: save new port 64 byte


	cmp	al,0FEh			;Q: Reset CPU?
	jne	short P64chkOutPut	; N: check if it's a output command
;
;  If next instruction is HLT (halt), then halt the CPU
;
	push	si			; save SI
	push	VM2_GSEL
	pop	fs			; FS = selector for VM code segment
	mov	si,[bp][VTFOE].VMTF_EIP	; FS:SI = VM CS:IP
	cmp	byte ptr fs:[si],HALT	;Q: Is reset CPU followed by a HALT?
	pop	si
	jne	short P64_exit		; N: leave
	out	64h,al			; Y: reset CPU and...
	hlt				; HALT the CPU!
;
;  Check if the client is trying an output to keyboard.  If so, need to trap
;  port 60h to determine the value being written (for A20 state).  If not,
;  make sure that port 60h is not being trapped.  This is to prevent a long
;  delay if multiple IRQ 1 handlers are trying to read the scan code.
;
P64chkOutPut:
ifndef LC910611
	test	gs:[GenFlags],fVir8042	;Q: Are we virtualizing the keyboard?
	jz	short @f		; N: continue
	cmp	al,KYBD_RD_ECHO		;Q: Is this a KB echo command?
	jne	short P64_exit		; N: don't clear any trap ports
	mov	[IRQ1Event],TRUE	; Y: assume an IRQ 1 has ocurred
	jmp	short P64_exit		; don't clear any trap ports
@@:
endif
	xor	bx,bx

	cmp	al,KYBD_WR_OUTPUT	;Q: Keyboard output?
	jne	short P64ClearP60	; N: clear port 60h trap
	mov	ax,KbdDataPort		; Y: trap port 60h
	call	PortTrap

ifdef LC910611
	jmp	short P64_exit
endif

P64ClearP60:
ifdef 901108	; Not needed because bit is cleared after P60 trap.  Also,
		; need to speed up access to P64h.
	mov	ax,KbdDataPort
	call	PortClear
endif
P64_exit:
	pop	bx
	pop	ax
	stc				; don't bother to emulate it
	ret
;
P64_Handler	endp

;******************************************************************************
;   P92_Handler - I/O trap handler for port 92h
;
;   ENTRY: Protected Mode Ring 0
;		AL = byte to output to port.
;		BX == 2 * port address
;		DX == 0  => Emulate input
;		   <> 0  => Emulate output
;		DS = _DATA
;		GS = R_CODE
;		SS:BP = points to stack frame on entry to GP fault handler
;
;   EXIT:
;		CLC => I/O emulated.
;		STC => I/O NOT emulated.
;
;   USED:  BX,Flags
;   STACK:
;------------------------------------------------------------------------------

P92_Handler	proc	near

	or	dx,dx			;Q: Output ?
	jnz	SHORT A20_PS2_Write	;  Y: check for write to output port

	mov	al,[SysPortA]		;  N: return shadow

	push	ds
	push	RCODEA_GSEL
	pop	ds
	assume	ds:R_CODE

	and	al,not ENABLE_A20	      ; assume virtual A20 is off
	test	[Current_State],fState_A20Ena ;Q: Is virtual A20 off?
	jz	short P92Rcont		      ; Y: asssumption correct
	or	al,ENABLE_A20	      	      ; N: virtual A20 is on

P92Rcont:
	pop	ds
	assume	ds:_DATA
	clc
	ret

A20_PS2_Write:
	mov	[SysPortA],al		; save it in the shadow

	push	ax
	push	ds
	push	RCODEA_GSEL
	pop	ds			; DS-> R_CODE
	ASSUME	ds:R_CODE

	and	[Current_State],NOT fState_A20Ena	; assume disabled
	test	al,ENABLE_A20			;Q: enabling A20 ?
	jz	SHORT P92_wr_okstate		; N: state is ok
	or	[Current_State],fState_A20Ena	; Y: set state to A20 enabled

P92_wr_okstate:
	call	UpdateHMA		; update page table wraparound area

	pop	ds
	ASSUME	ds:_DATA

	or	al, ENABLE_A20			; to leave A20 enabled
	out	PS2_PORTA,al			; "emulate" it
	pop	ax				; restore client's byte
	clc					;  emulated
	ret

P92_Handler 	endp
;
;******************************************************************************
;   Kybd_Watch - turn on I/O bit map trapping for Kybd A20 line watching
;
;   ENTRY: DS -> _DATA   - real,virtual, or protected mode
;TSSQLEO   ES -> TSS segment
;	   GS = R_CODE
;
;   EXIT: IO_BitMap Updated to trap ports 60h and 64h.
;
;   USED:  AX,Flags
;   STACK:
;------------------------------------------------------------------------------
Kybd_Watch	proc	near
	push	ax
	push	bx

ifndef LC910611
	test	gs:[GenFlags],fNoA20Trap;Q: Trap A20 switching?
	jnz	short KWexit		; N: leave keyboard alone
endif

	xor	bh,bh 		    	; assume EISA/MCA and don't set every 1k

ifdef ROMIDMCA
	cmp	[ROMID],ROMIDISA	;Q: ISA machine?
	jne	short KWPS2		; N: PS2 machine
;LEO	xor	bh,bh 		    	; assume EISA and don't set every 1k
endif

	test	gs:[GenFlags],fEISA+fMCA;Q: EISA or MCA machine?
	jnz	short KWcont		; Y: EISA or MCA
	mov	bh,80h			; N: ISA, reflect every 1K in IO space

KWcont:
;
;  reset flag
;
	mov	[p64data],0
;
;   Set IOBM traps to look for client's disabling of the A20 line
;
ifndef LC910611
	test	gs:[GenFlags],fVir8042	;Q: Trap 8042 data port?
	jz	short @f		; N: leave keyboard alone
	mov	ax, KbdDataPort		; Y: trap data port
	call	PortTrap    		; set traps on keyboard ports
@@:
endif

	mov	ax, KbdCmdPort		    	; in case client
	call	PortTrap    			; set traps on keyboard ports

KWPS2:
	in	al, PS2_PORTA			; get from System Port
	mov	[SysPortA], al			; for PS/2
	mov	ax, PS2_PORTA			; do the same for PS/2 machines
	call	PortTrap

KWexit:
	pop	bx
	pop	ax
	ret

Kybd_Watch	endp

_TEXT	ends				; end of segment

END
