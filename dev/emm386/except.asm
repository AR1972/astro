.386p
	page 58,132
;=============================================================================
	title	EXCEPT - 80386 Processor Exception Handlers
;=============================================================================
;==
;==  (C) Copyright MICROSOFT Corp. 1986-1991
;==  (C) Copyright COMPAQ Computer Corp. 1986-1991
;==
;==	Title:	MEMM.EXE - MICROSOFT Expanded Memory Manager 386 Driver
;==
;==	Module: EXCEPT - 80386 Processor Exception Handlers
;==
;==	Version: 1.00
;==
;==	Date:	March 11,1990
;==
;==	Author: Leo Cohen
;==
;=============================================================================
;==
;==	Change Log:
;==
;==	DATE	 REVISION	Description
;==	-------- --------	--------------------------------------------
;==     03/11/90 0.00	        Original
;==
;=============================================================================
;==
;==   Functional Description:
;==
;==   This module handles all 80386 processor exceptions.
;==
;==
;=============================================================================
;==	P U B L I C   D E C L A R A T I O N S
;=============================================================================
	public	ExceptHandler0
	public	ExceptHandler1
	public	ExceptHandler2
	public	ExceptHandler3
	public	ExceptHandler4
	public	ExceptHandler5
	public	ExceptHandler6
	public	ExceptHandler7
	public	ExceptHandler8
	public	ExceptHandler9
	public	ExceptHandler10
	public	ExceptHandler11
	public	ExceptHandler12
	public	ExceptHandler13
	public	ExceptHandler14
	public	ExceptHandler15
	public	ExceptHandler16
	public	ExceptHandler17
	public	_PFUser
;=============================================================================
;==	L O C A L   C O N S T A N T S
;=============================================================================
	include vdmseg.inc
	include vdmsel.inc
	include vm386.inc
	include oemdep.inc
	include	emm386.inc
	include	emmfunct.inc
	include	emmdata.inc
	include	page.inc
	include desc.inc
ifdef BETA
	include except.pub
endif

ProcessExcep	macro	ExcepNum
	mov	bx, ExcepNum
	mov	ax, ExcpErr
	PJmp	R1CODE_GSEL, R1_CODE:ErrHndlr
endm

;=============================================================================
;==	E X T E R N A L   R E F E R E N C E S
;=============================================================================
_TEXT	segment
	extrn	ReflectInterrupt:near
	extrn	pIRQHandlerMaster:near
	extrn	pIRQHandler:near
	extrn	VmFault:near
	extrn	MoveBlockParity:near
	extrn	PageFaultHandler:near
	extrn	TrapWrites:near
	extrn	MBMoveW:near
	extrn	MBrepMovD:near
_TEXT	ends

R_CODE	segment
ifdef PICtrap
	extrn	VirMasterPICVec:word
	extrn	MasterPICVec:word
	extrn	LastOCW3:byte
	extrn	MasterIS:word
endif
R_CODE	ends

R1_CODE	segment
	extrn	ErrHndlr:near
R1_CODE	ends

ifdef	BugMode
DCODE	segment
	extrn _Trap00:far		; divide by zero
	extrn _Trap01:far		; single step interrupt
	extrn _Trap02:far		; NMI interrupt
	extrn _Trap03:far		; breakpoint interrupt
	extrn _Trap04:far		; INTO detected overflow
	extrn _Trap05:far		; BOUND range exceeded
	extrn _Trap06:far		; invalid opcode
	extrn _Trap07:far		; processor extension not avaliable
	extrn _Trap08:far		; double exception detected
	extrn _Trap09:far		; processor extension segment overrun
	extrn _Trap10:far		; invalid task state segment
	extrn _Trap11:far		; segment not present
	extrn _Trap12:far		; stack segment overrun or not present
	extrn _Trap13:far		; general proctection fault
	extrn _Trap14:far		; page fault
DCODE	ends
endif
;=============================================================================
;==	L O C A	L   D A T A
;=============================================================================
_DATA	segment

_TABLE_ENTRIES	EQU	1024
_PF	DW	0			; Most recently referenced
_PFAddr	DW	1000h
	DW	2000h
_PFUser	DW	0			; Owner of page fault table 0
   	DW	0			; Owner of page fault table 1
_DATA	ends
;=============================================================================
;==	C O D E  S E G M E N T
;=============================================================================
_TEXT	segment
	assume	cs:_TEXT, ds:NOTHING, es:NOTHING, ss:NOTHING
;==============================================================================
;==
;==  ExceptHandler0: Divide error fault
;==
;==  Entry:  Protected Mode via 386 Interrupt gate
;==	     No error code on stack
;==
;==  Exit:   Reflect to real mode handler if fault occurred in virtual 8086
;==	     mode, else go to debugger.
;==
;==============================================================================
ExceptHandler0:
	push	ebp
	movzx	ebp,sp
	test	[bp][VTFO].VMTF_EFLAGShi,FLAGS_VM;Q: Virtual Mode?
	jz	short EH0debug			 ; N: go to debugger
	push	0000				 ; Y: interrupt 00
	jmp	ReflectInterrupt		 ; reflect it to virtual mode

EH0debug:
ifndef	BugMode
	ProcessExcep ErrDIV
else
	pop	ebp
	db	0EAh			; far jump opcode
	dw	offset DCODE:_trap00	; offset
	dw	DEBC_GSEL		; selector
endif
;==============================================================================
;==
;==  ExceptHandler1: Debug trap
;==
;==  Entry:  Protected Mode via 386 Interrupt gate
;==	     No error code on stack
;==
;==  Exit:   Reflect to real mode handler if fault occurred in virtual 8086
;==	     mode, else go to debugger.
;==
;==============================================================================
ExceptHandler1:
	push	ebp
	movzx	ebp,sp

	call	TrapWrites	;Q: Trap due to write violation?
	jc	short EH1iretd	; Y: return to client

ifndef	 BugMode
	test	[bp][VTFO].VMTF_EFLAGShi,FLAGS_VM;Q: Virtual Mode?
	jz	short EH1debug		 	 ; N: go to debugger
	push	0001				 ; Y: interrupt 01
	jmp	ReflectInterrupt		 ; reflect it to virtual mode

EH1debug:
	ProcessExcep ErrINT1
else
	pop	ebp

	db	0EAh			; far jump opcode
	dw	offset DCODE:_trap01	; offset
	dw	DEBC_GSEL		; selector
endif
EH1iretd:
	pop	ebp
	iretd
;==============================================================================
;==
;==  ExceptHandler2: NMI handler (H/W)
;==
;==  Entry:  Protected Mode via 386 Interrupt gate
;==	     No error code on stack
;==
;==  Exit:   Reflect to real mode handler
;==
;==QLEO:  Need to make this handler reentrant in protected mode!
;==============================================================================
ExceptHandler2:
	push	ebp
	movzx	ebp,sp

ifndef	BugMode
	test	[bp][VTFO].VMTF_EFLAGShi,FLAGS_VM;Q: Virtual Mode?
	jz	short EH2prot			 ; N: check for parity
	push	02				 ; Y: reflect it/debugger
	jmp	ReflectInterrupt		 ; reflect it

EH2prot:
;
;  Check if move block function was executing
;
	cmp	[bp][VTFO].VMTF_CS,VDMC_GSEL	;Q: Executing in _TEXT segment?
	jne	short EH2check			; N: check parity

	cmp	[bp][VTFO].VMTF_EIP,offset _TEXT:MBMoveW	;Q: MB code?
	jb	short EH2check

	cmp	[bp][VTFO].VMTF_EIP,offset _TEXT:MBrepMovD	;Q: MB code?
	ja	short EH2check
	jmp	MoveBlockParity			; Y: fail the move block.

EH2check:
	call	CheckParity		;Q: Was this a true parity error ?
	jc	short EH2parity		; Y: error/debug trap
	pop	ebp 			; N: NMI cleared, continue
	iretd				;    and toss the NMI

EH2parity:
	push	2			; reflect NMI to virtual mode (INT 2)
	push	ebx
	push	esi
	mov	bx,DATA32_GSEL
	push	ds
	mov	si,seg R_CODE
	mov	ds,bx
	movzx	esi,si
	shl	esi,4			; DS:[ESI] point to R_CODE
	jmp	pIRQHandler		; reflect interrupt

EH2MB:


;QLEO	ProcessExcep ErrNMI
else
	pop	ebp
	db	0EAh			; far jump opcode
	dw	offset DCODE:_trap02	; offset
	dw	DEBC_GSEL		; selector
endif
;==============================================================================
;==
;==  ExceptHandler3: Breakpoint trap.  Unfortunately, this breaks the
;==		     debugger's ability to GO and TRACE the VM program.
;==
;==  Entry:  Protected Mode via 386 Interrupt gate
;==	     No error code on stack
;==	     Will never be entered unless BugMode is chosen.
;==
;==  Exit:   Send to debugger
;==
;==============================================================================
ExceptHandler3:
ifndef	BugMode
	push	0
	push	0
	push	ebp
	movzx	ebp,sp
	ProcessExcep ErrINT3
else
	db	0EAh			; far jump opcode
	dw	offset DCODE:_trap03	; offset
	dw	DEBC_GSEL		; selector
endif
;==============================================================================
;==
;==  ExceptHandler4: Overflow trap
;==
;==  Entry:  Protected Mode via 386 Interrupt gate
;==	     No error code on stack
;==
;==  Exit:   Send to debugger
;==
;==============================================================================
ExceptHandler4:
ifndef	BugMode
	push	0
	push	0
	push	ebp
	movzx	ebp,sp
	ProcessExcep ErrINTO
else
	db	0EAh			; far jump opcode
	dw	offset DCODE:_trap04	; offset
	dw	DEBC_GSEL		; selector
endif
;==============================================================================
;==
;==  ExceptHandler5: Array bounds fault
;==
;==  Entry:  Protected Mode via 386 Interrupt gate
;==	     No error code on stack
;==
;==  Exit:   Reflect to real mode handler if fault occurred in virtual 8086
;==	     mode, else go to debugger.
;==
;==============================================================================
ExceptHandler5:
	push	ebp
	movzx	ebp,sp
	test	[bp][VTFO].VMTF_EFLAGShi,FLAGS_VM;Q: Virtual Mode?
	jz	SHORT EH5debug			 ; N: exit to debugger
	push	0005				 ; Y: interrupt 01
	jmp	ReflectInterrupt		 ; reflect it to virtual mode

EH5debug:
ifndef	BugMode
	ProcessExcep ErrBounds
else
	pop	ebp
	db	0EAh			; far jump opcode
	dw	offset DCODE:_trap05	; offset
	dw	DEBC_GSEL		; selector
endif
;==============================================================================
;==
;==  ExceptHandler6: Invalid opcode fault
;==
;==  Entry:  Protected Mode via 386 Interrupt gate
;==	     No error code on stack
;==
;==  Exit:   Reflect to real mode handler if fault occurred in virtual 8086
;==	     mode, else go to debugger.
;==
;==============================================================================
ExceptHandler6:
	push	0			; align stack with error offset
	push	0			;  for VmFault
	push	ebp
	movzx	ebp,sp
	test	[bp][VTFOE].VMTF_EFLAGShi,FLAGS_VM;Q: Virtual Mode?
	jnz	VmFault 			  ; Y: enter VM 06 Invalid handler

EH6debug:
ifndef	BugMode
	ProcessExcep ErrOpCode
else
	pop	ebp
	add	sp,4			; throw away dummy error code
	db	0EAh			; far jump opcode
	dw	offset DCODE:_trap06	; offset
	dw	DEBC_GSEL		; selector
endif
;==============================================================================
;==
;==  ExceptHandler7: Coprocessor not present fault
;==
;==  Entry:  Protected Mode via 386 Interrupt gate
;==	     No error code on stack
;==
;==  Exit:   Reflect to real mode handler if fault occurred in virtual 8086
;==	     mode, else go to debugger.
;==
;==============================================================================
ExceptHandler7:
	push	ebp
	movzx	ebp,sp
	test	[bp][VTFO].VMTF_EFLAGShi,FLAGS_VM;Q: Virtual Mode?
	jz	SHORT EH7debug			 ; N: exit to debugger
	push	0007				 ; Y: interrupt 07
	jmp	ReflectInterrupt		 ; reflect it to virtual mode

EH7debug:
ifndef	BugMode
	ProcessExcep ErrCoPNA
else
	pop	ebp
	db	0EAh			; far jump opcode
	dw	offset DCODE:_trap07	; offset
	dw	DEBC_GSEL		; selector
endif
ALIGN	16
;==============================================================================
;==
;==  ExceptHandler8: Double Fault
;==
;==  Entry:  Protected Mode via 386 Interrupt gate
;==	     Error Code on stack = 0000
;==
;==  Exit:   Send to debugger
;==
;==============================================================================
ExceptHandler8:
	push	ebp			; fix stack for pIRQHandler
	movzx	ebp,sp
	push	8    			; interrupt level
;QLEO jmp CheckInt
	push	ebx
	push	esi
	mov	bx,DATA32_GSEL
	push	ds
	mov	si,seg R_CODE
	mov	ds,bx
	movzx	esi,si
	shl	esi,4			; DS:[ESI] point to R_CODE
	assume	ds:R_CODE

ifdef PICtrap
;QLEO	mov	ebx,[esi][p_DATA]      	; DS:[EBX] point to _DATA
;QLEO	assume	ds:_DATA

	or	ds:[esi][MasterIS],1	; virtualize the master ISR for IRQ0
endif
	jmp	pIRQHandler		; reflect interrupt
	assume	ds:nothing

EH8except:
ifndef	BugMode
	ProcessExcep ErrDouble
else
	pop	ebp
	db	0EAh			; far jump opcode
	dw	offset DCODE:_trap08	; offset
	dw	DEBC_GSEL		; selector
endif
;==============================================================================
;==
;==  ExceptHandler9: Coprocessor segment overrun
;==
;==  Entry:  Protected Mode via 386 Interrupt gate
;==	     No error code on stack
;==
;==  Exit:   Send to debugger
;==
;==============================================================================
ExceptHandler9:
	push	ebp			; fix stack for pIRQHandler
	movzx	ebp,sp
	push	9    			; interrupt level

ifndef LC910611
	push	ebx
	push	esi
	mov	bx,DATA32_GSEL
	push	ds
	mov	si,seg R_CODE

	mov	ds,bx
	movzx	esi,si
	shl	esi,4			; DS:[ESI] point to R_CODE
	assume	ds:R_CODE

	mov	ebx,[esi][p_DATA]	; DS:[EBX] points to _DATA
	assume	ds:_DATA
	mov	[ebx][IRQ1Event],TRUE	; set flag indicating IRQ1 ocurred
	assume	ds:nothing

	jmp	pIRQHandler
else
	jmp CheckInt
endif

EH9except:
ifndef	BugMode
	ProcessExcep ErrCoPseg
else
	pop	ebp
	db	0EAh			; far jump opcode
	dw	offset DCODE:_trap09	; offset
	dw	DEBC_GSEL		; selector
endif
;==============================================================================
;==
;==  ExceptHandler10: Invalid TSS fault
;==
;==  Entry:  Protected Mode via 386 Interrupt gate
;==	     Error Code on stack = Selector
;==
;==  Exit:   Send to debugger
;==
;==============================================================================
ExceptHandler10:
	push	ebp			; fix stack for pIRQHandler
	movzx	ebp,sp
	push	10    			; interrupt level
jmp CheckInt

EH10except:
ifndef	BugMode
	ProcessExcep ErrTSS
else
	pop	ebp
	db	0EAh			; far jump opcode
	dw	offset DCODE:_trap10	; offset
	dw	DEBC_GSEL		; selector
endif
;==============================================================================
;==
;==  ExceptHandler11: Segment not present fault
;==
;==  Entry:  Protected Mode via 386 Interrupt gate
;==	     Error Code on stack = Selector
;==
;==  Exit:   Send to debugger
;==
;==============================================================================
ExceptHandler11:
	push	ebp			; fix stack for pIRQHandler
	movzx	ebp,sp
	push	11    			; interrupt level
jmp CheckInt

EH11except:
ifndef	BugMode
	ProcessExcep ErrSegNP
else
	pop	ebp
	db	0EAh			; far jump opcode
	dw	offset DCODE:_trap11	; offset
	dw	DEBC_GSEL		; selector
endif
;==============================================================================
;==
;==  ExceptHandler12: Stack fault
;==
;==  Entry:  Protected Mode via 386 Interrupt gate
;==	     Error Code on stack = Selector or 0000
;==
;==  Exit:   Send to debugger
;==
;==============================================================================
ExceptHandler12:
	push	ebp			; fix stack for pIRQHandler
	movzx	ebp,sp
	push	12    			; interrupt level
jmp CheckInt

EH12except:
ifndef	BugMode
	ProcessExcep ErrStack
else
	pop	ebp
	db	0EAh			; far jump opcode
	dw	offset DCODE:_trap12	; offset
	dw	DEBC_GSEL		; selector
endif
;==============================================================================
;==
;==  ExceptHandler13: General protection fault
;==
;==  Entry:  Protected Mode via 386 Interrupt gate
;==	     Error Code on stack = Selector or 0000
;==
;==  Exit:   Send to debugger
;==
;==============================================================================
ExceptHandler13:
	push	ebp
	movzx	ebp,sp

ifdef BugMode
	push	ax
	mov	al,1011b
	out	20h,al
	in	al,20h

	test	al,100000b
	pop	ax
	jz	short EH13except
endif
ifdef BETA
	push	ax
	mov	al,1011b
	out	20h,al
	in	al,20h

	test	al,100000b
	pop	ax
	jz	short EH13except
endif

	push	13    			; interrupt level
jmp CheckInt
	add	sp,2

EH13except:
	test	[bp][VTFOE].VMTF_EFLAGShi,FLAGS_VM;Q: Virtual Mode?
	jnz	VmFault				  ; Y: try to handle it!
ifdef BugMode
	pop	ebp
	db	0EAh			; far jump opcode
	dw	offset DCODE:_trap13	; offset
	dw	DEBC_GSEL		; selector
endif
ifdef BETA
	int 1;BETA got here do to a protected mode GP fault
endif
	ProcessExcep ErrGP

;==============================================================================
;==
;==  ExceptHandler14: Page fault
;==
;==  Entry:  Protected Mode via 386 Interrupt gate
;==	     Error Code on stack = type of fault
;==
;==  Exit:   A page table is created to handle faulting address
;==								(PIW)
;==============================================================================
ExceptHandler14:
	push	ebp
	movzx	ebp,sp
	push	14    			; interrupt level

	push	ebx
	push	esi
	mov	bx,DATA32_GSEL
	push	ds
	mov	si,seg R_CODE

	mov	ds,bx
	movzx	esi,si
	shl	esi,4			; DS:[ESI] point to R_CODE
	assume	ds:R_CODE               ; DS:[EBX] point to _DATA

	mov	ebx,cr2
	or	ebx,ebx			;Q: Page Fault Exception?
	jz	pIRQHandler		; N: H/W interrupt

	pop	ds			; clear stack for exception processing
	pop	esi
	pop	ebx
	add	sp,2			; interrupt vector must be cleared

EH14except:
	call	PageFaultHandler	;Q: Manageable page fault?
	jc	short _exception	; N: error
	pop	ebp			; Y: restore regs
	add	sp,4			; throw away error code
	iretd

_exception:
ifndef	BugMode
	ProcessExcep ErrPage
else
	pop	ebp
	db	0EAh			; far jump opcode
	dw	offset DCODE:_trap14	; offset
	dw	DEBC_GSEL		; selector
endif
;==============================================================================
;==
;==  ExceptHandler15: N/A (Dummy handler) Reflect spurious interrupts.
;==
;==  Entry:  N/A
;==
;==  Exit:   N/A
;==
;==============================================================================
ExceptHandler15:
	push	ebp			; fix stack for pIRQHandler
	movzx	ebp,sp
	push	15    			; interrupt level
	push	ebx
	push	esi
	mov	bx,DATA32_GSEL
	push	ds
	mov	si,seg R_CODE
	mov	ds,bx
	movzx	esi,si
	shl	esi,4			; DS:[ESI] point to R_CODE
	jmp	pIRQHandler		; reflect interrupt

EH15except:
	ProcessExcep 15

;==============================================================================
;==
;==  ExceptHandler16: Coprocessor Error
;==
;==  Entry:  Protected Mode via 386 Interrupt gate
;==	     No error code on stack
;==
;==  Exit:   Send to debugger - GOES TO NOT PRESENT FAULT IN DEBUGGER FOR NOW
;==
;==============================================================================
ExceptHandler16:
ifndef	BugMode
	push	0
	push	0
	push	ebp
	movzx	ebp,sp
	ProcessExcep ErrCoPerr
else
	db	0EAh			; far jump opcode
	dw	offset DCODE:_trap07	; offset
	dw	DEBC_GSEL		; selector
endif

;==============================================================================
;==
;==  ExceptHandler17: Alignment Check
;==
;==  Entry:  Protected Mode via 386 Interrupt gate
;==	     Error code on stack = 0
;==
;==  Exit:   Send to debugger - GOES TO NOT PRESENT FAULT IN DEBUGGER FOR NOW
;==
;==============================================================================
ExceptHandler17:
ifndef	BugMode
	push	ebp
	movzx	ebp,sp
	ProcessExcep 17
else
	db	0EAh			; far jump opcode
	dw	offset DCODE:_trap07	; offset
	dw	DEBC_GSEL		; selector
endif

;==============================================================================
;==
;==  CheckInt: Check if entry is due to an exception or a H/W interrupt.
;==
;==  Entry:  Protected Mode
;==	EBP and interrupt number on STACK
;==
;==  Exit:
;==
;==============================================================================
CheckInt:
	push	ebx
	push	esi
	mov	bx,DATA32_GSEL
	push	ds
	mov	si,seg R_CODE
	push	ax

	mov	ds,bx
	movzx	esi,si
	shl	esi,4			; DS:[ESI] point to R_CODE
	assume	ds:R_CODE               ; DS:[EBX] point to _DATA

ifndef PICtrap
	mov	ebx,[esi][p_DATA]	; DS:[EBX] points to _DATA
	assume	ds:_DATA
	mov	ebx,[ebx][TSS].TSS386_ESP0
	sub	bx,size VM_TRAP_FRAME+VTFOE

	cmp	bp,bx			;Q: Exception with error code?
	je	short CIexcept		; Y: it is an exception
	pop	ax			; N: reflect interrupt
	jmp	pIRQHandler

else
	test	[esi][GenFlags],fNoINT	;Q: Interruptible protected mode kernel?
	jnz	short CInoINT		; N: check for an exception via stack

	cmp	[esi][MasterPICVec],DOS_MASTER_VECTOR	;Q: Need to check ISR?
	jne	short CIexcept				; N: it's an exception
;QLEO	mov	ebx,[esi][p_DATA]			; Y: get _DATA pointer
;QLEO	assume	ds:_DATA

	mov	ah,[bp][-2]		; get interrupt number
	mov	al,1011b
	out	20h,al
	in	al,20h			; read ISR from master PIC
;QLEO
	push	ax
	mov	al,[esi][LastOCW3]
	out	20h,al
	pop	ax
;QLEO
	xchg	al,ah
	bt	ax,ax			;Q: ISR bit set?
	jnc	short CIexcept		; N: must be an exception
	sub	ax,8			; Y: get IRQ number
	xor	ah,ah
	bts	ds:[esi][MasterIS],ax	;Q: Is this an exception before the EOI?
	pop	ax
	jnc	pIRQHandler		; N: reflect interrupt
	push	ax
endif

CIexcept:
	movzx	ebx,word ptr [bp][-2]	; interrupt number
	sub	bx,8			; zero relative
	mov	bx,cs:[Exception][ebx*2]
	mov	[bp][-2],bx
	pop	ax
	pop	ds
	pop	esi
	pop	ebx
	ret

ifdef PICtrap
CInoINT:
;
;QLEO Need to detect an exception/interrupt via stack depth!  QLEO
;
	cmp	bp,Stack0Size*4-size VM_TRAP_FRAME-VTFO	;Q: H/W interrupt from VM ?
	jne	short CIexcept			    	; N: it is an exception
	pop	ax				    	; Y: reflect interrupt
	jmp	pIRQHandler
endif

Exception label	word
	dw	offset _TEXT:EH8except
	dw	offset _TEXT:EH9except
	dw	offset _TEXT:EH10except
	dw	offset _TEXT:EH11except
	dw	offset _TEXT:EH12except
	dw	offset _TEXT:EH13except
	dw	offset _TEXT:EH14except
	dw	offset _TEXT:EH15except

;===============================================================================
;==
;==  CheckParity : Check for parity error and clear NMI.
;==		   This routine clears the parity error on the system board.
;==	           If the NMI/parity line is still set, a parity error is
;==	           assumed. The purpose of this routine is to distiguish
;==	           between true parity errors and NMIs from add in boards.
;==
;==  Entry: CLI
;==
;==  Exit: CLC = no parity error
;==	   STC = parity error
;==
;===============================================================================
CheckParity	proc	near
	push	ax

	in	al,PPI		; get parity error flags, reset then set
	jmp	$+2		; parity checking to reset parity on
	jmp	$+2		; system board
;
; Disable IOCHECK & PCHECK
;
	or	al,PPO_MASK_IOCHECK+PPO_MASK_PCHECK
	out	PPO,al		; disable them
	jmp	$+2
	jmp	$+2
	jmp	$+2
;
; Enable IOCHECK & PCHECK
;
	and	al,NOT (PPO_MASK_IOCHECK+PPO_MASK_PCHECK)
	out	PPO,al		; enable them system board parity now reset

	in	al,PPI		; get parity error flags again

	test	al,(PPI_IOCHECK OR PPI_PCHECK)	;Q: any parity errors still?
	jnz	short CPParity			;  Y: return parity
	clc					;  N: wasn't a parity error
CPexit:
	pop	ax
	ret
CPParity:
	stc
	jmp	short CPexit

CheckParity	endp

_TEXT	 ends				 ; end of segment
;
	end				; end of module
