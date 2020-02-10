;******************************************************************************
;
;   (C) Copyright MICROSOFT Corp., 1989-1990
;
;   Title:	VCD.inc -
;
;   Version:	1.00
;
;   Date:	3-May-1989
;
;   Author:	RAP
;
;------------------------------------------------------------------------------
;
;   Change log:
;
;      DATE	REV		    DESCRIPTION
;   ----------- --- -----------------------------------------------------------
;   3-May-1989 RAP
;
;==============================================================================

Begin_Service_Table VCD

VCD_Service	VCD_Get_Version, LOCAL
VCD_Service	VCD_Set_Port_Global, LOCAL
VCD_Service	VCD_Get_Focus, LOCAL
VCD_Service	VCD_Virtualize_Port, LOCAL

End_Service_Table VCD


;***********
;  UART ports and offsets
;
UART_PORTS	equ	7		; # of ports for UART
UART_REG_MASK	equ	7

UART_BAUD_LSB	equ	0		; Least Signif. Byte of BAUD divisor
UART_BAUD_MSB	equ	1		; Most	Signif. Byte of BAUD divisor
UART_RBR	equ	0		; Receive buffer register
UART_THR	equ	0		; Transmit holding register
UART_IER	equ	1		; Interrupt enble register
UART_IIR	equ	2		; Interrupt identity register
UART_LCR	equ	3		; Line control register
UART_MCR	equ	4		; Modem control register
UART_LSR	equ	5		; Line status register
UART_MSR	equ	6		; Modem status register

;***********
;  Interrupt Enable Register Equates
;
IER_MS		equ	08h		; Modem Status
IER_LS		equ	04h		; Line Status
IER_THRE	equ	02h		; Transmit Holding Register Empty
IER_DR		equ	01h		; Data Ready

;***********
;  Interrupt Identity Register Equates
;
IIR_NONE	equ	01h	; No interrupt pending
IIR_LS		equ	06h	; Line Status Interrupt
IIR_DR		equ	04h	; Data Ready Interrupt
IIR_THRE	equ	02h	; Transmit Holding Register Empty
IIR_MS		equ	00h	; Modem Status Interrupt

;***********
;  Line Control Register Equates
;
LCR_DLAB	equ	80h	; Divisor Latch Access

;***********
;  Modem Control Register Equates
;
MCR_INTEN	equ	08h	; Interrupt Enable
MCR_RTS		equ	02h	; Request to Send
MCR_DTR 	equ	01h	; Data Terminal Ready
MCR_Loopback	equ	10h	; Enable Internal Loopback

;***********
;  Line Status Register Equates
;
LSR_THRE	equ	20h	; Transmit Holding Register Empty
LSR_DR		equ	01h	; Data Ready
LSR_OE		equ	02h	; Overrun Error
LSR_PE		equ	04h	; Parity Error
LSR_FE		equ	08h	; Frame Error
LSR_BI		equ	10h	; Break Detected
LSR_ERRBITS	equ	1Eh	; All the error bits
LSR_TXBITS	equ	60h	; THRE and TEMT

;***********
;  Modem Status Register Equates
;
MSR_DELTA	equ	0Fh	; Modem Status DELTA bits
MSR_DCTS	equ	01h	; Delta Clear to Send
MSR_DDSR	equ	02h	; Delta Data Set Ready
MSR_TERI	equ	04h	; Trailing Edge Ring Indicator
MSR_DRLSD	equ	08h	; Delta Receive Line Signal Detect
MSR_CTS 	equ	10h	; Clear to Send
MSR_DSR 	equ	20h	; Data Set Ready
MSR_RING	equ	40h	; Ring Indicator
MSR_RLSD	equ	80h	; Receive Line Signal Detect

;***********
;  Masks for read status flags
;
VCD_Read_LSR_Mask equ   01h     ; Read Line Status Register
VCD_Read_MSR_Mask equ   02h     ; Read Modem Status Register



;***********
;  Flag bits  for VCD_COM_Struc.VCD_Flags
;
VCD_global_port 	equ 00000001b
VCD_global_port_Bit	equ	   0
VCD_IRQ_Init		equ 00000100b	; Set if COM attempted to virt IRQ
VCD_IRQ_Init_Bit	equ	 2
VCD_Owns_IRQ		equ 00001000b	; Set if COM DID virtualize IRQ
VCD_Owns_IRQ_Bit	equ	3
VCD_SetOwner_Event	equ 00100000b	; Set if event scheduled to set owner
VCD_SetOwner_Event_Bit	equ   5
VCD_Windows_Port	equ 01000000b	; Set if Windows has control of this port
VCD_Windows_Port_Bit	equ  6
VCD_Virtualized 	equ 10000000b	; Set if VCD_Virtualize_Port was called
VCD_Virtualized_Bit	equ 7

;***********
;  Flag bits  for VCD_CB_Struc.VCD_CB_Flags
;
VCD_Contended		equ 00000001b	; set if VM contended for port and lost
VCD_Contended_Bit	equ	   0
VCD_CB_Windows_Port	equ 01000000b	; Set if Windows has control of this port
VCD_CB_Windows_Port_Bit equ  6
VCD_Touched		equ 10000000b
VCD_Touched_Bit 	equ 7

.errnz VCD_CB_Windows_Port_Bit - VCD_Windows_Port_Bit

;******************************************************************************
;			  D A T A   S T R U C T U R E S
;******************************************************************************

;***********
;
; Per-VM structure
;
VCD_CB_Struc STRUC
VCD_BAUD_LSB	    db	?	; LSB of virtual BAUD
VCD_BAUD_MSB	    db	?	; MSB of virtual BAUD
VCD_IER 	    db	?	; Virtual Int Enable Reg
VCD_LCR 	    db	?	; Virtual Line Ctrl Reg
VCD_MCR 	    db	?	; Virtual Modem Ctrl Reg
VCD_Read_Stat	    db	?	; Status of LSR & MSR reads
VCD_CB_Flags	    dw	?
		    dd	4 DUP(?); reserved
VCD_CB_Struc ENDS


;***********
;
; Global state data
;
VCD_COM_Struc STRUC

VCD_CB_Offset	dd	?	; Offset in VM control block for per VM data
VCD_Number	db	?	; Either 1 or 2 for COM1 or COM2
VCD_IRQN	db	?	; h/w IRQ number for this device
VCD_Flags	dw	0	; Flags for Mouse, Virtualized, etc.
VCD_IRQ_Desc	dd	?	; Pointer to IRQ descriptor
VCD_Auto_Assign dd	?	; -1 = never auto assign, 0 = always auto assign
				; > 0, auto assign after n msec of no use
VCD_Name	db	8 DUP(?); Pointer to name of COM port
VCD_IObase	dd	?	; Base of IO regs
VCD_Owner	dd	0	; VM handle owning this COM device
VCD_IRQ_Handle	dd	?	; Handle used to talk to VPIC
VCD_Last_Use	dd	?

VCD_COMDEB	dd	?	; linear ptr to Windows COMDEB, if VCD_Windows_Port
VCD_QIn 	dd	?	; linear ptr to input queue, if VCD_Windows_Port
VCD_QOut	dd	?	; linear ptr to output queue, if VCD_Windows_Port

; NOTE:  The bytes BAUD_LSB - Def_MCR MUST BE LEFT IN THIS ORDER.  They are
;        copied into each VMs control block.
VCD_Def_BAUD_LSB    db  ?       ; Default virtual BAUD rate LSB
VCD_Def_BAUD_MSB    db  ?       ; Default virtual BAUD rate MSB
VCD_Def_IER     db      ?       ; Default vitrual Interrupt Enable register
VCD_Def_LCR     db      ?       ; Default virtual Line Control Register
VCD_Def_MCR     db      ?       ; Default virtual Modem Control Register
VCD_Def_LSR     db      ?       ; Default virtual Line Status Register
VCD_Def_MSR     db      ?       ; Default virtual Modem Status Register

VCD_Virt_IIR	db	?	; IIR value read in ring 0

VCD_virt_procs	dd	?

VCD_Hw_Int_Proc 	dd  ?
VCD_Virt_Int_Proc	dd  ?
VCD_EOI_Proc		dd  ?
VCD_Mask_Change_Proc	dd  ?
VCD_IRET_Proc		dd  ?

		dd	4 DUP(?); reserved
VCD_COM_Struc ENDS


VCD_Control_Set_Owner	equ 0


COM_IRQ_Sharable    equ 00000001h


;***********
;
; List of virtualization procs
;
VCD_ProcList_Struc STRUC
; Control proc for handling status change notifications
; proc will be entered with:
;     EAX = message #
;     EBX = VM Handle
;     ESI -> VCD_COM_Struc
VPS_Control_Proc	dd  ?

; Procs for virtualizing the IRQ, 0 = none - use default
; procs will be entered with:
;     EAX = IRQ handle
;     EBX = VM handle
;     ESI -> VCD_COM_Struc
;
VPS_Hw_Int_Proc 	dd  0	; hardware int for IRQ
VPS_Virt_Int_Proc	dd  0	; virtual int for IRQ
VPS_EOI_Proc		dd  0	; EOI for IRQ
VPS_Mask_Change_Proc	dd  0	; mask change for IRQ
VPS_IRET_Proc		dd  0	; virtual iret for IRQ

; Procs for virtualizing I/O while port owned
; procs will be entered with:
;     EBX = VM handle
;     ESI -> VCD_COM_Struc
;     EDX = port #
;     ECX = 0 - Input, 4 - Output
;     AL = data, if output
; procs should return AL = data, if input
VPS_In_RxTxB		dd  ?	; in from receive buffer
VPS_Out_RxTxB		dd  ?	; out to transmit buffer
VPS_In_IER		dd  ?	; in from Interrupt Enable Register
VPS_Out_IER		dd  ?	; out to Interrupt Enable Register
VPS_In_IIR		dd  ?	; in from Interrupt Identification Register
VPS_Out_IIR		dd  ?	; out to Interrupt Identification Register
VPS_In_LCR		dd  ?	; in from Line Control Register
VPS_Out_LCR		dd  ?	; out to Line Control Register
VPS_In_MCR		dd  ?	; in from Modem Control Register
VPS_Out_MCR		dd  ?	; out to Modem Control Register
VPS_In_LSR		dd  ?	; in from Line Status Register
VPS_Out_LSR		dd  ?	; out to Line Status Register
VPS_In_MSR		dd  ?	; in from Modem Status Register
VPS_Out_MSR		dd  ?	; out to Modem Status Register
VCD_ProcList_Struc ENDS


.errnz VCD_Virt_Int_Proc - VCD_Hw_Int_Proc - 4
.errnz VCD_EOI_Proc - VCD_Virt_Int_Proc - 4
.errnz VCD_Mask_Change_Proc - VCD_EOI_Proc - 4
.errnz VCD_IRET_Proc - VCD_Mask_Change_Proc - 4

.errnz VPS_Virt_Int_Proc - VPS_Hw_Int_Proc - 4
.errnz VPS_EOI_Proc - VPS_Virt_Int_Proc - 4
.errnz VPS_Mask_Change_Proc - VPS_EOI_Proc - 4
.errnz VPS_IRET_Proc - VPS_Mask_Change_Proc - 4
