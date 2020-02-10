.386p
	page 58,132
;=============================================================================
	title	D M A T R A P - traps DMA programming into EMS windows
;=============================================================================
;==
;== (C) Copyright MICROSOFT Corp. 1989-1991
;== (C) Copyright COMPAQ Computer Corp. 1989-1991
;==
;==	Title:	EMM386.EXE - MICROSOFT Expanded Memory Manager 386 Driver
;==
;==	Module: DMATrap  - Traps DMA programming into EMS windows.
;==
;==	Version: 1.00
;==
;==	Date:	August 26,1989
;==
;==	Author: Leo Cohen
;==
;=============================================================================
;==
;==	Change Log:
;==
;==	DATE	 REVISION	Description
;==	-------- --------	--------------------------------------------
;==     08/26/89 0.00	        Original
;==
;==	02/08/91 M006		Initialize the channel 4 data structure for
;==				MCA machines. Also make DMAMask/UnMaskChannel
;==				sensitive to programming of the extended MCA
;==				ports.
;==	
;==	02/08/91 M007		Use the right equate when initializing the 
;==				DMA count info for EISA machines.
;==
;==	02/27/91 M012		Use ah to test for LONG READ/WRITE in int 13
;==				handler
;==
;==	04/19/91 		Added support for > 64K xfer in int 13 
;==				handler. Merged from compaq drop
;==
;=============================================================================
;==
;==   Functional Description:
;==
;==   This module monitors DMA programming while in Virtual 8088 mode.
;==
;==		a) DMA_Operation flag.
;==		   Bit 0: Buffer in use?
;==		   Bit 1: Operation going on?
;==
;==		b) Routine to check terminal count on Controller1 channels.
;==		   Get DMA status for controller 1 and test lower nibble.
;==
;==		c) Routine to check terminal count on Controller2 channels.
;==		   Get DMA status for controller2 and test lower nibble.
;==
;==		d) Routine to do the transfer to user buffer.
;==		   Check to see if the DMA buffer was being used in channel
;==		   flags.  If so read terminal count to determine how many
;==		   bytes were transferred and transfer those many bytes
;==		   to the user buffer.
;==
;==		e) DMA port handlers:
;==
;==			- All should check for TC.  And finish the transfer
;==			  if TC reached.
;==			- A mask operation is definitely transfer complete.
;==			- The base, page and count address register accesses
;==			  are just stored in the linear address and count.
;==			  The physical state of the DMA chip is not updated
;==			  till the unmask operation.
;==			- An unmask operation is the one which starts DMA
;==			  operation.  We need to examine linear address and
;==			  count, check to see if translation is enabled
;==			  or disabled on the channel and if enabled, see if
;==			  the range is contiguous.  If contiguous the DMA
;==			  can go across to the physical address and the
;==			  DMA buffer needn't be requested.  Else we need
;==			  to request the DMA buffer.  If the DMA buffer is
;==			  being used currently we should loop with interrupts
;==			  enabled till the buffer is free.  Note that
;==			  currently we operate with interrupts off. The
;==			  physical address in the DMA virtual state buffer
;==			  is updated and the transfer address and count are
;==			  dumped on the DMA chip and the channel unmasked.
;==			- The mode register access is processed to disallow
;==			  certain modes like autoinitialize if the DMA buffer
;==			  is needed.	.
;==			- The other ports are used just for TC checking.
;==
;==	  DMA ports to which must be trapped (ISA):  (CH 4 - refresh)
;==
;==			DMA1 	DMA2
;==	  Address0      000h    0C0h
;==	  Count0      	001h    0C2h
;==	  Page0         087h    ----
;==	  Address1      002h    0C4h
;==	  Count1      	003h    0C6h
;==	  Page1         083h    08Bh
;==	  Address2      004h    0C8h
;==	  Count2      	005h    0CAh
;==	  Page2         081h    089h
;==	  Address3      006h    0CCh
;==	  Count3      	007h    0CEh
;==	  Page3         082h    08Ah
;==	  Status        008h    0D0h
;==	  Softreq       009h    0D2h
;==	  Single Mask   00Ah    0D4h
;==	  Mode          00Bh    0D6h
;==	  FlipFlop      00Ch    0D8h
;==	  Reset         00Dh    0DAh
;==	  Reset Mask    00Eh    0DCh
;==	  Mask	   	00Fh	0DEh
;==
;==	  In addition for EISA:
;==
;==	  HiBase/Count0 401h    ----
;==	  HiPage0       487h    ----
;==	  HiBase/Count1 403h    4C6h
;==	  HiPage1       483h    48Bh
;==	  HiBase/Count2 405h    4CAh
;==	  HiPage2       481h    489h
;==	  HiBase/Count3 407h    4CEh
;==	  HiPage3       482h    48Ah
;==	  ChainMode	40Ah    4D4h
;==	  ExtendedMode  40Bh    4D6h
;==	  Stop0<7:2>    4E0h    ----
;==	  Stop0<15:8>   4E1h    ----
;==	  Stop0<23:16>  4E2h    ----
;==	  Stop1<7:2>    4E4h    4F4h
;==	  Stop1<15:8>   4E5h    4F5h
;==	  Stop1<23:16>  4E6h    4F6h
;==	  Stop2<7:2>    4E8h    4F8h
;==	  Stop2<15:8>   4E9h    4F9h
;==	  Stop2<23:16>  4EAh    4FAh
;==	  Stop3<7:2>    4ECh    4FCh
;==	  Stop3<15:8>   4EDh    4FDh
;==	  Stop3<23:16>  4EEh    4FEh
;==
;==
;==	  ChainBuffExp	40Ch         ** For DMA1 & DMA2
;==
;==
;==	  In addition for MCA:
;==
;==	  XFN
;==	  EXE
;==
;=============================================================================
;==	P U B L I C   D E C L A R A T I O N S
;=============================================================================
	public	DMAInit
	public	InitDMA
	public	DMARegSav
	public	DMABase0
	public	DMABase1
	public	DMABase2
	public	DMABase3
	public	DMABase5
	public	DMABase6
	public	DMABase7
	public	DMACnt0
	public	DMACnt1
	public	DMACnt2
	public	DMACnt3
	public	DMACnt5
	public	DMACnt6
	public	DMACnt7
	public	DMAPg0
	public	DMAPg1
	public	DMAPg2
	public	DMAPg3
	public	DMAPg5
	public	DMAPg6
	public	DMAPg7
	public	DMAStat1
	public	DMARequest1
	public	DMARequest2
	public	DMASinMsk1
	public	DMAMode1
	public	DMAClrFF1
	public	DMAReset1
	public	DMAResMsk1
	public	DMAMask1
	public	DMAStat2
	public	DMASinMsk2
	public	DMAMode2
	public	DMAClrFF2
	public	DMAReset2
	public	DMAResMsk2
	public	DMAMask2
	public	DMACheckTC
	public	DMATrapExit
	public	DMABeginChannel
	public	MoveBuffer
	public	SectorsPerTrack
	public	BytesPerSector
	public	ContigCheck
	public	pINT13hHandler
	public	rINT13hHandler

	public	FlushDMAState

;=============================================================================
;==	L O C A L   C O N S T A N T S
;=============================================================================
	include allocmem.inc
	include vdmseg.inc
	include vdmsel.inc
	include dma.inc
	include	page.inc
	include vm386.inc
	include	emm386.inc
	include	emmdata.inc
	include i13.inc

;=============================================================================
;==	E X T E R N A L   R E F E R E N C E S
;=============================================================================

_TEXT	segment
	extrn	PortTrapFar:far
	extrn	ReflectInterrupt:near
	extrn	CheckPageProt:near
_TEXT	ends

R_CODE	segment
	extrn	EMM_rFarEntry:word
R_CODE	ends

R1_CODE	segment
	extrn	rInt13HEntry:dword
	extrn	I13SectTrans:byte
	extrn	I13DriveTrap:dword
	extrn	SectorsInDMABuffer:byte
	extrn	LongSectorsInDMABuffer:byte
	extrn	ErrHndlr:far
R1_CODE	ends


LAST	segment
	extrn	EndDriver:word
LAST	ends

;=============================================================================
;==	L O C A	L   D A T A
;=============================================================================
_DATA	segment
DMARegSav   DMARegBuf	<>		; DMA Register buffer
ReturnAL    		db  	?	; Temp storage for client's AL

FormatBufferSize	dw	0	; Largest buffer needed for format

I13Operation		db	0	; Type of operation
I13DriveNumber		db	0	; Drive number for this operation

I13DriveIndex		db	0	; Outstanding DMA drive index
I13CheckDMA		dd	-1	; DMA drive check index
I13BuffAddress		dd	0	; Outstanding DMA buffer address

I13OrigNumSect		db	0	; Number of sectors to transfer
I13OrigStartSect	db	0	; Starting sector for transfer
I13OrigStartHead	db	0	; Starting head for transfer
I13OrigStartCyl		dw	0	; Starting cylinder for transfer
I13OrigBuffStart	dd	0	; Starting buffer offset
;*128KDMA I13OrigBuffOff		dw	0	; Starting buffer offset

I13CurrNumSect		db	0	; Number of sectors to transfer
I13CurrStartSect	db	0	; Starting sector for transfer
I13CurrStartHead	db	0	; Starting head for transfer
I13CurrStartCyl		dw	0	; Starting cylinder for transfer
I13CurrBuffStart	dd	0	; Starting buffer offset
;*128KDMA I13CurrBuffOff		dw	0	; Starting buffer offset

HeadsPerCylinder	db	0	; Heads per cylinder (current)
	db	TotalDrives	dup (0)

SectorsPerTrack		db	0	; Sectors per track (current)
	db	TotalDrives	dup (0)

BytesPerSector		dw	0	; Bytes per sector (current)
	dw	TotalDrives     dup (0)

_DATA	ends


;=============================================================================
;==	C O D E  S E G M E N T
;=============================================================================
_TEXT	segment
	assume	cs:_TEXT,ds:_DATA,es:_DATA,gs:R_CODE,ss:STACK
;==============================================================================
;==
;==  DMAStat(1-2) - Read/Write DMA Controller Status/Command Registers
;==		    Check to see if a DMA operation has completed.
;==		    Emulate reads of status register and allow programming
;==		    of command register.  Memory-To-Memory command is not
;==		    allowed.
;==
;==  Entry: (Protected Mode Ring 0)
;==	AL = byte to output to port.
;==	DS = _DATA
;==	GS = R_CODE
;==	BX = port address
;==	DX = 0 => Emulate Input (Status Register).
;==	   <>0 => Emulate Output (Command Register).
;==
;==  Exit:  (Protected Mode Ring 0)
;==	AL  =  input or output value to port.
;==	CLC => I/O emulated by DMATrap.
;==	STC => I/O NOT emulated by DMATrap.
;==
;== Status Register: (READ) 8237A Programmable DMA Controller
;==
;== 76543210
;== xxxxxxxxÄÄÄ> Channel 0 has reached TC
;== ³³³³³³ÀÄÄÄÄ> Channel 1 has reached TC
;== ³³³³³ÀÄÄÄÄÄ> Channel 2 has reached TC
;== ³³³³ÀÄÄÄÄÄÄ> Channel 3 has reached TC
;== ³³³ÀÄÄÄÄÄÄÄ> Channel 0 request
;== ³³ÀÄÄÄÄÄÄÄÄ> Channel 1 request
;== ³ÀÄÄÄÄÄÄÄÄÄ> Channel 2 request
;== ÀÄÄÄÄÄÄÄÄÄÄ> Channel 3 request
;==
;== Command Register: (WRITE) 8237A Programmable DMA Controller
;==
;== 76543210
;== xxxxxxxxÄÄÄ> Memory-to-Memory enable
;== ³³³³³³ÀÄÄÄÄ> Channel 0 address hold enable (don't care if bit 0 is reset)
;== ³³³³³ÀÄÄÄÄÄ> Controller Disable
;== ³³³³ÀÄÄÄÄÄÄ> Compressed Timing (don't care if bit 0 is set)
;== ³³³ÀÄÄÄÄÄÄÄ> Rotating Priority
;== ³³ÀÄÄÄÄÄÄÄÄ> Extended Write Selection (don't care if bit 3 is set)
;== ³ÀÄÄÄÄÄÄÄÄÄ> DREQ Sense Active Low
;== ÀÄÄÄÄÄÄÄÄÄÄ> DACK Sense Active High
;==
;==============================================================================
fMemoryToMemory	equ 00000001b		; memory-to-memory transfer
fDMADisabled	equ 00000100b		; controller disabled
fIllegalCommand equ fMemoryToMemory+fDMADisabled

DMAStat1:
	push	si
	xor	si,si			; index into controller 1
	jmp	short DMAStat

DMAStat2:
	push	si
	mov	si,1			; index into controller 2

DMAStat:
	call	DMACheckTC		; update virtualized status registers

	or	dx,dx			;Q: Emulate input?
	jnz	short DMACommand	; N: write to command register port
	mov	al,[DMARegSav][si].DMAStatus ; Y:get virtualized status register
	mov	[DMARegSav][si].DMAStatus,0  ;   and reset it
	clc
	pop	si
	ret

DMACommand:
;
;  **** Controller disable command is currently NOT supported ****
;
;  If a disable is detected, unmasks will not begin a transfer until the
;  controller is enabled.
;
	and	[DMARegSav][si].DMACtlFlgs,not fDisabled ; assume enabled
	test	al,fDMADisabled		;Q: Disable controller?
	jz	short DSCcont		; N: controller enabled
	or	[DMARegSav][si].DMACtlFlgs,fDisabled ; Y: disable controller

DSCcont:
	pop	si
	test	al,fIllegalCommand	;Q: Memory-To-Memory/disabled illegal
	jnz	SHORT DSCerror		; Y: error, can't handle
	stc				; N: program the 8237
	ret

DSCerror:
	mov	ax,DMAModeErr
	mov	bx,0
	PJmp	R1CODE_GSEL,R1_CODE:ErrHndlr

;==============================================================================
;==
;==  DMARequest - (WRITE) DMA Request register.
;==		  Check to see if a DMA operation has completed.  Program the
;==		  8237 if its a request.  Complete current transfer if request
;==		  is being reset.  Allow request to go through.  ** This
;==		  routine is not emulating H/W under all situations.  If
;==		  request is reset, it must read current registers from the
;==		  8237, and use them if a S/W request follows.  Currently,
;==		  this sequence will cause the 8237 to be reprogrammed with
;==		  initial values. **
;==
;==  Entry: (Protected Mode Ring 0)
;==	AL = byte to output to port.
;==	DS = _DATA
;==	GS = R_CODE
;==	BX = port address
;==	DX = 0 => Emulate Input
;==	   <>0 => Emulate Output
;==
;==  Exit:  (Protected Mode Ring 0)
;==	AL  =  input or output value to port.
;==	CLC => I/O emulated by DMATrap.
;==	STC => I/O NOT emulated by DMATrap.
;==
;== Request Register: (WRITE) 8237A Programmable DMA Controller
;==
;== 76543210
;== xxxxxxxx
;== ³³³³³³ÀÁÄÄÄ> Channel 0-3 selected
;== ³³³³³ÀÄÄÄÄÄ> Set Request Bit
;== ÀÁÁÁÁÄÄÄÄÄÄ> don't care
;==
;==============================================================================
DMARequest1:
	push	si
	lea	si,[DMARegSav].Chnl0	; channel 0 on controller 1
	jmp	short DMARequest

DMARequest2:
	push	si
	lea	si,[DMARegSav].Chnl4	; channel 0 on controller 1

DMARequest:
	call	DMACheckTC
	push	cx

	or	dx,dx			;Q: Emulate output?
	jz	short DRqNoEm	 	; N: no emulation

	mov	cl,al			; get channel being used
	and	cx,00000011b            ;Q: Channel 0?
	jcxz	DRqChannel		; Y: have channel 0 context
DRqFindChannel:
	add	si,size DMARegRec	; next channel
	loop	DRqFindChannel		;Q: Channel context found? (if not, loop)
					; Y: channel context at [SI]
DRqChannel:
	test	al,00000100b		;Q: Requesting the channel?
	jnz	short DRqRequest	; Y: start transfer

	call	DMACompleteChannel	; check for completion
	jmp	short DRqNoEm	 	; no emulation

DRqRequest:
	test	[si].DMAChnFlgs,fChnlActive ;Q: Is channel already active?
	jnz	short DRqNoEm		    ; Y: no need to initialize again
	call	DMABeginChannel		    ; N: program 8237

DRqNoEm:
	stc
	pop	cx
	pop	si
	ret
;==============================================================================
;==
;==  DMASinMsk(1-2) - (WRITE) DMA Single Mask register.
;==		      Check to see if a DMA operation has completed.
;==		      If a mask to a channel is detected, check for DMA buffer
;==		      freeing up and update virtual data for channel. If an
;==		      unmask is detected, program 8237 for transfer to begin.
;==		      If DMA buffer is needed and is currently being used, it
;==		      will loop with interrupts on until it is free.
;==
;==
;==  Entry: (Protected Mode Ring 0)
;==	AL = byte to output to port.
;==	DS = _DATA
;==	GS = R_CODE
;==	BX = port address
;==	DX = 0 => Emulate Input
;==	   <>0 => Emulate Output
;==
;==  Exit:  (Protected Mode Ring 0)
;==	AL  =  input or output value to port.
;==	CLC => I/O emulated by DMATrap.
;==	STC => I/O NOT emulated by DMATrap.
;==
;==
;== Single Mask Register: (WRITE) 8237A Programmable DMA Controller
;==
;== 76543210
;== xxxxxxxx
;== ³³³³³³ÀÁÄÄÄ> Channel 0-3 selected
;== ³³³³³ÀÄÄÄÄÄ> Set Mask Bit
;== ÀÁÁÁÁÄÄÄÄÄÄ> don't care
;==
;==============================================================================
DMASinMsk1:
	push	si
	lea	si,[DMARegSav].Chnl0	; channel 0 on controller 1
	jmp	short DMASinMsk

DMASinMsk2:
	push	si
	lea	si,[DMARegSav].Chnl4	; channel 0 on controller 2

DMASinMsk:
	call	DMACheckTC

	or	dx,dx			;Q: Emulate output?
	jz	short DSMNoEm	 	; N: no emulation
	push	cx			; Y: save

	mov	cl,al			; get channel being used
	and	cx,00000011b            ;Q: Channel 0?
	jcxz	DSMChannel		; Y: have channel 0 context
DSMFindChannel:
	add	si,size DMARegRec	; next channel
	loop	DSMFindChannel		;Q: Channel context found? (if not, loop)
					; Y: channel context at [SI]
DSMChannel:
	test	al,00000100b		;Q: Unmasking a channel?
	jnz	short DSMMask		; N: masking it, check for completion

	call	DMABeginChannel		; unmasking channel, must program 8237
	call	DMAUnMaskChannel	; unmask channel to start transfer
	jmp	short DSMExit

DSMMask:
	call	DMAMaskChannel		; mask channel to terminate DMA
	call	DMACompleteChannel	; masking channel, check for completion

DSMexit:
	clc
	pop	cx
	pop	si
	ret

DSMNoEm:
	stc
	pop	si
	ret
;==============================================================================
;==
;==  DMAMode(1-2) - (WRITE) DMA Mode register.
;==		    Check to see if a DMA operation has completed.
;==		    Mark DMAChnFlags if it is a read or a write operation.
;==		    Do not allow autoinitialize or decrement mode (note: I could
;==		    not think of a way to virtualize while in autoinitialize
;==		    mode, but decrement mode may be supported if needed).
;==
;==  Entry: (Protected Mode Ring 0)
;==	AL = byte to output to port.
;==	DS = _DATA
;==	GS = R_CODE
;==	BX = port address
;==	DX = 0 => Emulate Input
;==	   <>0 => Emulate Output
;==
;==  Exit:  (Protected Mode Ring 0)
;==	AL  =  input or output value to port.
;==	CLC => I/O emulated by DMATrap.
;==	STC => I/O NOT emulated by DMATrap.
;==
;==
;== Mode Register: (WRITE) 8237A Programmable DMA Controller
;==
;== 76543210
;== xxxxxxxx
;== ³³³³³³ÀÁÄÄÄ> Channel 0-3 selected
;== ³³³³ÀÁÄÄÄÄÄ> 0-Verify, 1-Write, 2- Read, 3-illegal, X- if bits 6 & 7 are set
;== ³³³ÀÄÄÄÄÄÄÄ> Enable Autoinitaialize
;== ³³ÀÄÄÄÄÄÄÄÄ> Address Decrement
;== ÀÁÄÄÄÄÄÄÄÄÄ> 0-Demand Mode, 1-Single Mode, 2-Block Mode, 3-Cascade Mode
;==
;==============================================================================
fWriteMode	equ 00000100b
fReadMode	equ 00001000b
fAutoInitMode	equ 00010000b
fDecrementMode	equ 00100000b
fCascadeMode	equ 11000000b

DMAMode1:
	push	si
	lea	si,[DMARegSav].Chnl0	; channel 0 on controller 1
	jmp	short DMAMode

DMAMode2:
	push	si
	lea	si,[DMARegSav].Chnl4	; channel 0 on controller 2

DMAMode:
	call	DMACheckTC

	or	dx,dx			;Q: Emulate output?
	jz	short DMoNoEm	 	; N: no emulation
	push	cx			; Y: save

	mov	cl,al			; get channel being used
	and	cx,00000011b            ;Q: Channel 0?
	jcxz	DMoChannel		; Y: have channel 0 context
DMoFindChannel:
	add	si,size DMARegRec	; next channel
	loop	DMoFindChannel		;Q: Channel found? (if not, loop)
					; Y: channel context at [SI]
DMoChannel:
	pop	cx			; restore
;
; Channel is assumed to be programmed for: read, non decrement, non auto initialize
;
	or	[si].DMAChnFlgs,fReadOp	; assume read mode
	and	[si].DMAChnFlgs,not (fWriteOp+fDecMode+fAutoInit+fCascadeOp)

	push	ax
	and	al, fCascadeMode	;Q: Cascade mode?
	cmp	al, fCascadeMode	;   (both bits must be set)
	pop	ax
	jne	short DMoMode0
	or	[si].DMAChnFlgs, fCascadeOp

DMoMode0:
	test	al,fDecrementMode	;Q: Decrement mode requested?
	jz	short DMoMode1	  	; N: assumption correct
	or	[si].DMAChnFlgs,fDecMode; Y: decrement mode

DMoMode1:
	test	al,fReadMode		    ;Q: Read mode for channel?
	jnz	short DMoMode2	       	    ; Y: assumption correct
	and	[si].DMAChnFlgs,not fReadOp ; N: assume verify mode

	test	al,fWriteMode		;Q: Write mode for channel?
	jz	short DMoMode2		; N: verify mode
	or	[si].DMAChnFlgs,fWriteOp; Y: write mode

DMoMode2:
	test	al,fAutoInitMode	  ;Q: Autoinitialize mode?
	jz	short DMoNoEm		  ; N: assumption correct
	or	[si].DMAChnFlgs,fAutoInit ; Y: auto initialize mode

DMoNoEm:
	stc
	pop	si
	ret

;==============================================================================
;==
;==  DMAMask(1-2) - (WRITE) DMA Mask register.
;==		    Check to see if a DMA operation has completed.
;==		    If a mask to a channel is detected, check for DMA buffer
;==		    freeing up and update virtual data for channel. If an
;==		    unmask is detected, program 8237 for transfer to begin.
;==		    If DMA buffer is needed and is currently being used, it
;==		    will loop with interrupts on until it is free.
;==
;==  Entry: (Protected Mode Ring 0)
;==	AL = byte to output to port.
;==	DS = _DATA
;==	GS = R_CODE
;==	BX = port address
;==	DX = 0 => Emulate Input
;==	   <>0 => Emulate Output
;==
;==  Exit:  (Protected Mode Ring 0)
;==	AL  =  input or output value to port.
;==	CLC => I/O emulated by DMATrap.
;==	STC => I/O NOT emulated by DMATrap.
;==
;==
;== Mask Register: (WRITE) 8237A Programmable DMA Controller
;==
;== 76543210
;== xxxxxxxx
;== ³³³³³³ÀÁÄÄÄ> Mask Channel 0
;== ³³³³³ÀÄÄÄÄÄ> Mask Channel 1
;== ³³³³ÀÄÄÄÄÄÄ> Mask Channel 2
;== ³³³ÀÄÄÄÄÄÄÄ> Mask Channel 3
;== ÀÁÁÄÄÄÄÄÄÄÄ> don't care
;==
;==============================================================================
DMAMask1:
	push	si
	lea	si,[DMARegSav].Chnl0	; channel 0 on controller 1
	jmp	short DMAMask

DMAMask2:
	push	si
	lea	si,[DMARegSav].Chnl4	; channel 0 on controller 2

DMAMask:
	call	DMACheckTC

	or	dx,dx			;Q: Emulate output?
	jz	short DMNoEm	 	; N: no emulation
	push	cx			; Y: save
	push	ax

	mov	cx,4			; loop thru four channels
DMEachChannel:
	test	al,1			;Q: Unmasking channel?
	jnz	short DMMask		; N: masking it, check for completion

	call	DMABeginChannel		; program 8237
	call	DMAUnMaskChannel	; unmask channel
	jmp	short DMNextChannel

DMMask:
	call	DMAMaskChannel		; mask channel to terminate DMA
	call	DMACompleteChannel	; check for completion

DMNextChannel:
	add	si,size DMARegRec	; next channel
	shr	al,1
	loop	DMEachChannel
	clc				; emulated
	pop	ax
	pop	cx
	pop	si
	ret

DMNoEm:
	stc
	pop	si
	ret

;==============================================================================
;==
;==  DMAClrFF(1-2) - Reset Controller FlipFlop
;==
;==  Entry: (Protected Mode Ring 0)
;==	AL = byte to output to port.
;==	DS = _DATA
;==	GS = R_CODE
;==	BX = port address
;==	DX = 0 => Emulate Input
;==	   <>0 => Emulate Output (Clear Flip-Flop)
;==
;==  Exit:  (Protected Mode Ring 0)
;==	AL  =  input or output value to port.
;==	CLC => I/O emulated by DMATrap.
;==	STC => I/O NOT emulated by DMATrap.
;==
;==============================================================================
DMAClrFF1:
	push	si
	xor	si,si				; index into controller 1
	jmp	short DMAClrFF

DMAClrFF2:
	push	si
	mov	si,1				; index into controller 2

DMAClrFF:
	call	DMACheckTC

	or	dx,dx				;Q: Input ?
	jz	short DCFFNoEm			; Y: Let it go
	mov	[DMARegSav][si].DMAFF,0		; N: emulate it
	clc
	pop	si
	ret

DCFFNoEm:
	stc
	pop	si
	ret
;==============================================================================
;==
;==  DMAReset(1-2) - Reset Controller
;==		     *** Need to FREE UP DMA buffer if busy ***
;==
;==  Entry: (Protected Mode Ring 0)
;==	AL = byte to output to port.
;==	DS = _DATA
;==	GS = R_CODE
;==	BX = port address
;==	DX = 0 => Emulate Input (Status Register).
;==	   <>0 => Emulate Output (Command Register).
;==
;==  Exit:  (Protected Mode Ring 0)
;==	AL  =  input or output value to port.
;==	CLC => I/O emulated by LIM_Trap.
;==	STC => I/O NOT emulated by LIM_Trap.
;==
;==============================================================================
DMAReset1:
	push	ax
	push	si

	mov	al,0Fh			; mask channels 0-3 on controller 1
	call	DMAMask1

	xor	si,si			; index into controller 1
	jmp	short DMAReset

DMAReset2:
	push	ax
	push	si

	mov	al,0Fh			; mask channels 1-3 on controller 2
	call	DMAMask2

	mov	si,1			; index into controller 2

DMAReset:
	or	dx,dx				;Q: Input ?
	jz	short DRNoEm			; Y: no reset
	mov	[DMARegSav][si].DMAFF,0		; N: reset controller FF
	mov	[DMARegSav][si].DMAStatus,0 	; and status

DRNoEm:
	stc					; do the I/O
	pop	si
	pop	ax
	ret
;==============================================================================
;==
;==  DMAResMsk(1-2) - Reset Mask Register
;==
;==  Entry: (Protected Mode Ring 0)
;==	AL = byte to output to port.
;==	DS = _DATA
;==	GS = R_CODE
;==	BX = port address
;==	DX = 0 => Emulate Input
;==	   <>0 => Emulate Output
;==
;==  Exit:  (Protected Mode Ring 0)
;==	AL  =  input or output value to port.
;==	CLC => I/O emulated by LIM_Trap.
;==	STC => I/O NOT emulated by LIM_Trap.
;==
;==============================================================================
DMAResMsk1:
	push	si
	lea	si,[DMARegSav].Chnl0	; channel 0 on controller 1
	jmp	short DMAResMsk

DMAResMsk2:
	push	si
	lea	si,[DMARegSav].Chnl4	; channel 0 on controller 2

DMAResMsk:
	call	DMACheckTC

	or	dx,dx			;Q: Emulate output?
	jz	short DRMNoEm	 	; N: no emulation
	push	cx			; Y: save

	mov	cx,4			; loop thru four channels
DRMEachChannel:
	call	DMAMaskChannel		; mask channel to terminate DMA
	call	DMACompleteChannel	; check for completion
	add	si,size DMARegRec	; next channel
	loop	DRMEachChannel
	pop	cx

DRMNoEm:
	stc
	pop	si
	ret
;==============================================================================
;==
;==  DMABase(0-7) - Write/Read DMA Channel Base Register
;==
;==  Entry: (Protected Mode Ring 0)
;==	AL = byte to output to port.
;==	DS = _DATA
;==	GS = R_CODE
;==	BX = port address
;==	DX = 0 => Emulate Input.
;==	   <>0 => Emulate Output.
;==
;==  Exit:  (Protected Mode Ring 0)
;==	SI = pts to proper DMARegRec channel
;==	DI = Controller index
;==
;==============================================================================
DMABase0:
	push	si
	push	di
	xor	di,di				; controller 1 index
	lea	si,[DMARegSav].Chnl0
	jmp	DMABase

DMABase1:
	push	si
	push	di
	xor	di,di				; controller 1 index
	lea	si,[DMARegSav].Chnl1
	jmp	DMABase

DMABase2:
	push	si
	push	di
	xor	di,di				; controller 1 index
	lea	si,[DMARegSav].Chnl2
	jmp	DMABase

DMABase3:
	push	si
	push	di
	xor	di,di				; controller 1 index
	lea	si,[DMARegSav].Chnl3
	jmp	DMABase

DMABase5:
	push	si
	push	di
	mov	di,1				; controller 2 index
	lea	si,[DMARegSav].Chnl5
	jmp	DMABase

DMABase6:
	push	si
	push	di
	mov	di,1				; controller 2 index
	lea	si,[DMARegSav].Chnl6
	jmp	DMABase

DMABase7:
	push	si
	push	di
	mov	di,1				; controller 2 index
	lea	si,[DMARegSav].Chnl7
	jmp	DMABase

;==============================================================================
;==
;==  DMACnt(0-7) - Write/Read DMA Channel Count Register
;==
;==  Entry: (Protected Mode Ring 0)
;==	AL = byte to output to port.
;==	DS = _DATA
;==	GS = R_CODE
;==	BX = port address
;==	DX = 0 => Emulate Input.
;==	   <>0 => Emulate Output.
;==
;==  Exit:  (Protected Mode Ring 0)
;==	SI = pts to proper DMARegRec channel
;==	DI = Controller index
;==
;==============================================================================
DMACnt0:
	push	si
	push	di
	xor	di,di				; controller 1 index
	lea	si,[DMARegSav].Chnl0
	jmp	DMACnt

DMACnt1:
	push	si
	push	di
	xor	di,di				; controller 1 index
	lea	si,[DMARegSav].Chnl1
	jmp	DMACnt

DMACnt2:
	push	si
	push	di
	xor	di,di				; controller 1 index
	lea	si,[DMARegSav].Chnl2
	jmp	DMACnt

DMACnt3:
	push	si
	push	di
	xor	di,di				; controller 1 index
	lea	si,[DMARegSav].Chnl3
	jmp	DMACnt

DMACnt5:
	push	si
	push	di
	mov	di,1				; controller 2 index
	lea	si,[DMARegSav].Chnl5
	jmp	short DMACnt

DMACnt6:
	push	si
	push	di
	mov	di,1				; controller 2 index
	lea	si,[DMARegSav].Chnl6
	jmp	SHORT DMACnt

DMACnt7:
	push	si
	push	di
	mov	di,1				; controller 2 index
	lea	si,[DMARegSav].Chnl7
	jmp	SHORT DMACnt

;==============================================================================
;==
;==  DMAPg(0-7) - Write/Read DMA Channel Page Register
;==
;==  Entry: (Protected Mode Ring 0)
;==	AL = byte to output to port.
;==	DS = _DATA
;==	GS = R_CODE
;==	BX = port address
;==	DX = 0 => Emulate Input.
;==	   <>0 => Emulate Output.
;==
;==  Exit:  (Protected Mode Ring 0)
;==	SI = pts to proper DMARegRec channel
;==	AL = DMA addr bits 16-23 for channels 1-3
;==	   = DMA addr bits 17-23 for channels 5-7
;==
;==============================================================================
DMAPg0:
	push	si
	lea	si,[DMARegSav].Chnl0
	jmp	DMAPg

DMAPg1:
	push	si
	lea	si,[DMARegSav].Chnl1
	jmp	DMAPg

DMAPg2:
	push	si
	lea	si,[DMARegSav].Chnl2
	jmp	short DMAPg

DMAPg3:
	push	si
	lea	si,[DMARegSav].Chnl3
	jmp	short DMAPg

DMAPg5:
	push	si
	lea	si,[DMARegSav].Chnl5
	jmp	SHORT DMAPg

DMAPg6:
	push	si
	lea	si,[DMARegSav].Chnl6
	jmp	SHORT DMAPg

DMAPg7:
	push	si
	lea	si,[DMARegSav].Chnl7
	jmp	SHORT DMAPg

;==============================================================================
;==
;==  DMABase - Write/Read DMA Channel N Base Register
;==
;==  Entry: (Protected Mode Ring 0), DX, DS & DI on stack
;==	AL = byte to output to port.
;==	DS:SI pts to the DMA save area of interest
;==	DI = Controller index
;==	DX = 0 => Emulate Input.
;==	   <>0 => Emulate Output.
;==
;==  Exit:  (Protected Mode Ring 0)
;==	AL  =  emulated input value from port.
;==	CLC => I/O emulated by DMATrap.
;==	STC => I/O NOT emulated by DMATrap.
;==
;==============================================================================
DMABase:
	push	eax
	push	ecx
	push	dx
	push	es

	push	ds
	pop	es				; ES = _DATA

	call	DMACheckTC

	movzx	bx,[DMARegSav][di].DMAFF	; Flip-Flop state for controller
	xor	[DMARegSav][di].DMAFF,1		; toggle Flip-Flop

	or	dx,dx				;Q: Input ?
	jz	short DBNRead			; Y: do Read operation

	and	[si].DMAChnFlgs,not fExtPI	; ISA compatible mode
	mov	byte ptr [si][3].DMALinAdr,0	; ISA compatible mode
	mov	byte ptr [si][bx].DMALinAdr,al	; write virtualized 8237 base
	or	[si].DMAChnFlgs,fDirtyAddr	; virtual <> physical
	jmp	short DMATrapExit

DBNRead:
	call	GetDMAAddress			; AL = current count
	jmp	short DMATrapExit

;==============================================================================
;==
;==  DMACnt - Write/Read DMA Channel N Count Register
;==
;==  Entry: (Protected Mode Ring 0), DX, DS & DI on stack
;==	AL = byte to output to port.
;==	DS:SI pts to the DMA save area of interest
;==	DI = Controller index
;==	DX = 0 => Emulate Input.
;==	   <>0 => Emulate Output.
;==
;==  Exit:  (Protected Mode Ring 0)
;==	AL  =  emulated input value from port.
;==	CLC => I/O emulated by DMATrap.
;==	STC => I/O NOT emulated by DMATrap.
;==
;==============================================================================
DMACnt:
	push	eax
	push	ecx
	push	dx
	push	es

	push	ds
	pop	es				; ES = _DATA

	call	DMACheckTC

	movzx	bx,[DMARegSav][di].DMAFF	; Flip-Flop state for controller
	xor	[DMARegSav][di].DMAFF,1		; toggle Flip-Flop

	or	dx,dx				;Q: Input ?
	jz	short DCNRead			; Y: do Read operation

	and	[si].DMAChnFlgs,not fExtPI	; ISA compatible mode
	mov	word ptr [si][2].DMACount,0	; ISA compatible mode
	mov	byte ptr [si][bx].DMACount,al
	or	[si].DMAChnFlgs,fDirtyCount	; virtual <> physical
	jmp	short DMATrapExit

DCNRead:
	call	GetDMACount			; AL = current count
	jmp	short DMATrapExit

;==============================================================================
;==
;==  DMAPg - Write/Read DMA Channel N Page Register
;==
;==  Entry: (Protected Mode Ring 0), DX, DS & DI on stack
;==	AL = byte to output to port.
;==	BX = port address for I/O * 2
;==	DS:SI pts to the DMA save area of interest
;==	DX = 0 => Emulate Input.
;==	   <>0 => Emulate Output.
;==
;==  Exit:  (Protected Mode Ring 0)
;==	AL  =  emulated input value from port.
;==	CLC => I/O emulated by DMATrap.
;==	STC => I/O NOT emulated by DMATrap.
;==
;==
;==  For channels 0-4, DMACount is in Bytes, and DMALinAdr holds the address as:
;==
;==            ÚÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;==            ³   31-24   ³   23-16   ³         15-0         ³
;==            ÃÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;==            ³ 0000 0000 ³  A23-A16  ³        A15-A0        ³
;==            ÀÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
;==
;==  For channels 5-7, DMACount is in Words, and DMALinAdr holds the address as:
;==
;==            ÚÄÄÄÄÄÄÄÄÄÄÄÂÄÄÂÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
;==            ³   31-24   ³23³ 22-16  ³         15-0         ³
;==            ÃÄÄÄÄÄÄÄÄÄÄÄÅÄÄÁÄÄÄÄÄÂÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´
;==            ³ 0000 0000 ³A23-A17 ³0 ³        A16-A1        ³
;==            ÀÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÁÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
;==
;==
;==============================================================================
DMAPg:
	push	di
	push	eax
	push	ecx
	push	dx
	push	es

	push	ds
	pop	es			; ES = _DATA

	call	DMACheckTC

	or	dx,dx			;Q: Input ?
	jz	short DPNRead		; Y: do Read operation

	and	[si].DMAChnFlgs,not fExtPI	; ISA compatible mode
	mov	byte ptr [si][3].DMALinAdr,0	; ISA compatible mode
	mov	byte ptr [si][2].DMALinAdr,al
	or	[si].DMAChnFlgs,fDirtyAddr	; virtual <> physical
	jmp	short DMATrapExit

DPNRead:
	test	[si].DMAChnFlgs,fDirtyAddr	;Q: virtual => physical?
	jz	short DPNRphys			; Y: physical read
	mov	al,byte ptr [si].DMALinAdr[2]   ; N: virtual  page
	jmp	short DMATrapExit

DPNRphys:
	movzx	ecx,[si].DMAChnlNum	; get port # for page register
	movzx	dx,[DMA_page_port][ecx]
	in	al,dx			; get byte from count register
	sub	al,byte ptr [si].DMAPhyAdr[2] ; Y: get starting physical address
	add	al,byte ptr [si].DMALinAdr[2] ; get starting linear address

;
;  unwind stack and return to IO_Trap
;
DMATrapExit:
	mov	[ReturnAL],al
	clc				; instruction was emulated
	pop	es
	pop	dx
	pop	ecx
	pop	eax
	pop	di
	pop	si
	mov	al,[ReturnAL]		; set AL for exit (read emulation)
	ret
;==============================================================================
;==
;==  DMACheckTC - Check if any of the channels have reached terminal count and
;==		  update virtualized status register.  If a channel has reached
;==		  TC, update flags and release DMA buffer if it was being used.
;==
;==  Entry: (Protected Mode Ring 0)
;==	DS = VDMD_GSEL
;==
;==  Exit:  (Protected Mode Ring 0)
;==
;==============================================================================
DMACheckTC proc	near
	push	ax
	push	cx
	push	si

	in	al,DMA_STAT1		; get status for controller 1
	or	[DMARegSav][0].DMAStatus,al ; update latest status information
	lea	si,[DMARegSav].Chnl0	; channel 0 on controller 1
	mov	cx,4			; loop through channels 0-3
	call	DCTCEachChannel

	in	al,DMA_STAT2		; get status for controller 2
	or	[DMARegSav][1].DMAStatus,al ; update latest status information
	lea	si,[DMARegSav].Chnl5	; channel 1 on controller 2
	mov	cx,3			; loop through channels 5-7
	call	DCTCEachChannel

	pop	si
	pop	cx
	pop	ax
	ret

DCTCEachChannel:
	test	al,1			;Q: Has TC been reached?
	jz	short DCTCNextChannel	; N: try next channel
	call	DMACompleteChannel	; Y: complete DMA transfer

DCTCNextChannel:
	add	si,size DMARegRec	; next channel
	shr	al,1
	loop	DCTCEachChannel
	ret

DMACheckTC endp

;==============================================================================
;==
;==  DMABeginChannel - Up to here, all DMA programming has been virtualized.
;==		       This routine checks to see if user buffer is contiguous.
;==		       If it is, programs the 8237 with the corresponding
;==		       physical address. If not contiguous, the DMA buffer
;==		       is used.
;==
;==  Entry: (Protected Mode Ring 0)
;==	DS:SI ptr to channel data area
;==
;==  Exit:  (Protected Mode Ring 0)
;==
;==============================================================================
DMABeginChannel proc	near
	pushad
	push	es

	btr	[si].DMAChnFlgs,fDMABufferBit	;Q: Channel using buffer?
	jnc	short DBCFormat			; N: nothing to be done
	and	[DMAFlags],not (fDMABufferBusy+fDMASystem) ;Y: free buffer

DBCFormat:
	test	[DMAFlags],fDMAFormat	;Q: Could this be a floppy format?
	jz	SHORT DBCGetAddress	; N: continue
;
;  Check to see if this is due to a floppy format.  If so, reduce the
;  size of the buffer from 64K to 4*(sectors/track)
;

	cmp	[si].DMACount,ORG_FORMAT_BUFFER_SIZE-1	;Q: Is it a format?
	jne	SHORT DBCGetAddress			; N: continue
	movzx	eax,[FormatBufferSize]			; Y: smaller buffer size
	mov	[si].DMACount,eax
	and	[DMAFlags],not fDMAFormat		; clear format flag

DBCGetAddress:
	call	DMAGetLinearAddress	; get linear address and size of buffer

	test	[si].DMAChnFlgs,fNoTrans;Q: Translate linear to physical?
	jnz	short DBCexit		; N: use given address ..
	jecxz	DBCexit			;    also if zero length transfer
	push	ecx			; Y: save transfer size

	test	[si].DMAChnFlgs,fDecMode;Q: Is controller in decrement mode?
	jz	short DBCContigCheck	; N: base and start address are the same
	sub	eax,ecx			; Y: calculate starting address

DBCContigCheck:
	call	ContigCheck		;Q: Physically contiguous buffer?
	pop	ecx			;   (restore size of transfer)
	jc	short DBCUseBuffer	; N: might use buffer

;  Check if ISA memory above 16 MB is supported.  If so, then use DMA
;  buffer when user buffer is above 16 MB.
;    eax = physical start address, ecx = transfer length in bytes

	test	gs:[genflags],fabove16M ;Q: Is ISA above 16M supported?
	jz	short DBCexit           ; N: continue
	mov	edx,ecx
	add	edx,eax
	cmp	edx,1000000h		;Q: Below 16M?
	jbe	short DBCexit   	; Y: Continue

DBCUseBuffer:
	test	[si].DMAChnFlgs, fCascadeOp	;Q: Cascade mode?
	jnz	short DBCexit			; Y: address/count don't matter
;
;  User Buffer is either not contiguous or a 64K/128K boundary crossing
;  is detected.  The DMA buffer must be used.
;  Must make sure the transfer size is smaller than the DMA buffer!!!!!!
;
;  Currently, if transfer size is larger, the user will be notified to reboot
;  the machine and restart CEMM with a larger DMA buffer size parameter.
;
;				 D:nnn, nnn=16,32,64,128
;
;				 A REBOOT MUST OCCUR!!!!
;

	test	[si].DMAChnFlgs,fAutoInit ;Q: AUTO INITIALIZE mode?
	jnz	short DBCModeError	  ; Y: nothing we can do!! (REBOOT)

	cmp	ecx,[DMABufferSize]	;Q: Larger than DMA buffer?
	ja	short DBCSizeError	; Y: nothing we can do!! (REBOOT)

	call	PreBufferedDMA

DBCexit:
	test	[si].DMAChnFlgs,fDecMode;Q: Is controller in decrement mode?
	jz	short DBCBaseAddr	; N: base and start address are the same
	add	eax,ecx			; Y: calculate starting address

DBCBaseAddr:
	call	DMASavePhysicalAddress
	call	DMAProgramChannel

	or	[si].DMAChnFlgs,fChnlActive	; set channel active

	mov	cx,[si].DMAChnlNum	; set active bit for this channel
	mov	al,1
	shl	ax,cl
	or	[DMAActive],al

	pop	es
	popad
	ret

DBCSizeError:
	shr	ecx,10			; size in 1K units
	add	cx,10h-1		; round up to next 16K boundary
	and	cx,not (10h-1)
	mov	bx,cx			; suggested D=nnn parameter size
	mov	ax,DMASizeErr
	PJmp	R1CODE_GSEL,R1_CODE:ErrHndlr

DBCModeError:
	mov	ax,DMAModeErr
	mov	bx,0
	PJmp	R1CODE_GSEL,R1_CODE:ErrHndlr

DMABeginChannel endp
;==============================================================================
;==
;==  DMACompleteChannel - Check if the channel was active (if not, do nothing)
;==			  If so, reset active bit and check if DMA buffer was
;==			  being used (if not, do nothing).  If DMA buffer is
;==			  in use by this channel, move data to user buffer if
;==			  a write operation took place.
;==
;==  Entry: (Protected Mode Ring 0)
;==	DS:SI ptr to channel data area
;==
;==  Exit:  (Protected Mode Ring 0)
;==	DMA operation completed (buffered/non-buffered)
;==
;==============================================================================
DMACompleteChannel proc	near
	push	ax
	push	cx

	mov	cx,[si].DMAChnlNum	; reset active bit for this channel
	mov	al,1
	shl	ax,cl
	not	al
	and	[DMAActive],al

	btr	[si].DMAChnFlgs,fChnlActiveBit	;Q: Is channel currently active?
	jnc	short DCCexit			; N: nothing to be done

	btr	[si].DMAChnFlgs,fDMABufferBit	;Q: Buffer being used?
	jnc	short DCCexit			; N: nothing to be done

	call	PostBufferedDMA			; complete buffered DMA transfer

DCCexit:
	pop	cx
	pop	ax
	ret
DMACompleteChannel endp

;==============================================================================
;==
;==  DMAMaskChannel - Physically masks a channel.
;==
;==  Entry: (Protected Mode Ring 0)
;==	DS:SI ptr to channel data area
;==
;==  Exit:  (Protected Mode Ring 0)
;==	Channel physically masked
;==
;==============================================================================
DMAMaskChannel proc	near
	push	ax
	push	dx
     					; M006 - Start
	test	[si].DMAChnFlgs,fExtPI	;Q: PS2 Ext PI?
	jz	short DMCISA		; N: ISA compatible

	test	gs:[GenFlags], fMCA	; Q: MCA ?
	jz	short DMCISA		; N: must be EISA

	xor	ax, ax
	mov	ax,[si].DMAChnlNum	; get single mask port channel number	
	or	ax, Set_Chn_Mask	; OR it with appropriate fn #
	mov	dx, DMA_XFN
	jmp	short DMCmask		; M006 - End

DMCISA:
	call	DMASetupSinMask		; get port in DX and channel AL
	or	al,00000100b		; set mask bit

DMCmask:
	out	dx,al			; mask channel

	pop	dx
	pop	ax
	ret
DMAMaskChannel endp
;==============================================================================
;==
;==  DMAUnMaskChannel - Physically unmasks a channel.
;==
;==  Entry: (Protected Mode Ring 0)
;==	DS:SI ptr to channel data area
;==
;==  Exit:  (Protected Mode Ring 0)
;==	Channel physically unmasked
;==
;==============================================================================
DMAUnMaskChannel proc	near
	push	ax
	push	dx
       					; M006 - Start
	test	[si].DMAChnFlgs,fExtPI	;Q: PS2 Ext PI?
	jz	short DUMCISA		; N: ISA compatible

	test	gs:[GenFlags], fMCA	; Q: MCA ?
	jz	short DUMCISA		; N: must be EISA

	xor	ax, ax
	mov	ax,[si].DMAChnlNum	; get single mask port channel number	
	or	ax, Reset_Chn_Mask	; OR it with appropriate fn #
	mov	dx, DMA_XFN
	jmp	short DUMCunmask	; M006 - End

DUMCISA:
	call	DMASetupSinMask		; get port in DX and channel AL
	and	al,11111011b		; clear mask bit

DUMCunmask:
	out	dx,al			; unmask channel

	pop	dx
	pop	ax
	ret
DMAUnMaskChannel endp
;==============================================================================
;==
;==  DMASetupSinMask - Gets single mask port number and seets up appropriate
;==		       channel number in AL.
;==
;==  Entry: (Protected Mode Ring 0)
;==	DS:SI ptr to channel data area
;==
;==  Exit:  (Protected Mode Ring 0)
;==	AL = 000000xxb, where xx is the channel number
;==	DX = Address for Single Mask port
;==
;==============================================================================
DMASetupSinMask	proc	near
	push	bx

	mov	bx,[si].DMAChnlNum	; get single mask port channel number
	movzx	dx,[DMA_single_mask_port][bx]
	mov	al,bl			; channel number
	and	al,03h			; modulo 4 (controller channel number)

	pop	bx
	ret
DMASetupSinMask	endp
;==============================================================================
;==
;==  PreBufferedDMA -
;==
;==  Entry: (Protected Mode Ring 0)
;==	DS:SI ptr to channel data area
;==
;==  Exit:  (Protected Mode Ring 0)
;==	EAX = Physical address of DMA buffer
;==	DMA channel flags and buffer ready for physically programming channel
;==
;==============================================================================
PreBufferedDMA	proc	near
	push	ecx
	push	edx
	push	esi
	push	edi
	push	ds
	push	es

;
;  Need to improve "wait till not busy scheme." System DMA might be depending
;  on an external interrupt also!!!!!		 QLEO
;
PBDCheckBuffer:
	test	[DMAFlags],fDMABufferBusy	;Q: DMA buffer busy?
	jz	short PBDBufferFree		; N: buffer is free

	test	[DMAFlags],fDMASystem		;Q: Is system DMA using buffer?
	jz	short PBDNonSystem		; N: turn interrupts on
	call	DMACheckTC			; Y: check if free
	jmp	short PBDCheckBuffer

PBDNonSystem:
	sti				; allow completion interrupt thru
	jmp	short PBDCheckBuffer

PBDBufferFree:
	cli
	or	[DMAFlags],fDMABufferBusy+fDMASystem	; DMA buffer used
	or	[si].DMAChnFlgs,fDMABuffer

	test	[si].DMAChnFlgs,fReadOp	;Q: Read operation
	jz	SHORT PBDexit		; N: copy data after completion
					; Y: copy user buffer to DMA buffer
	call	DMAGetLinearAddress	; ECX = size

	test	[si].DMAChnFlgs,fDecMode;Q: Is controller in decrement mode?
	jz	short PBDBaseAddr	; N: base and start address are the same
	sub	eax,ecx			; Y: calculate starting address

PBDBaseAddr:
	mov	esi,eax			; user buffer is the source
	mov	edi,[DMABufferAddress]	; DMA buffer is destination

	call	MoveBuffer

PBDexit:
	mov	eax,[DMABufferAddress]	; DMA buffer address (physical)
	pop	es
	pop	ds
	pop	edi
	pop	esi
	pop	edx
	pop	ecx
	ret
PreBufferedDMA	endp
;==============================================================================
;==
;==  PostBufferedDMA -  If DMA operation was a read, move data from DMA buffer
;==			to client/user buffer.
;==
;==  Entry: (Protected Mode Ring 0)
;==	DS:SI ptr to channel data area
;==
;==  Exit:  (Protected Mode Ring 0)
;==	DMA buffer free
;==
;==============================================================================
PostBufferedDMA proc	near
	push	eax
	push	ecx
	push	edx
	push	esi
	push	edi

	test	[si].DMAChnFlgs,fWriteOp;Q: Write operation
	jz	SHORT PoBDexit		; N: buffer copy is unnecessary
					; Y: copy DMA buffer to user buffer
	call	DMAGetLinearAddress	; ECX = size

	test	[si].DMAChnFlgs,fDecMode;Q: Is controller in decrement mode?
	jz	short PoBDStartAddr	; N: base and start address are the same
	sub	eax,ecx			; Y: calculate starting address

PoBDStartAddr:
	mov	esi,[DMABufferAddress]	; DMA buffer is source
	mov	edi,eax			; user buffer is the destination

	call	MoveBuffer

PoBDexit:
	and	[DMAFlags],not (fDMABufferBusy+fDMASystem) ; dma buffer free
	pop	edi
	pop	esi
	pop	edx
	pop	ecx
	pop	eax
	ret
PostBufferedDMA	endp
;==============================================================================
;==
;==  DMAGetLinearAddress - Get linear address from our channel data area
;==
;==  Entry: (Protected Mode Ring 0)
;==	DS:SI ptr to channel data area
;==
;==  Exit:  (Protected Mode Ring 0)
;==	EAX = linear Base Address
;==	ECX = size of transfer (bytes)
;==	EDX = boundary restriction (if crossed, will cause DMA to wrap)
;==
;==============================================================================
DMAGetLinearAddress	proc	near

	mov	eax,[si].DMALinAdr
	mov	ecx,[si].DMACount
	inc	ecx				; size of transfer
	mov	edx,10000h			; 64K boundary restriction

	test	[si].DMAChnFlgs,fExtPI		;Q: EISA/MCA Extended PI?	  	  ;PS2
	jnz	short DGLAExtMode		; Y: check type of mode

	cmp	[si].DMAChnlNum,4		;Q: Controller 2?
	jb	short DGLAexit			; N: no special treatment
	mov	edx,20000h			; Y: 128K boundary restriction
	shl	ecx,1				; adjust for word units

DGLAadjust:
	ror	eax,16
	shr	al,1				; D0 null in page reg
	rol	eax,17				; adjust for 'A0' offset

DGLAexit:
	cmp	eax,[I13BuffAddress]		;Q: Last INT 13h request?
	jne	short DGLAcont			; N: continue
	mov	[I13BuffAddress],0		; Y: mark DMA usage

DGLAcont:
	ret

DGLAExtMode:
	xor	edx,edx				; no boundary conditions
	test	[si].DMAChnFlgs,fWordTx		;Q: Word transfers?
	jz	short DGLAexit			; N: no more processing
	shl	ecx,1				; Y: adjust for word transfers

ifdef	ROMIDMCA
	cmp	[ROMID],ROMIDISA		;Q: EISA?
	je	short DGLAadjust		; Y: ISA compatible
	jmp	short DGLAexit			; N: return
endif
	test	gs:[GenFlags], fMCA		; Q: MCA ?
	jz	short DGLAadjust		; N: must be EISA
	jmp	short DGLAexit			; Y: return

DMAGetLinearAddress	endp
;==============================================================================
;==
;==  DMASavePhysicalAddress - Save the physical address to our channel data area
;==
;==  Entry: (Protected Mode Ring 0)
;==	EAX = Physical Address
;==	DS:SI ptr to channel data area
;==
;==  Exit:  (Protected Mode Ring 0)
;==
;==============================================================================
DMASavePhysicalAddress	proc	near

	test	[si].DMAChnFlgs,fExtPI		;Q: EISA/MCA Extended PI?	  	  ;PS2
	jnz	short DSPAExtMode		; Y: check type of mode

	cmp	[si].DMAChnlNum,4		;Q: Controller 2?
	jb	short DSPASaveIt		; N: save it as is

DSPAadjust:
	shr	eax,1				; adjust for implied 'A0'
	push	ax				; save A16-A1
	xor	ax,ax
	shl	eax, 1				; adjust for unused Pg Reg D0
	pop	ax				; restore A16-A1

DSPASaveIt:
	mov	[si].DMAPhyAdr,eax
	ret

DSPAExtMode:
	test	[si].DMAChnFlgs,fWordTx		;Q: Word transfers?
	jz	short DSPASaveIt		; N: no more processing

ifdef	ROMIDMCA
	cmp	[ROMID],ROMIDISA		;Q: EISA?
	je	short DSPAadjust		; Y: ISA compatible
	jmp	short DSPASaveIt		; N: return
endif
	test	gs:[GenFlags], fMCA		; Q: MCA ?
	jz	short DSPAadjust		; N: must be EISA
	jmp	short DSPASaveIt		; Y: return


DMASavePhysicalAddress	endp
;==============================================================================
;==
;==  DMAProgramChannel - Physically programs 8237 according to the current
;==			 channel data area.
;==
;==  Entry: (Protected Mode Ring 0)
;==	DS:SI ptr to channel data area
;==
;==  Exit:  (Protected Mode Ring 0)
;==	channel physically programmed
;==
;==============================================================================
DMAProgramChannel proc	near
	push	ax

	and	[si].DMAChnFlgs,not (fDirtyAddr+fDirtyCount) ; virtual=physical

ifdef	ROMIDMCA
	cmp	[ROMID],ROMIDPS2	;Q: PS2 machine?
	je	short DPCPS2		; Y: check if extended PI is used
endif
	test	gs:[GenFlags], fMCA	; Q: MCA ?
	jnz	short DPCPS2		; Y: check if extended PI is used

DPCISA:
	push	ebx
	push	dx

	xor	bx,bx			; physically clear clear FF
	call	DMASetUpFF

	movzx	ebx,[si].DMAChnlNum	; channel number

;
;  Program base address register
;
	movzx	dx,[DMA_address_port][bx]	; DMA Base register port
	mov	al,byte ptr [si][0].DMAPhyAdr	; low order byte
	out	dx,al

	mov	al,byte ptr [si][1].DMAPhyAdr	; high order byte
	out	dx,al

;
;  Program page register
;
	movzx	dx,[DMA_page_port][bx]		; page port address
	mov	al,byte ptr [si][2].DMAPhyAdr	; high word, low byte
	out	dx,al

;
;  Program count register
;
	movzx	dx,[DMA_count_port][bx]		; DMA count register port
	mov	al,byte ptr [si][0].DMACount	; low order byte
	out	dx,al

	mov	al,byte ptr [si][1].DMACount	; high order byte
	out	dx,al

	cmp	byte ptr [si][3].DMAPhyAdr,0	;Q: Super extended memory?
	jne	short DPCEISA			; Y: need to program EISA DMA

	test	[si].DMAChnFlgs,fExtPI		;Q: EISA extended PI used?
	jz	short DPCexit			; N: ISA programming complete

DPCEISA:
;
;  Program EISA high page register
;
	mov	dx,[DMA_EISA_HighPagePort][ebx*2]; EISA high page port address
	mov	al,byte ptr [si][3].DMAPhyAdr	 ; bits 24.31
	out	dx,al

;
;  Program EISA high count register
;
	mov	dx,[DMA_EISA_HighCountPort][ebx*2]; EISA high count register port
	mov	al,byte ptr [si][2].DMACount	  ; bits 16.23
	out	dx,al

DPCexit:
	pop	dx
	pop	ebx
	pop	ax
	ret


DPCPS2:
	test	[si].DMAChnFlgs,fExtPI	;Q: PS2 Ext PI?
	jz	short DPCISA		; N: ISA compatible
					; Y: program extended mode
;output base address

	mov	ax,[si].DMAChnlNum
	or	al,Set_Mem_Adr
	out	DMA_XFN,al
	jmp	$+2
	jmp	$+2

	mov	al,byte ptr [si][0].DMAPhyAdr
	out	DMA_EXE,al
	jmp	$+2
	jmp	$+2

	mov	al,byte ptr [si][1].DMAPhyAdr
	out	DMA_EXE,al
	jmp	$+2
	jmp	$+2

	mov	al, byte ptr [si][2].DMAPhyAdr
	out	DMA_EXE,al
	jmp	$+2
	jmp	$+2

;output count

	mov	ax,[si].DMAChnlNum
	or	al,Set_Count
	out	DMA_XFN,al
	jmp	$+2
	jmp	$+2

	mov	al,byte ptr [si][0].DMACount
	out	DMA_EXE,al
	jmp	$+2
	jmp	$+2

	mov	al,byte ptr [si][1].DMACount
	out	DMA_EXE,al
	jmp	$+2
	jmp	$+2

	pop	ax
	ret

DMAProgramChannel	endp

;==============================================================================
;==
;==  GetDMACount - Read the DMA current count byte
;==
;==  Entry: (Protected Mode Ring 0)
;==	BX = Internal flip flop state
;==	ES:SI pts to the DMA save area of interest
;==
;==  Exit:  (Protected Mode Ring 0)
;==	AL = current byte of count register
;==	Internal FF state updated
;==
;==============================================================================
GetDMACount	proc	near
	push	bx
	push	dx

	test	[si].DMAChnFlgs,fDirtyCount	;Q: virtual => physical?
	jz	short GDCphys			; Y: physical read
	mov	al,byte ptr [si][bx].DMACount	; N: virtual count
	jmp	short GDCexit

GDCphys:
	call	DMASetUpFF

	mov	bx,[si].DMAChnlNum	; get port # for count register
	movzx	dx,[DMA_count_port][bx]
	in	al,dx			; get byte from count register

GDCexit:
	pop	dx
	pop	bx
	ret
GetDMACount	endp

;==============================================================================
;==
;==  GetDMAAddress - Read the DMA current address byte
;==
;==  Entry: (Protected Mode Ring 0)
;==	BX = Internal flip flop state
;==	ES:SI pts to the DMA save area of interest
;==
;==  Exit:  (Protected Mode Ring 0)
;==	AL = current byte of count register
;==	Internal FF state updated
;==
;==============================================================================
GetDMAAddress	proc	near
	push	bx
	push	dx

	test	[si].DMAChnFlgs,fDirtyAddr	;Q: virtual => physical?
	jz	short GDAphys			; Y: physical read
	mov	al,byte ptr [si][bx].DMALinAdr	; N: virtual address
	jmp	short GDAexit

GDAphys:
	call	DMASetUpFF

	push	bx			; save FF state
	mov	bx,[si].DMAChnlNum	; get port # for count register
	movzx	dx,[DMA_address_port][bx]
	in	al,dx			; get byte from count register
	pop	bx			; restore FF state

	or	bx,bx			;Q: Need to adjust for lin/phy trans?
	jz	short GDAexit		; N: exit
	mov	dl,byte ptr [si].DMAPhyAdr[1] ; Y: get starting physical address
	and	dl,0F0h			; starting physical 4K page
	sub	al,dl			; number of 4K pages completed
	mov	dl,byte ptr [si].DMALinAdr[1] ; get starting linear address
	and	dl,0F0h			; starting linear 4K page
	add	al,dl			; current linear address

GDAexit:
	pop	dx
	pop	bx
	ret
GetDMAAddress	endp

;==============================================================================
;==
;==  DMASetUpFF - Set up Flip-Flop of controller to proper state
;==
;==  Entry: (Protected Mode Ring 0)
;==	BL =  desired state of physical Flip-Flop
;==	DS:SI pts to the DMA channel save area
;==
;==  Exit:  (Protected Mode Ring 0)
;==	Physical Flip-Flop state of controller is set up properly
;==
;==============================================================================
DMASetUpFF proc	near
	push	dx
	push	di

	mov	di,[si].DMAChnlNum	; get port for Clear Flip-Flop
	movzx	dx,[DMA_clr_FF_port][di]
	out	dx,al			; clear Flip-Flop

	or	bl,bl			;Q: Flip-Flop cleared?
	jz	SHORT DSUFFexit		; Y: Flip-Flop state OK
	movzx	dx,[DMA_count_port][di] ; N: set Flip-Flop
	in	al,dx

DSUFFexit:
	pop	di
	pop	dx
	ret
DMASetUpFF	endp

;==============================================================================
;==
;==  MoveBuffer - Moves a buffer.
;==
;==  Entry: (Protected Mode Ring 0)
;==	ECX = byte count
;==	ESI = 32-bit source buffer ptr
;==	EDI = 32-bit destination buffer ptr
;==
;==  Exit:  (Protected Mode Ring 0)
;==	Buffer moved (**QLEO: this routine should be interruptible!!!)
;==
;==============================================================================
MoveBuffer	proc	near
	push	ax
	push	ecx
	push	esi
	push	edi
	push	es

	mov	ax,DATA32_GSEL			; access all of memory
	mov	es,ax
	cld

	call	CheckPageProt			;Q: Write protection violation?
	jecxz	short MBexit			; Y: don't copy buffer

	test	cx,1				;Q: move an odd byte?
	jz	short MBword			; N: move word
	movs byte ptr es:[edi],byte ptr es:[esi]; Y: move a byte
	db	67h				; this and NOP need to be here
	nop					; due to an early 80386 bug!!!!!!!!

MBword:
	shr	ecx,1				; words
	test	cx,1				;Q: move an odd word?
	jz	short MBdword			; N: move dwords
	movs word ptr es:[edi],word ptr es:[esi]; Y: move a word
	db	67h				; this and NOP need to be here
	nop					; due to an early 80386 bug!!!!!!!!

MBdword:
	shr	ecx,1				; dwords
	rep movs dword ptr es:[edi],dword ptr es:[esi]
	db	67h				; this and NOP need to be here
	nop					; due to an early 80386 bug!!!!!!!!
MBexit:
	pop	es
	pop	edi
	pop	esi
	pop	ecx
	pop	ax
	ret
MoveBuffer	endp

;==============================================================================
;==
;==  ContigCheck - Checks if a linear buffer is physically contiguous.
;==
;==  Entry: (Protected Mode Ring 0)
;==	EAX = Starting linear address for buffer
;==	ECX = Size of buffer
;==	EDX = Boundary which may not be crossed (0=no boundary restrictions)
;==	      NOTE: boundary value must be larger than buffer size (ECX)
;==
;==  Exit:  (Protected Mode Ring 0)
;==	EAX = Starting physical address for buffer
;==	ECX = Size of maximum contiguous buffer which does not cross boundary
;==	EDX = 0 if boundary not crossed, else unchanged
;==	CY  = set, if buffer was not contiguous or crossed boundary condition
;==	      reset, if user buffer may be used for entire transfer
;==
;==============================================================================
ContigCheck 	proc	near
	push	ebx
	push	esi
	push	edi
	push	es

	mov	bx,PAGET_GSEL	; access to page tables
	mov	es,bx

	mov	ebx,eax
	add	ebx,ecx		; top of memory region
	jc	CCenough	; this should never happen
	dec	ebx		; ending linear address of buffer
	push	eax		; save starting linear address
	mov	esi,eax		; assume linear equals physical

	shr	eax,12		; beginning index into page table
	shr	ebx,12		; ending index into page table
;
;  Is buffer physically contiguous
;
	cmp	eax,[MaxPTEIndex] ;Q: Above page tables?
	jae	short CCphys1	  ; Y: physical and linear are the same

	test	word ptr es:[eax*4],P_WRITE ;Q: Is PTE write protected?
	jz	short CCphys1		    ; Y: lin=phy due to ROM
	mov	esi,es:[eax*4]	  	    ; N: get first physical address
CCphys1:
	and	si,0F000h	; clear control bits
	push	esi		; save starting physical page

CCNextPTE:
	add	esi,1000h	; next contiguous physical address
	inc	eax		; next PTE entry

	cmp	eax,ebx		;Q: Beyond buffer?
	ja    	short CCcont	; Y: buffer contiguous

	mov	edi,eax
	shl	edi,12		; assume physical and linear are the same

	cmp	eax,[MaxPTEIndex] ;Q: Above page tables?
	jae	short CCphys2	  ; Y: physical and linear are the same

	test	word ptr es:[eax*4],P_WRITE ;Q: Is PTE write protected?
	jz	short CCphys2		    ; Y: lin=phy due to ROM
	mov	edi,es:[eax*4]	  	    ; N: next physical address
CCphys2:
	and	di,0F000h
	cmp	esi,edi		;Q: Is it contiguous?
	je	CCNextPTE	; Y: try next PTE

CCcont:
	pop	eax		; starting physical page
	pop	ebx		; starting linear address
	and	bx,0FFFh	; offset into page
	or	ax,bx		; starting physical address

	dec	esi		; last addressable contiguous byte

	or	edx,edx		;Q: Any boundary restrictions?
	jz	short CCok	; N: no more checking

	cmp	edx,ecx		;Q: Is boundary restriction large enough?
	jb	short CCerror	; N: error

	test	eax,edx		;Q: Buffer starts at even boundary?
	jnz	short CCodd	; N: check if it also ends at an odd boundary?
	test	esi,edx		;Q: Does it also end on an even boundary?
	jz	short CCok	; Y: boundary is not crossed
	jmp	short CCbound	; N: it crossed a boundary

CCodd:
	test	esi,edx		;Q: Does it also end on an odd boundary?
	jnz	short CCok	; Y: boundary is not crossed

CCbound:
	mov	esi,eax		; starting physical address of buffer
	dec	edx
	or	esi,edx		; last byte before crossing boundary
	inc	edx		; restore boundary restriction
	jmp	short CCBufferEnd

;
; Calculate the largest contiguous buffer which does not cross a boundary
;
CCok:
	xor	edx,edx		; boundary not crossed

CCBufferEnd:
	inc	esi		; end of buffer
	sub	esi,eax		; largest contiguous buffer

	cmp	esi,ecx		;Q: Enough for user buffer
	jae	short CCenough	; Y: return OK status
	mov	ecx,esi		; N: return maximum size
	stc			; user buffer not usable for full transfer
	jmp	short CCexit

CCenough:
	clc			; user buffer is OK

CCexit:
	pop	es
	pop	edi
	pop	esi
	pop	ebx
	ret

CCerror:
	jmp 	short CCexit

ContigCheck	endp

;==============================================================================
;==
;== InitDMARegRec - macro for initting save area for channel in DMA controller
;==
;== Entry:
;==	chan_num = channel number (1-3)
;==	DMA_num  = DMA number (1-2)
;==	ES 	-> _DATA
;==
;==============================================================================
InitDMARegRec	MACRO	chan_num,cntrl_num

	lea	di,[DMARegSav.Chnl&chan_num]	; pt to channel's save area

	xor	ah,ah
	test	gs:[GenFlags],fEISA	;Q: EISA machine?
	jz	SHORT IDRRP&chan_num	; N: continue
	mov	dx,DMA_E_P&chan_num	; Y: read current high byte value
	in	al,dx
	mov	ah,al
IDRRP&chan_num:
	in	al,DMA_P&chan_num	; page register for channel
	jmp	$+2
	jmp	$+2			; timing
	shl	eax,16			; high EAX = high word of linear addr

	out	DMA_CLR_FF&cntrl_num,al	; clear flip-flop for controller
	jmp	$+2
	jmp	$+2			; timing
	in	al,DMA_B&chan_num	; get low byte of base
	jmp	$+2
	jmp	$+2			; timing
	xchg	ah,al
	in	al,DMA_B&chan_num	 ; get high byte of base
	xchg	ah,al
					; EAX = LINEAR BASE address
	cld
	stosd				; store LINEAR BASE address
	stosd				; store PHYSICAL BASE address

	xor	eax,eax			; clear EAX
	test	gs:[GenFlags],fEISA	;Q: EISA machine?
	jz	SHORT IDRRC&chan_num	; N: continue
	mov	dx,DMA_E_C&chan_num	; Y: read current bits 16.23. M007
	in	al,dx
	shl	eax,16
IDRRC&chan_num:
	jmp	$+2
	jmp	$+2			; timing
	in	al,DMA_C&chan_num	; get low byte of count
	jmp	$+2
	jmp	$+2			; timing
	xchg	ah,al
	in	al,DMA_C&chan_num	; get high byte of count
	xchg	ah,al			; EAX = count
	stosd				; store count

	ENDM
;==============================================================================
;==
;== InitDMA - initialize internal values for DMA registers
;==
;== Entry: (Protected Mode)
;==	DS = _DATA
;==
;== Exit:  (Protected Mode)
;==    _DATA:[DMARegSav] = DMA register save area initialized
;==
;==============================================================================
InitDMA	proc	near
	push	eax
	push	edx
	push	di
	push	es
	push	gs
	pushf

	cli

;
;  Set DMA/Bus Master Device Driver Interface Flag
;
	mov	ax,DATA32_GSEL
	mov	es,ax
	assume	es:ABS0

	or	es:[DBSflag],fDBSactive	; activate DMA/bus master interface

	mov	ax,VDMD_GSEL
	mov	es,ax
	assume	es:_DATA
	mov	ax,RCODEA_GSEL
	mov	gs,ax
	assume	gs:R_CODE

	InitDMARegRec	0,1		; init channel 0
	InitDMARegRec	1,1		; init channel 1
	InitDMARegRec	2,1		; init channel 2
	InitDMARegRec	3,1		; init channel 3
	InitDMARegRec	5,2		; init channel 5
	InitDMARegRec	6,2		; init channel 6
	InitDMARegRec	7,2		; init channel 7

	xor	al,al
	out	DMA_CLR_FF1,al		; clear ff on first cntrl
	mov	[DMARegSav][0].DMAFF,0	; reset variable
	jmp	$+2
	jmp	$+2			; for timing
	out	DMA_CLR_FF2,al		; clear ff on second cntrl
	mov	[DMARegSav][1].DMAFF,0	; reset variable

					; M006 - Start
	test	gs:[GenFlags], fMCA	; Q: MCA ?
	jz	short IDdone		; N: done
					; Y: initialize channel 4 structure

	lea	di,[DMARegSav].Chnl4	; channel 0 on controller 2
					; we're going to set the ext PI 
					; bit as channel can be programmed 
					; only thru the extended ports
	or	es:[di.DMAChnFlgs],fExtPI
	xor	eax, eax

	mov	al, Get_Mem_Adr OR 04h	; get current base address
	out	DMA_XFN, al 
	jmp	$+2
	jmp	$+2
	in	al, DMA_EXE
	jmp	$+2
	jmp	$+2
	xchg	ah, al

	in	al, DMA_EXE
	jmp	$+2
	jmp	$+2
	xchg	ah, al

    	movzx	edx, ax			; save the low 16 bits in dx

	xor	ax, ax
	in	al, DMA_EXE
	jmp	$+2
	jmp	$+2

	shl	eax, 16			; put high 8 bits in correct place
	or	eax, edx		; eax = current memory base value

	cld
	stosd				; store LINEAR BASE address
	stosd				; store PHYSICAL BASE address

					; get current count value
	xor	eax, eax
	mov	al, Get_Count OR 04h
	out	DMA_XFN, al
	jmp	$+2
	jmp	$+2
	in	al, DMA_EXE		; get low byte of count
	jmp	$+2
	jmp	$+2
	xchg	ah, al

	in	al, DMA_EXE		; get high byte of count
	jmp	$+2
	jmp	$+2
	xchg	ah, al
	stosd				; save count value
IDdone:					; M006 - End
	popf
	pop	gs
	pop	es
	pop	di
	pop	edx
	pop	eax
	ret
InitDMA	endp

;==============================================================================
;==
;==  pINT13hHandler: Breaks down disk transfers to less than the
;==		     DMA buffer size. QLEO: Need to allow 128K transfers!
;==
;==  Entry: (Protected Mode)
;==	INT 13h interface
;==
;==  Exit:
;==
;==============================================================================
ECC_CODE_SIZE   equ	4
FORMAT		equ     5
pINT13hHandler:
	push	ebp
	movzx	ebp,sp

	push	VDMD_GSEL	; DS/GS are setup for CEMM's 2 data areas.
	pop	ds
	push	RCODEA_GSEL
	pop	gs
	push	R1CODEA_GSEL
	pop	fs
	ASSUME	ds:_DATA,es:nothing,gs:R_CODE, fs:R1_CODE

	and	[DMAFlags],not fDMAFormat ; assume it's not a format

	test	gs:[TrapFlags],fI13trap	;Q: Reflect back?
	jnz	short pI13ProcessIO	; N: break down transfers

	cmp	ah,FORMAT		;Q: Format request?
	jne	short pINT13Reflect	; N: reflect
	or	[DMAFlags],fDMAFormat	; Y: format request

pINT13Reflect:
	push	13h
	jmp	ReflectInterrupt

;
; Set up registers for next I/O
;
pI13ProcessIO:
	or	ah,ah			;Q: First time thru?
	jz	pI13cont		; N: continue
					; Y: initialize data
;
;  First time through (original request)
;
	and	ebx,0FFFFh		; mask off hi word
	mov	[I13Operation],ah	; save operation (READ/WRITE)
	mov	[I13DriveNumber],dl	; save drive number
	mov	[I13OrigNumSect],al	; original # sectors to transfer
	mov	[I13OrigStartSect],cl
	and	[I13OrigStartSect],3Fh	; original starting sector
	mov	[I13OrigStartHead],dh	; original head number
	mov	[I13OrigStartCyl],cx
	and	[I13OrigStartCyl],0FFC0h; original cylinder number

ifdef *128KDMA
	movzx	esi,[bp.VTFO+VMTF_ES]	; get buffer segment
	shl	esi,4
	add	esi,ebx			; linear address of user buffer
	mov	ecx,esi			; used for DMA check
	shr	esi,4			; normalize buffer
	mov	[bp.VTFO+VMTF_ES],si	; normalized buffer segment
	and	bx,0Fh
	mov	[I13OrigBuffOff],bx	; normalized buffer offset
endif

	movzx	ecx,[bp.VTFO+VMTF_ES]	; get buffer segment		*128KDMA
	shl	ecx,4			;                            	*128KDMA
	add	ecx,ebx			; linear address of user buffer	*128KDMA
	mov	[I13OrigBuffStart],ecx	; buffer start		 	*128KDMA

	mov	fs:[I13SectTrans],0	; no sectors have been transferred
;
;  Check to see if previous operation used DMA
;
	cmp	[I13BuffAddress],0	;Q: Outstanding DMA?
	je	short pI13GetIndex	; N: get this request's index
	movzx	ebx,[I13DriveIndex]	; Y: drive index w/outstanding DMA tx
	btr	fs:[I13DriveTrap],ebx	; don't trap that drive
	mov	[I13BuffAddress],0	; no outstanding DMA

pI13GetIndex:
;
;  Index into proper drive data and initialize for this drive and operation
;
	movzx	ebx,dl
	btr	bx,7			;Q: Is it a fixed disk?
	jc	short pI13CheckTrap	; Y: continue
	add	bx,FixedDrives		; N: floppy data
	xor	ecx,ecx			; don't check for DMA

pI13CheckTrap:
	btr	[I13CheckDMA],ebx	;Q: Has DMA been checked for this drive?
	jnc	short pI13Index		; Y: get proper data
	mov	[I13BuffAddress],ecx	; N: save for DMA check
	mov	[I13DriveIndex],bl	; current drive index

pI13Index:
	inc	bx			; index into drive data

	mov	al,[HeadsPerCylinder][bx]
	mov	[HeadsPerCylinder],al	; heads per cylinder on this drive

	mov	al,[SectorsPerTrack][bx]
	mov	[SectorsPerTrack],al	; sectors per track on this drive

	mov	ax,[BytesPerSector][ebx*2]
	test	[I13Operation],LONG	;Q: Long sector?
	jz	short pI13SectSize	; N: regular sectors
	add	ax,ECC_CODE_SIZE	; Y: add 4 bytes of ECC code
pI13SectSize:
	mov	[BytesPerSector],ax	; sector size for drive and operation

;
;  Calculate current starting coordinates.
;
pI13cont:
;*128KDMA	movzx	eax,[I13OrigStartSect]	; original starting sector

	movzx	ax,[I13OrigStartSect]	; original starting sector
	dec	al			; zero relative
	add	al,fs:[I13SectTrans]	; current starting sector
	div	[SectorsPerTrack]
	inc	ah			; one relative
	mov	[I13CurrStartSect],ah	; save current sector number

	xor	ah,ah
	add	al,[I13OrigStartHead]
	adc	ah,0
	div	[HeadsPerCylinder]
	mov	[I13CurrStartHead],ah	; save starting head number

	xor	ah,ah
	add	al,byte ptr [I13OrigStartCyl][1] ; cylinder number
	adc	ah,0				 ; if overflow
	shl	ah,6				 ; upper 2 bits
	add	ah,byte ptr [I13OrigStartCyl]
	mov	byte ptr [I13CurrStartCyl][1],al ; save current cylinder number
	mov	byte ptr [I13CurrStartCyl],ah	 ; upper 2 bits

;
;  Calculate current buffer pointer.  (Check 32 bit MUL problem with early 386s)
;
;*128KDMA	movzx	eax,gs:[I13SectTrans]	; sectors already transferred
	movzx	ax,fs:[I13SectTrans]	; sectors already transferred
	mul	[BytesPerSector]	; offset into buffer 	       	*128KDMA
	shl	edx,16			; allow greater than 64K       	*128KDMA
	or	dx,ax			; 				*128KDMA
	add	edx,[I13OrigBuffStart]	; get current buffer start	*128KDMA
	mov	[I13CurrBuffStart],edx	;				*128KDMA

;
;  Get number of bytes left to transfer
;
;*128KDMA	movzx	eax,[I13OrigNumSect]	; number of sectors to transfer
	movzx	ax,[I13OrigNumSect]	; number of sectors to transfer
	sub	al,fs:[I13SectTrans]	; number of sectors left to transfer
	jz	pI13error		; there should always be some!
	mul	[BytesPerSector]
;*128KDMA	mov	ecx,eax
	shl	edx,16
	or	dx,ax			; number of bytes left		*128KDMA
	mov	ecx,edx			;				*128KDMA
;*128KDMA	or	ecx,edx

	push	eax			;				*128KDMA
	cmp	ecx,[DMABufferSize] 	;Q: Smaller than DMA buffer?
	jbe	short pI13complete	; Y: transfer the rest
					; N: get linear address for buffer
;
;  Get maximum contiguous transfer
;
ifdef *128KDMA
	movzx	eax,[bp.VTFO+VMTF_ES]	; normalized buffer segment
	shl	eax,4
	add	eax,[I13CurrBuffOff]	; current lin address of buffer	*128KDMA
endif
	mov	eax,[I13CurrBuffStart]	; current lin address of buffer	*128KDMA

	mov	edx,10000h		; can not cross 64K boundary
	cmp	bx,FixedDrives		;Q: Fixed drive?		*128KDMA
	ja	short @f		; N: 64K boundary is OK         *128KDMA
	shl	edx,1			; Y: 128K boundary              *128KDMA
@@:					;				*128KDMA
	cmp	ecx,edx			;Q: Is buffer > boundary 	*128KDMA
	ja	short pI13complete	; Y: the ROM will reject it	*128KDMA

	call	ContigCheck		;Q: Is the user buffer contiguous?
	jc	short pI13notContig	; N: use DMA buffer or contig buffer
					; Y: complete transfer
pI13complete:
	and	gs:[TrapFlags],not fI13trap ; transfer complete, don't trap
	jmp	short pI13exit

;
;  Transfer the larger of either the DMA buffer or contiguous space
;
pI13notContig:
	cmp	ecx,[DMABufferSize]	;Q: DMA buffer size larger?
	jae	short pI13exit		; N: use contiguous buffer space
	mov	ecx,[DMABufferSize]	; Y: use DMA buffer

;
;  Setup the registers for the ROM INT13h interface
;
pI13exit:
	pop	eax			;				*128KDMA
;*128KDMA	mov	eax,ecx
	mov	ax,cx
	mov	edx,ecx
	shr	edx,16
	div	[BytesPerSector]
	add	fs:[I13SectTrans],al	; number of sectors transferred

	mov	ah,[I13Operation]	; type of operation (READ/WRITE)
;*128KDMA	mov	bx,[I13CurrBuffOff]	; current buffer offset
	mov	ebx,[I13CurrBuffStart]	; current buffer offset		*128KDMA
	ror	ebx,4			; segment in bx                 *128KDMA
	mov	[bp.VTFO+VMTF_ES],bx	; normalized buffer segment     *128KDMA
	shr	ebx,28			; offset in bx                  *128KDMA

	mov	cx,[I13CurrStartCyl]	; current cylinder number
	or	cl,[I13CurrStartSect]	; current sector number
	mov	dh,[I13CurrStartHead]	; current head number
	mov	dl,[I13DriveNumber]	; drive number

	pop	ebp
	iretd

pI13error:
	pop	ebp
	iretd

;=======================================================================
;
;	Procedure	: FlushDMAState
;
;	Input		: DS = _DATA (VDMD_GSEL)
;			  GS = R_CODE (R_CODEA_GSEL)
;
;	Checks to see if the fDirtyAddr bit or the fDirtyCount bit is set
;	on any channel. If set it writes out the virtual DMA address and 
;	count registers out by calling DMAProgramChannel.
;
;========================================================================
FlushDMAState	proc	near

	push	si
	push	cx

	lea	si,[DMARegSav].Chnl0	; channel 0 on controller 1
	mov	cx, 8			; 8 dma channels

FDSnext:
	test	[si].DMAChnFlgs,fDirtyAddr+fDirtyCount	;Q: virtual => physical?
	jz	FDScont
	call	DMAProgramChannel
FDScont:
	add	si,size DMARegRec	; next channel
	loop	FDSnext

	pop	cx
	pop	si
	ret

	
FlushDMAState 	endp
_TEXT	ends				; end of segment

;=============================================================================
;==	C O D E  S E G M E N T
;=============================================================================
R_CODE	segment
	assume	cs:R_CODE,ds:nothing,es:nothing
;==============================================================================
;==
;==  rINT13hHandler: This is an interrupt 13h handler which monitors activity to
;==	      	     the drives for DMA purposes.  If it detects a user buffer
;==		     (ES:BX) in an EMS window and its corresponding  physical
;==		     memory is discontiguous, the operation will be modified.
;==
;==			1) The operation will be broken down to multiple read/
;==			   write requests, each being less than the DMA buffer.
;==
;==			2) If a format request is encountered, a flag will be
;==			   set so the DMA programming by the ROM will be modified
;==			   not to use a 64K buffer.
;==
;==  Entry: (Real Mode)
;==	INT 13h interface
;==
;==  Exit:
;==
;==============================================================================


rINT13hHandler:

	test	cs:[Current_State],fState_Active ;Q: in Virtual mode ?
	jz	short rI13oldHandler		 ; N: don't need to process
	jmp	cs:[rInt13HEntry]
rI13oldHandler:
	jmp	cs:[PrevInt13]


R_CODE	ends

;=============================================================================
;==	C O D E  S E G M E N T
;=============================================================================
LAST	segment
	assume	cs:LAST,ds:_DATA,es:_DATA,gs:R_CODE
;==============================================================================
;==
;==  DMAInit: This routine initializes necessary data structures for
;==	      trapping DMA programming in virtual 8088 mode.
;==
;==  Entry: (Real Mode)
;==
;==  Exit:
;==
;==============================================================================
DMAInit	proc	near
	push	es

;
; Allocate the DMA buffer. (This buffer should be below 16MB)
;
DIGetDMABuffer:
	mov	eax,[DMABufferSize]	; size of DMA buffer

	test	[DMAFlags],fDMABuffXT	;Q: Need buffer below 1 Meg?
	jnz	short DIBelowMeg	; Y:  get it

	mov	ebx,DMA_BOUNDARY	; DMA boundary
	call	MemGet			;Q: Enough free memory?
	mov	[DMABufferAddress],ebx	;   save starting address
	jnc	short DIBelow16M	; Y: make sure it is below 16Meg
	or	gs:[msg_flag],MEM_ERR_MSG;N: do not load
	jmp	DIexit

DIBelow16M:
ifndef LC910610
;
; Make sure buffer is not located in the HMA so the A20 state is not a concern.
;
	cmp	ebx,110000h		;Q: Is it above the HMA?
	jb	short DIGetDMABuffer	; N: try again
endif
	add	ebx,eax
	cmp	ebx,1000000h		;Q: Below 16M?
	jbe	short DIDriveParms	; Y: initialize drive parameters

DIBelowMeg:
	movzx	ebx,cs:[EndDriver]	; put buffer in base
	shl	ebx,4			; starting address
	mov	ecx,eax
	dec	ecx
	add	ecx,ebx			; ending address in ECX
;
;  Make sure alignment/boundary conditions meet DMA specifications
;
	cmp	eax,10000h		;Q: DMA buffer larger than 64K?
	jae	short DIBM64Kalign	; Y: align on 64K boundary
;
;  Check to see if a 64K boundary is crossed
;
	test	ebx,10000h		;Q: Is start of buffer an even 64K
	jnz	short DIBModd		; N: start is odd
	test	ecx,10000h		;Q: Is end of buffer even?
	jnz	short DIBM64Kalign	; N: align to 64K boundary
	jmp	short DIBMsave		; Y: buffer OK
DIBModd:
	test	ecx,10000h		;Q: Is end of buffer even?
	jnz	short DIBMsave		; N: buffer OK
;
;  Must align to a 64K boundary
;
DIBM64Kalign:
	mov	ecx,ebx			; restore start of buffer
	add	ebx,10000h-1		; round to next 64K boundary
	and	ebx,not (10000h-1)
	sub	ecx,ebx			; get offset from current [EndDrive]
	sub	eax,ecx			; add to size of buffer

DIBMsave:
	mov	[DMABufferAddress],ebx	; save starting address
	shr	eax,4			; size in paragraphs
	add	cs:[EndDriver],ax	; base memory usage grew
	or	[DMAFlags],fDMABuffXT	; buffer below 1 Meg

DIDriveParms:
	call	DriveParmsInit
ifdef TSSQLEO
;
; Access to TSS via ES for IO bit map access.
;
	mov	ax,TSS
	mov	es,ax
endif
;
; The addresses of the DMA ports are set in the IO bit map so that accesses
; to them while in virtual 8088 mode will be trapped.
;
	mov	bx,8000h		; 1K IO address space

	test	gs:[GenFlags],fEISA	;Q: EISA machine?
	jz	SHORT DIMCAPorts	; N: continue
;
;  Trap EISA specific DMA ports
;
	xor	bx,bx			; 64K IO address space
	mov	cx,TOTAL_DMA_EISA_PORTS ; trap EISA DMA ports
	xor	si,si
DISetEISAPorts:
	mov	ax,[DMAEISAPortList][si]
	call	_TEXT:PortTrapFar
	inc	si
	inc	si
	loop	DISetEISAPorts

DIMCAPorts:

ifdef	ROMIDMCA
	cmp	[ROMID],ROMIDPS2	;Q: MCA machine?
	jne	SHORT DIISAPorts	; N: continue
endif
	test	gs:[GenFlags], fMCA	; Q: is this MCA
	jz	SHORT DIISAPorts	; N: continue
	xor	bx,bx			; Y: don't alias

;
;  Trap MCA specific DMA ports
;
	mov	cx,TOTAL_DMA_MCA_PORTS	; trap MCA DMA ports
	xor	si,si
DISetMCAPorts:
	movzx	ax,[DMAMCAPortList][si]
	call	_TEXT:PortTrapFar
	inc	si
	loop	DISetMCAPorts

;
;  Trap ISA specific DMA ports
;
DIISAPorts:
	mov	cx,TOTAL_DMA_PORTS
	xor	si,si
DISetISAPorts:
	movzx	ax,[DMAPortList][si]
	call	_TEXT:PortTrapFar
	inc	si
	loop	DISetISAPorts


DIexit:
	pop	es
	ret
DMAInit	endp

	assume	cs:LAST,ds:_DATA,es:_DATA,gs:R_CODE
;==============================================================================
;==
;==  DriveParmsInit: This routine initializes necessary data structures for
;==	             breaking down INT 13h drive requests.
;==
;==  Entry: (Real Mode)
;==
;==  Exit:
;==
;==============================================================================
DriveParmsInit	proc	near

	push	fs

	mov	si, word ptr gs:[EMM_rFarEntry+2]
	mov	fs, si		; fs = R1_CODE seg
	assume	fs:R1_CODE

	xor	esi,esi			; index for drive data
	mov	cx,TotalDrives		; total drives supported
DPINextDrive:
	push	cx
	mov	dx,si
	or	dl,80h			; assume fixed disk parameters
	cmp	si,FixedDrives		;Q: Fixed drive?
	jb	short DPIGetParms	; Y: get parameters
	mov	dx,si			; N: get floppy drive parameters
	sub	dl,FixedDrives		; floppy drive number

DPIGetParms:
	mov	ah,08			; get drive parameters
	int	13h			;Q: Error
	jc	short DPIerr		; Y: -1 for calculations
	and	cl,03fh 		; N: get number of sectors per track

	or	cl,cl			;Q: Sector information given?
	jz	short DPIerr		; N: -1 for calculations

	cmp	cl,cs:[MaxSectorsPerTrack]	;Q: Largest track so far?
	jbe	DPIsectors			; N: continue
	mov	cs:[MaxSectorsPerTrack],cl	; Y: save it
	jmp	short DPIsectors

DPIerr:
	btr	fs:[I13DriveTrap],esi	; don't trap on this drive
	mov	cl,-1			; N: -1 for calculations
DPIsectors:
	inc	si			; index for drive data
	mov	[SectorsPerTrack][si],cl
	inc	dh
	mov	[HeadsPerCylinder][si],dh

	pop	cx
	loop	DPINextDrive
;
;  Calculate size of the largest format buffer needed
;
	movzx	ax,cs:[MaxSectorsPerTrack]; sectors/track*4=format buffer size
	shl	ax,2
	dec	ax
	mov	[FormatBufferSize],ax

;
;  Get number of bytes per sector!  (** assumption: 512 bytes/sector)
;
;  ** Need to find a way to get the number of bytes per sector on a
;  ** physical drive!!  A BPB is a logical drive structure and therefore
;  ** not useful unless a link between the logical physical drive is available.
;
	mov	ecx,TotalDrives
	mov	ax,200h			; ** WARNING: 512 bytes per sector
DPIbytes:
	mov	[BytesPerSector][ecx*2],ax
	loop	DPIbytes

;
;  Calculate number of sectors which fit in the DMA buffer
;
	mov	ecx,TotalDrives
DPIDMABuffer:
	mov	eax,[DMABufferSize]
	mov	edx,eax
	shr	edx,16
	mov	bx,[BytesPerSector][ecx*2]
	div	bx
	cmp	ax,100h				;Q: more than 255 sectors?
;
; If DMABufferSize is 128K then ax =100h. Therefore the foll. test should 
; be jl and not jle.
;	jle	DPIbsCont			; N: save correct number

	jl	DPIbsCont			; N: save correct number
	mov	al,-1				; Y: 255 sectors!
DPIbsCont:
	mov	fs:[SectorsInDMABuffer][ecx-1],al

	mov	eax,[DMABufferSize]
	mov	edx,eax
	shr	edx,16
	mov	bx,[BytesPerSector][ecx*2]
	add	bx,ECC_CODE_SIZE
	div	bx
	cmp	ax,100h				;Q: more than 255 sectors?
;
; If DMABufferSize is 128K then ax =100h. Therefore the foll. test should 
; be jl and not jle.
;	jle	DPIblCont			; N: save correct number

	jl	DPIblCont			; N: save correct number
	mov	al,-1				; Y: 255 sectors!
DPIblCont:
	mov	fs:[LongSectorsInDMABuffer][ecx-1],al

	loop	DPIDMABuffer

	pop	fs
	ret				; return
DriveParmsInit	endp

MaxSectorsPerTrack	db	0	; initializes format buffer

LAST	ends

	end				; end of module

