.386p
	page 58,132
;=============================================================================
	title	D M A E I S A - EISA DMA Emulation
;=============================================================================
;==
;== (C) Copyright MICROSOFT Corp. 1989-1991
;== (C) Copyright COMPAQ Computer Corp. 1989-1991
;==
;==	Title:	EMM386.EXE - MICROSOFT Expanded Memory Manager 386 Driver
;==
;==	Module: DMAEISA  - EISA DMA Emulation
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
;=============================================================================
;=============================================================================
;==	P U B L I C   D E C L A R A T I O N S
;=============================================================================
	public	DMAEISACnt0
	public	DMAEISACnt1
	public	DMAEISACnt2
	public	DMAEISACnt3
	public	DMAEISACnt5
	public	DMAEISACnt6
	public	DMAEISACnt7
	public	DMAEISAPg0
	public	DMAEISAPg1
	public	DMAEISAPg2
	public	DMAEISAPg3
	public	DMAEISAPg5
	public	DMAEISAPg6
	public	DMAEISAPg7
	public	DMAEISAExt1
	public	DMAEISAChain1
	public	DMAEISAExt2
	public	DMAEISAChain2
;=============================================================================
;==	L O C A L   C O N S T A N T S
;=============================================================================
	include vdmseg.inc
	include vdmsel.inc
	include dma.inc
	include	emmdata.inc
;=============================================================================
;==	E X T E R N A L   R E F E R E N C E S
;=============================================================================
_TEXT	segment
	extrn	DMACheckTC:near
	extrn	DMABeginChannel:near
	extrn	DMATrapExit:near
_TEXT	ends

_DATA	segment
	extrn	DMARegSav:byte
_DATA	ends
;=============================================================================
;==	C O D E  S E G M E N T
;=============================================================================
_TEXT	segment
	assume	cs:_TEXT,ds:_DATA,es:_DATA,gs:R_CODE,ss:STACK
;==============================================================================
;==
;== EISA DMA Channel Interrupt Status Register: (READ) (40Ah)
;==
;== 76543210
;== xxxxxxxxÄÄÄ> Interrupt on channel 0
;== ³³³³³³ÀÄÄÄÄ> Interrupt on channel 1
;== ³³³³³ÀÄÄÄÄÄ> Interrupt on channel 2
;== ³³³³ÀÄÄÄÄÄÄ> Interrupt on channel 3
;== ³³³ÀÄÄÄÄÄÄÄ> reserved
;== ³³ÀÄÄÄÄÄÄÄÄ> Interrupt on channel 5
;== ³ÀÄÄÄÄÄÄÄÄÄ> Interrupt on channel 6
;== ÀÄÄÄÄÄÄÄÄÄÄ> Interrupt on channel 7
;==
;== EISA DMA Chaining Mode Status Register: (READ) (4D4h)
;==
;== 76543210
;== xxxxxxxxÄÄÄ> Channel 0 enabled
;== ³³³³³³ÀÄÄÄÄ> Channel 1 enabled
;== ³³³³³ÀÄÄÄÄÄ> Channel 2 enabled
;== ³³³³ÀÄÄÄÄÄÄ> Channel 3 enabled
;== ³³³ÀÄÄÄÄÄÄÄ> reserved
;== ³³ÀÄÄÄÄÄÄÄÄ> Channel 5 enabled
;== ³ÀÄÄÄÄÄÄÄÄÄ> Channel 6 enabled
;== ÀÄÄÄÄÄÄÄÄÄÄ> Channel 7 enabled
;==
;== EISA DMA Chaining Mode Register: (WRITE) (40Ah & 4D4h)
;==
;== 76543210
;== xxxxxxxx
;== ³³³³³³ÀÁÄÄÄ> Channel 0-3 selected
;== ³³³³ÀÁÄÄÄÄÄ> 0-Disable, 1-Enable, 2-illegal, 3-Programming complete
;== ÀÁÁÁÄÄÄÄÄÄÄ> reserved
;==
;==============================================================================
DMA_E_IS	equ	40Ah 	; Channel interrupt status register (read only)
DMA_E_CS	equ	4D4h 	; Chaining mode status register (read only)

DMA_E_CH1	equ	40Ah	; ChainMode for channels 0-3 (write only)
DMA_E_CH2	equ	4D4h	; ChainMode for channels 5-7 (write only)

DMAEISAChain1:
	push	si
	lea	si,[DMARegSav].Chnl0	; channel 0 on controller 1
	jmp	short DMAEISAChain

DMAEISAChain2:
	push	si
	lea	si,[DMARegSav].Chnl4	; channel 0 on controller 2

DMAEISAChain:
	push	cx
	call	DMACheckTC

	or	dx,dx			;Q: Emulate output?
	jz	short DECNoEm	 	; N: no emulation

	mov	cl,al			; get channel being used
	and	cx,00000011b            ;Q: Channel 0?
	jcxz	DECChannel		; Y: have channel 0 context
DECFindChannel:
	add	si,size DMARegRec	; next channel
	loop	DECFindChannel		;Q: Channel found? (if not, loop)
					; Y: channel context at [SI]
DECChannel:
;
;  If DMA buffer is used by any buffer in the buffer chain (except for the
;  last one), it will cause data integrity problems.  This can be solved by
;  using buffer chaining to emulate a scatter/gather implementation. **QLEO?**
;
	call	DMABeginChannel

DECNoEm:
	stc
	pop	cx
	pop	si
	ret

;==============================================================================
;==
;== EISA DMA Extended Mode Register: (WRITE) (40Bh & 4D6h)
;==
;== 76543210
;== xxxxxxxx
;== ³³³³³³ÀÁÄÄÄ> Channel 0-3 selected
;== ³³³³ÀÁÄÄÄÄÄ> 0-8/byte, 1-16/word, 2-32/byte, 3-16/byte
;== ³³ÀÁÄÄÄÄÄÄÄ> 0-ISA, 1-"A", 2-"B", 3-"C" (Burst)
;== ³ÀÄÄÄÄÄÄÄÄÄ> T-C is an input for this channel
;== ÀÄÄÄÄÄÄÄÄÄÄ> Stop register disabled
;==
;==============================================================================
fWordMode	equ 00000100b
fNotWordMode	equ 00001000b

DMAEISAExt1:
	push	si
	lea	si,[DMARegSav].Chnl0	; channel 0 on controller 1
	jmp	short DMAEISAExtended

DMAEISAExt2:
	push	si
	lea	si,[DMARegSav].Chnl4	; channel 0 on controller 2

DMAEISAExtended:
	push	cx
	call	DMACheckTC

	or	dx,dx			;Q: Emulate output?
	jz	short DEENoEm	 	; N: no emulation

	mov	cl,al			; get channel being used
	and	cx,00000011b            ;Q: Channel 0?
	jcxz	DEEChannel		; Y: have channel 0 context
DEEFindChannel:
	add	si,size DMARegRec	; next channel
	loop	DEEFindChannel		;Q: Channel found? (if not, loop)
					; Y: channel context at [SI]
DEEChannel:
	and	[si].DMAChnFlgs,not fWordTx ; assume byte count

	test	al,fNotWordMode		;Q: Word count?
	jnz	short DEENoEm		; N: byte count

	test	al,fWordMode		;Q: Word count?
	jz	short DEENoEm		; N: byte count
	or	[si].DMAChnFlgs,fWordTx ; Y: word count

DEENoEm:
	stc
	pop	cx			; N: program 8237
	pop	si
	ret
;==============================================================================
;==
;==  DMAEISACnt(0-7) - Write/Read EISA DMA Channel High Count Register
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
DMAEISACnt0:
	push	si
	lea	si,[DMARegSav].Chnl0
	jmp	SHORT DMAEISACnt

DMAEISACnt1:
	push	si
	lea	si,[DMARegSav].Chnl1
	jmp	SHORT DMAEISACnt

DMAEISACnt2:
	push	si
	lea	si,[DMARegSav].Chnl2
	jmp	SHORT DMAEISACnt

DMAEISACnt3:
	push	si
	lea	si,[DMARegSav].Chnl3
	jmp	SHORT DMAEISACnt

DMAEISACnt5:
	push	si
	lea	si,[DMARegSav].Chnl5
	jmp	SHORT DMAEISACnt

DMAEISACnt6:
	push	si
	lea	si,[DMARegSav].Chnl6
	jmp	SHORT DMAEISACnt

DMAEISACnt7:
	push	si
	lea	si,[DMARegSav].Chnl7
	jmp	SHORT DMAEISACnt

;==============================================================================
;==
;==  DMAEISAPg(0-7) - Write/Read EISA DMA Channel High Page Register
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
;==	AL = DMA address bits 24-31
;==
;==============================================================================
DMAEISAPg0:
	push	si
	lea	si,[DMARegSav].Chnl0
	jmp	short DMAEISAPg

DMAEISAPg1:
	push	si
	lea	si,[DMARegSav].Chnl1
	jmp	short DMAEISAPg

DMAEISAPg2:
	push	si
	lea	si,[DMARegSav].Chnl2
	jmp	short DMAEISAPg

DMAEISAPg3:
	push	si
	lea	si,[DMARegSav].Chnl3
	jmp	short DMAEISAPg

DMAEISAPg5:
	push	si
	lea	si,[DMARegSav].Chnl5
	jmp	short DMAEISAPg

DMAEISAPg6:
	push	si
	lea	si,[DMARegSav].Chnl6
	jmp	short DMAEISAPg

DMAEISAPg7:
	push	si
	lea	si,[DMARegSav].Chnl7
	jmp	short DMAEISAPg

;==============================================================================
;==
;==  DMAEISACnt - Write/Read EISA DMA Channel N High Count Register
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
;==	DMA_E_C0   equ	401h	; HiCount0 (Bits: <23.16>)
;==	DMA_E_C1   equ	403h	; HiCount1
;==	DMA_E_C2   equ	405h	; HiCount2
;==	DMA_E_C3   equ	401h	; HiCount3
;==	DMA_E_C5   equ	4C6h	; HiCount5
;==	DMA_E_C6   equ	4CAh	; HiCount6
;==	DMA_E_C7   equ	4CEh	; HiCount7
;==
;==============================================================================
DMAEISACnt:
	push	di
	push	eax
	push	ecx
	push	dx
	push	es

	push	ds
	pop	es				; ES = _DATA

	call	DMACheckTC

	or	dx,dx				;Q: Input ?
	jz	short DECNRead			; Y: do Read operation

	and	ax,0FFh
	or	[si].DMAChnFlgs,fExtPI+fDirtyCount ; EISA extended mode
	mov	word ptr [si][2].DMACount,ax
	jmp	DMATrapExit

DECNRead:
	test	[si].DMAChnFlgs,fDirtyCount	;Q: virtual => physical?
	jz	short DECNRphys			; Y: physical read
	mov	al,byte ptr [si].DMACount[2]	; N: virtual count
	jmp	DMATrapExit

DECNRphys:
	movzx	ecx,[si].DMAChnlNum	; get port # for EISA count register
	mov	dx,[DMA_EISA_HighCountPort][ecx*2]
	in	al,dx			; get byte from EISA count register
	jmp	DMATrapExit
;==============================================================================
;==
;==  DMAEISAPg - Write/Read EISA DMA Channel N High Page Register
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
;==  DMA_E_P0  equ   487h	; HiPage0 (Bits: <31.24>)
;==  DMA_E_P1  equ   483h	; HiPage1
;==  DMA_E_P2  equ   481h	; HiPage2
;==  DMA_E_P3  equ   482h	; HiPage3
;==  DMA_E_P5  equ   48Bh	; HiPage5
;==  DMA_E_P6  equ   489h	; HiPage6
;==  DMA_E_P7  equ   48Ah	; HiPage7
;==
;==============================================================================
DMAEISAPg:
	push	di
	push	eax
	push	ecx
	push	dx
	push	es

	push	ds
	pop	es			; ES = _DATA

	call	DMACheckTC

	or	dx,dx			;Q: Input ?
	jz	short DEPNRead		; Y: do Read operation

	or	[si].DMAChnFlgs,fExtPI+fDirtyAddr ; EISA extended mode
	mov	byte ptr [si][3].DMALinAdr,al
	jmp	DMATrapExit

DEPNRead:
	test	[si].DMAChnFlgs,fDirtyAddr	;Q: virtual => physical?
	jz	short DEPNRphys			; Y: physical read
	mov	al,byte ptr [si].DMALinAdr[3]   ; N: virtual high page
	jmp	DMATrapExit

DEPNRphys:
	movzx	ecx,[si].DMAChnlNum	; get port # for EISA high page register
	mov	dx,[DMA_EISA_HighPagePort][ecx*2]
	in	al,dx			; get byte from EISA count register
	sub	al,byte ptr [si].DMAPhyAdr[3] ; Y: get starting physical address
	add	al,byte ptr [si].DMALinAdr[3] ; get starting linear address
	jmp	DMATrapExit

_TEXT	ends

	end				; end of module
