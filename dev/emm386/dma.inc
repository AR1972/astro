ifndef INC_LIST
.xlist
endif
;=============================================================================
;==
;== (C) Copyright MICROSOFT Corp. 1989-1991
;== (C) Copyright COMPAQ Computer Corp. 1989-1991
;==
;==	Title:	EMM386.EXE - MICROSOFT Expanded Memory Manager 386 Driver
;==
;==	Module: DMA.INC  - Include for DMA support
;==
;==	Version: 1.00
;==
;==	Date:	September 9,1989
;==
;==	Author: Leo Cohen
;==
;=============================================================================
;==
;==	Change Log:
;==
;==	DATE	 	 REVISION	Description
;==	-------- --------	--------------------------------------------
;== 09/09/89 0.00	        Original
;==
;=============================================================================
;=============================================================================
;==  DMA related ports (ISA)
;=============================================================================
DMA_P0		equ	0087h	; DMA page register for Channel 0
DMA_P1		equ	0083h	; DMA page register for Channel 1
DMA_P2		equ	0081h	; DMA page register for Channel 2
DMA_P3		equ	0082h	; DMA page register for Channel 3
DMA_P5		equ	008Bh	; DMA page register for Channel 5
DMA_P6		equ	0089h	; DMA page register for Channel 6
DMA_P7		equ	008Ah	; DMA page register for Channel 7

DMA_B0		equ	0000h	; DMA base register for Channel 0
DMA_C0		equ	0001h	; DMA count register for Channel 0
DMA_B1		equ	0002h	; DMA base register for Channel 1
DMA_C1		equ	0003h	; DMA count register for Channel 1
DMA_B2		equ	0004h	; DMA base register for Channel 2
DMA_C2		equ	0005h	; DMA count register for Channel 2
DMA_B3		equ	0006h	; DMA base register for Channel 3
DMA_C3		equ	0007h	; DMA count register for Channel 3
DMA_B4		equ	00C0h	; DMA base register for Channel 4
DMA_C4		equ	00C2h	; DMA count register for Channel 4
DMA_B5		equ	00C4h	; DMA base register for Channel 5
DMA_C5		equ	00C6h	; DMA count register for Channel 5
DMA_B6		equ	00C8h	; DMA base register for Channel 6
DMA_C6		equ	00CAh	; DMA count register for Channel 6
DMA_B7		equ	00CCh	; DMA base register for Channel 7
DMA_C7		equ	00CEh	; DMA count register for Channel 7

DMA_CLR_FF1	equ	000Ch	; clear flip-flop cmd for channels 0-3
DMA_CLR_FF2	equ	00D8h	; clear flip-flop cmd for channels 5-7

;==============================================================================
;==
;== Status Register: (READ) 8237A Programmable DMA Controller
;==
;== 76543210
;== xxxxxxxx---> Channel 0 has reached TC
;== |||||||----> Channel 1 has reached TC
;== ||||||-----> Channel 2 has reached TC
;== |||||------> Channel 3 has reached TC
;== ||||-------> Channel 0 request
;== |||--------> Channel 1 request
;== ||---------> Channel 2 request
;== |----------> Channel 3 request
;==
;== Command Register: (WRITE) 8237A Programmable DMA Controller
;==
;== 76543210
;== xxxxxxxx---> Memory-to-Memory enable
;== |||||||----> Channel 0 address hold enable (don't care if bit 0 is reset)
;== ||||||-----> Controller Disable
;== |||||------> Compressed Timing (don't care if bit 0 is set)
;== ||||-------> Rotating Priority
;== |||--------> Extended Write Selection (don't care if bit 3 is set)
;== ||---------> DREQ Sense Active Low
;== |----------> DACK Sense Active High
;==
;==============================================================================
DMA_STAT1	equ	008h	; Status 1       	008h
DMA_STAT2	equ	0D0h	; Status 2       	0D0h
;==============================================================================
;==
;== Request Register: (WRITE) 8237A Programmable DMA Controller
;==
;== 76543210
;== xxxxxxxx
;== ||||||||---> Channel 0-3 selected
;== ||||||-----> Set Request Bit
;== |||||------> don't care
;==
;==============================================================================
DMA_REQUEST1	equ	009h	; Softreq 1      	009h
DMA_REQUEST2	equ	0D2h	; Softreq 2      	0D2h
;==============================================================================
;==
;== Single Mask Register: (WRITE) 8237A Programmable DMA Controller
;==
;== 76543210
;== xxxxxxxx
;== ||||||||---> Channel 0-3 selected
;== ||||||-----> Set Mask Bit
;== |||||------> don't care
;==
;==============================================================================
DMA_SINMASK1	equ	00Ah	; Single Mask 1  	00Ah
DMA_SINMASK2	equ	0D4h	; Single Mask 2  	0D4h
;==============================================================================
;==
;== Mode Register: (WRITE) 8237A Programmable DMA Controller
;==
;== 76543210
;== xxxxxxxx
;== ||||||||---> Channel 0-3 selected
;== ||||||-----> 0-Verify, 1-Write, 2- Read, 3-illegal, X- if bits 6 & 7 are set
;== ||||
;== ||||-------> Enable Autoinitaialize
;== |||--------> Address Decrement
;== ||---------> 0-Demand Mode, 1-Single Mode, 2-Block Mode, 3-Cascade Mode
;==
;==============================================================================
DMA_MODE1	equ	00Bh	; Mode 1         	00Bh
DMA_MODE2	equ	0D6h	; Mode 2         	0D6h

DMA_RESET1	equ	00Dh	; Reset 1        	00Dh
DMA_RESET2	equ	0DAh	; Reset 2        	0DAh
DMA_RESMASK1	equ	00Eh	; Reset Mask 1  	00Eh
DMA_RESMASK2	equ	0DCh	; Reset Mask 2		0DCh
;==============================================================================
;==
;== Mask Register: (WRITE) 8237A Programmable DMA Controller
;==
;== 76543210
;== xxxxxxxx
;== ||||||||---> Mask Channel 0
;== ||||||-----> Mask Channel 1
;== |||||------> Mask Channel 2
;==	||||-------> Mask Channel 3
;== |||--------> don't care
;==
;==============================================================================
DMA_MASK1	equ	00Fh	; Mask 1		00Fh
DMA_MASK2	equ	0DEh	; Mask 2		0DEh

;==============================================================================
;==  In addition for EISA:
;==============================================================================
;==============================================================================
;==
;== Channel Interrupt Status Register: (READ) (40Ah)
;==
;== 76543210
;== xxxxxxxx---> Interrupt on channel 0
;== |||||||----> Interrupt on channel 1
;== ||||||-----> Interrupt on channel 2
;== |||||------> Interrupt on channel 3
;== ||||-------> reserved
;== |||--------> Interrupt on channel 5
;== ||---------> Interrupt on channel 6
;== |----------> Interrupt on channel 7
;==
;== Chaining Mode Status Register: (READ) (4D4h)
;==
;== 76543210
;== xxxxxxxx---> Channel 0 enabled
;== |||||||----> Channel 1 enabled
;== ||||||-----> Channel 2 enabled
;== |||||------> Channel 3 enabled
;== ||||-------> reserved
;== |||--------> Channel 5 enabled
;== ||---------> Channel 6 enabled
;== |----------> Channel 7 enabled
;==
;== Chaining Mode Register: (WRITE) (40Ah & 4D4h)
;==
;== 76543210
;== xxxxxxxx
;== ||||||||---> Channel 0-3 selected
;== ||||||-----> 0-Disable, 1-Enable, 2-illegal, 3-Programming complete
;== ||||
;== ||||-------> reserved
;==
;==============================================================================
DMA_E_IS	equ	40Ah 	; Channel interrupt status register (read only)
DMA_E_CS	equ	4D4h 	; Chaining mode status register (read only)

DMA_E_CH1	equ	40Ah	; ChainMode for channels 0-3 (write only)
DMA_E_CH2	equ	4D4h	; ChainMode for channels 5-7 (write only)

;==============================================================================
;==
;== DMA Extended Mode Register: (WRITE) (40Bh & 4D6h)
;==
;== 76543210
;== xxxxxxxx
;== ||||||||---> Channel 0-3 selected
;== ||||||-----> 0-8/byte, 1-16/word, 2-32/byte, 3-16/byte
;== ||||-------> 0-ISA, 1-"A", 2-"B", 3-"C" (Burst)
;== ||---------> T-C is an input for this channel
;==	|----------> Stop register disabled
;==
;==============================================================================
DMA_E_EM1	equ	40Bh	; Extended Mode for channels 0-3
DMA_E_EM2	equ	4D6h	; Extended Mode for channels 5-7

DMA_E_P0	equ	487h	; HiPage0 (Bits: <31.24>)
DMA_E_P1	equ	483h	; HiPage1
DMA_E_P2	equ	481h	; HiPage2
DMA_E_P3	equ	482h	; HiPage3
DMA_E_P5	equ	48Bh	; HiPage5
DMA_E_P6	equ	489h	; HiPage6
DMA_E_P7	equ	48Ah	; HiPage7

DMA_E_C0	equ	401h	; HiCount0 (Bits: <23.16>)
DMA_E_C1	equ	403h	; HiCount1
DMA_E_C2	equ	405h	; HiCount2
DMA_E_C3	equ	407h	; HiCount3
DMA_E_C5	equ	4C6h	; HiCount5
DMA_E_C6	equ	4CAh	; HiCount6
DMA_E_C7	equ	4CEh	; HiCount7

DMA_E_S00	equ	4E0h	; Stop channel 0 (Bits: <7:2> )
DMA_E_S01	equ	4E1h	; Stop channel 0 (Bits: <15.8> )
DMA_E_S02	equ	4E2h	; Stop channel 0 (Bits: <23.16> )
DMA_E_S10	equ	4E4h	; Stop channel 1 (Bits: <7:2> )
DMA_E_S11	equ	4E5h	; Stop channel 1 (Bits: <15.8> )
DMA_E_S12	equ	4E6h	; Stop channel 1 (Bits: <23.16> )
DMA_E_S20	equ	4E8h	; Stop channel 2 (Bits: <7:2> )
DMA_E_S21	equ	4E9h	; Stop channel 2 (Bits: <15.8> )
DMA_E_S22	equ	4EAh	; Stop channel 2 (Bits: <23.16> )
DMA_E_S30	equ	4ECh	; Stop channel 3 (Bits: <7:2> )
DMA_E_S31	equ	4EDh	; Stop channel 3 (Bits: <15.8> )
DMA_E_S32	equ	4EEh	; Stop channel 3 (Bits: <23.16> )
DMA_E_S50	equ	4F4h	; Stop channel 5 (Bits: <7:2> )
DMA_E_S51	equ	4F5h	; Stop channel 5 (Bits: <15.8> )
DMA_E_S52	equ	4F6h	; Stop channel 5 (Bits: <23.16> )
DMA_E_S60	equ	4F8h	; Stop channel 6 (Bits: <7:2> )
DMA_E_S61	equ	4F9h	; Stop channel 6 (Bits: <15.8> )
DMA_E_S62	equ	4FAh	; Stop channel 6 (Bits: <23.16> )
DMA_E_S70	equ	4FCh	; Stop channel 7 (Bits: <7:2> )
DMA_E_S71	equ	4FDh	; Stop channel 7 (Bits: <15.8> )
DMA_E_S72	equ	4FEh	; Stop channel 7 (Bits: <23.16> )

;==
;==	  ChainBuffExp	40Ch         ** For DMA1 & DMA2  ?
;==   DMA_E_IS	40Ah : Channel interrupt status register (read only)
;==   DMA_E_CS	4D4h : Chaining mode status register (read only)
;==

;=============================================================================
;==  DMA Register Save Structure
;=============================================================================
Chnl0Num	equ	0	; Address offset in Address tables
Chnl1Num	equ	1	; Address offset in Address tables
Chnl2Num	equ	2	; Address offset in Address tables
Chnl3Num	equ	3	; Address offset in Address tables
Chnl4Num	equ	4	; Address offset in Address tables
Chnl5Num	equ	5	; Address offset in Address tables
Chnl6Num	equ	6	; Address offset in Address tables
Chnl7Num	equ	7	; Address offset in Address tables

DMARegRec	struc
  DMALinAdr	dd	0	; Channel Linear Base Register
  DMAPhyAdr	dd	0	; Channel Physical Base Register
  DMACount	dd	0	; Channel Count Register
  DMAChnFlgs	dw	0	; Channel Flags
  DMAChnlNum	dw	?	; Channel identifier index
DMARegRec	ends
;=============================================================================
;==  DMAChnlFlgs uses the following bits:
;=============================================================================
fChnlActive	equ 000000000001b ; Operation in progress on this channel
fDMABuffer	equ 000000000010b ; DMA buffer being used by this channel
fNoTrans	equ 000000000100b ; No linear to physical translation needed
fReadOp		equ 000000001000b ; Read operation
fWriteOp	equ 000000010000b ; Write operation
fDecMode	equ 000000100000b ; Channel is in decrement mode
fAutoInit	equ 000001000000b ; Channel is in auto initialize mode
fWordTx		equ 000010000000b ; Channel programmed for word transfers
fExtPI		equ 000100000000b ; Channel programmed via extended PI
fDirtyAddr	equ 001000000000b ; Virtual & physical address are not equal
fDirtyCount	equ 010000000000b ; Virtual & physical count are not the same
fCascadeOp	equ 100000000000b ; Cascade mode

fChnlActiveBit	equ	0 				; bit numbers for BT instruction
fDMABufferBit	equ	1
fNoTransBit	equ	2
fReadOpBit 	equ 	3
fWriteOpBit	equ 	4
fDecModeBit	equ	5
fAutoInitBit	equ	6
fWordTxBit	equ	7
fExtPIBit	equ	8
fDirtyAddrBit	equ	9
fDirtyCountBit	equ	10
fCascadeOpBit	equ	11
;=============================================================================
;==	 _DATA:[DMAFlags]: General flags
;=============================================================================
fDMABufferBusy	equ 00000001b	; DMA buffer is in use
fDMASystem	equ 00000010b	; DMA buffer is in use by system DMA
fDMAFormat	equ 00000100b	; Floppy format will be next DMA operation
fDMABuffXT	equ 00001000b	; DMA buffer is below 1 Meg

fDMABufferBusyBit equ	0	; bit numbers for BTx instructions
fDMASystemBit	  equ	1
fDMAFormatBit	  equ	2
fDMABuffXTBit	  equ	3

;=============================================================================
;==  Assume 18 sectors per track *QLEO: How about future floppy media?
;==  Need to find out largest amount of sectors/track of any floppy!
;=============================================================================
ORG_FORMAT_BUFFER_SIZE equ 10000h; ROM uses this size buffer for floppy format

DMARegBuf	struc
  Chnl0		db	(size DMARegRec - 2) dup (00), Chnl0Num, 0
  Chnl1		db	(size DMARegRec - 2) dup (00), Chnl1Num, 0
  Chnl2		db	(size DMARegRec - 2) dup (00), Chnl2Num, 0
  Chnl3		db	(size DMARegRec - 2) dup (00), Chnl3Num, 0
  Chnl4		db	(size DMARegRec - 2) dup (00), Chnl4Num, 0
  Chnl5		db	(size DMARegRec - 2) dup (00), Chnl5Num, 0
  Chnl6		db	(size DMARegRec - 2) dup (00), Chnl6Num, 0
  Chnl7		db	(size DMARegRec - 2) dup (00), Chnl7Num, 0
  DMAStatus	db	0	; Controller 1 Virtual Status Register
		db	0	; Controller 2 Virtual Status Register
  DMAFF		db	0	; Controller 1 FlipFlop State
		db	0	; Controller 2 FlipFlop State
  DMACtlFlgs	db  	0	; Controller 1 Flags
		db  	0	; Controller 2 Flags
; additional data for PS2 support. HKN
  DMA_PS2_cmd	db  	0	; last commmand byte
  DMA_writing	db  	0	; 0 - reading, 1 - writing port 1Ah
  DMA_bytePtr	db  	0	; byte position in read or write
  DMA_dataBytes	db  	0	; # of bytes readable or writable
  DMA_data	dd  	0	; if reading, then bytes that can be read
DMARegBuf	ends
;=============================================================================
;==  DMACtlFlgs: Controller flags
;=============================================================================
fDisabled	equ 00000001b	; controller is disabled
;=============================================================================
;==  DMA/bus master DMA version, OEM number, and DMA buffer ID
;=============================================================================
DBSMajVer	equ	1	; major version
DBSMinVer	equ	0	; minor version
DBSNumOEM	equ	1	; OEM number   (QLEO: ?????)
DBSRevOEM	equ	1	; OEM revision (QLEO: ?????)
DBSDMAXT 	equ 0010b	; DMA buffer below 1 megabyte
;=============================================================================
;==  DMA/bus master service flags
;=============================================================================
fDBSRSVD	equ 00000001b	; Reserved
fDBSBufferCopy  equ 00000010b	; automatic copy to/from bufffer
fDBSDisAutoBuff	equ 00000100b	; disable auto buffer allocation
fDBSDisAutoRem	equ 00001000b	; disable auto remap
fDBS64kAlign	equ 00010000b	; region must not cross a 64K boundary
fDBS128kAlign	equ 00100000b	; region must not cross a 128K boundary
fDBSPTEs	equ 01000000b	; copy PTE's for scatter/gather remap
fDBSNotPresent	equ 10000000b	; not present pages should not be locked
;=============================================================================
;==  DMA/bus master error codes
;=============================================================================
eDBSNotContig	equ	01h	; region not contiguous
eDBSBoundCross	equ	02h	; region crossed a boundary specified by flag
eDBSUnLockPage	equ	03h	; unable to lock pages
eDBSNoBuffer	equ	04h	; no buffer available
eDBSBufferSize	equ	05h	; size of buffer is too small
eDBSBufferBusy	equ	06h	; buffer is currently in use
eDBSInvalidMem	equ	07h	; invalid memory region
eDBSLockRegion	equ	08h	; region was not locked
eDBSTableSize	equ	09h	;	number of physical pages was greater than table length
eDBSInvalidBuff	equ	0Ah	; invalid buffer ID
eDBSBuffRange	equ	0Bh	; copy out of buffer range
eDBSInvalidDMA	equ	0Ch	; invalid DMA channel number
eDBSCountOver	equ	0Dh	; disable count overflow
eDBSCountUnder	equ	0Eh	; disable count underflow
eDBSInvalidFunc	equ	0Fh	; Function not supported
eDBSInvalidFlag	equ	10h	; Invalid flags requested
;=============================================================================
;==  DMA/bus master DMA descriptor structure (DDS)
;=============================================================================
sDDSDescriptor	struc
  DDSsize	dd	0	; region size
  DDSLinOffs	dd	0	; linear offset
  DDSseg	dw	0	; segment
  DDSBuffID	dw	0	; buffer ID (only one DMA buffer used in CEMM)
  DDSPhyAddr	dd	0	; physical address of region
sDDSDescriptor	ends
;=============================================================================
;==  DMA/bus master extended DDS
;=============================================================================
sDDSExtended	struc
  DDSEsize	dd	0	; region size
  DDSELinOffs	dd	0	; linear offset
  DDSEseg	dw	0	; segment
  DDSRSVD	dw	0	; reserved for future use
  DDSEAvail	dw	0	; available PTE/region entries
  DDSEUsed	dw	0	; used PTE/region entries
  DDSElist	dd	0	; first entry in PTE/region list
sDDSExtended	ends

sDDSEPTE	struc
  DDSEP		dd	0	; PTE entry
sDDSEPTE	ends

sDDSERegion		struc
  DDSERPhyAddr	dd	0	; physical address of region
  DDSERsize	dd	0	; size of contiguous region
sDDSERegion		ends
;=============================================================================
;==  Intel Above Brd / AT ports   as of 4/9/86 board !!!
;=============================================================================
IAB_P0		equ	0258h	; page 0 mapping register
IAB_P1		equ	4258h	; page 1 mapping register
IAB_P2		equ	8258h	; page 2 mapping register
IAB_P3		equ	0C258h	; page 3 mapping regester
IAB_PB0 	equ	2258h	; page 0 map reg for piggyback brd
IAB_PB1 	equ	6258h	; page 1 map reg for piggyback brd
IAB_PB2 	equ	0A258h	; page 2 map reg for piggyback brd
IAB_PB3 	equ	0E258h	; page 3 map reg for pibbyback brd
IAB_C0		equ	0259h	; control register
IAB_C1		equ	4259h	; control register
IAB_C2		equ	8259h 	; control register
IAB_C3		equ	0C259h	; control register

;##########################################################################
;
;equates and data for ps2 support. HKN
;
TRUE			equ	1
FALSE			equ	0
PS2_CHANNEL_MASK	equ	00000007h
PS2_FUNCTION_MASK	equ	000000f0h

;
;	extended mode bit
;
Transfer_Data	equ	00000100b
Write_Mem	equ	00001000b
_16_bit_xfer	equ	01000000b


;
;   PS/2 specific ports
;
DMA_XFN 	equ	0018h
DMA_EXE 	equ	001Ah

Set_IO_Adr	equ	00h
Set_Mem_Adr	equ	20h
Get_Mem_Adr	equ	30h
Set_Count	equ	40h
Get_Count	equ	50h
Get_Status	equ	60h
Set_Mode	equ	70h
Set_Arbus	equ	80h
Set_Chn_Mask	equ	90h
Reset_Chn_Mask	equ 	0A0h
Master_Clear	equ 0D0h

.list	; end of DMA.inc

