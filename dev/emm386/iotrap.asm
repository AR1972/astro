.386p
page	58,132
;******************************************************************************
	title	IOTRAP.ASM - Dispatches I/O trap handlers
;******************************************************************************
;
;   (C) Copyright MICROSOFT Corp. 1986-1991
;   (C) Copyright COMPAQ Computer Corp. 1986-1991
;
;   Title:    EMM386.EXE - MICROSOFT Expanded Memory Manager 386 Driver
;
;   Module:   IOTRAP.ASM - Dispatches I/O trap handlers
;
;   Version:  2.00
;
;   Date:     July 1, 1986
;
;   Author:   Steve Preston
;
;******************************************************************************
;
;   Change log:
;
;     DATE    REVISION			DESCRIPTION
;   --------  --------	-------------------------------------------------------
;   07/01/86  0.03	From ELIMTRAP.ASM (SBP).
;   07/03/86  0.03	Added handlers for 84,85,60, & 64 (SBP).
;   07/06/86  0.04	Changed assume to _DATA (SBP).
;   07/02/87  2.00	Add DMA channel 0 support (SBP).
;   07/13/88  3.31 (*C) Change handling of port 92h for 386s (RDV).
;   01/15/89  4.00 (*D) Generic 8042 detect added (RDV)
;
;   02/21/91  M011	Do not simulate IO address wrap
;
;******************************************************************************
;
;   Functional Description:
;
;	This routine is called by all I/O space trap handlers to allow
;   emulation/monitoring of I/O address reads and writes.  When a GP fault
;   occurs due to I/O to an address trapped in the I/O Bit Map, the I/O
;   instruction emulation routine in VMINST calls this routine.  This
;   routine calls the appropriate I/O trap handler for the I/O address.
;
;******************************************************************************
.lfcond 				; list false conditionals
	page
;******************************************************************************
;			P U B L I C   D E C L A R A T I O N S
;******************************************************************************
;
	public	IO_Trap 		; dispatches I/O trap handlers
	public	DMATable

	page
;******************************************************************************
;			L O C A L   C O N S T A N T S
;******************************************************************************
;
	include VDMseg.inc
	include VDMsel.inc
	include desc.inc
	include dma.inc
	include page.inc
	include oemdep.inc
	include vm386.inc
	include emmfunct.inc
	include emmdata.inc
	include emm386.inc
;
;******************************************************************************
;			E X T E R N A L   R E F E R E N C E S
;******************************************************************************
;

_TEXT	segment

;
;   Return to Real Handlers	(rrtrap.asm)
;
extrn	P84_Handler:near
extrn	P85_Handler:near

ifdef PICtrap
;
;  PIC trapping ports		(pictrap.asm)
;
extrn	PICCommand1:near
extrn	PICCommand2:near
extrn	PICData1:near
extrn	PICData2:near
endif

;
;   A20 watch handlers	(a20trap.asm)
;
extrn	P60_Handler:near	; Kybd Data port - A20 watch
extrn	P64_Handler:near	; Kybd Cmd port - A20 watch

;
;  LIM related handlers (elimtrap.asm)
;
extrn	DMABase0:near		;  DMA base register for Channel 0
extrn	DMABase1:near		;  DMA base register for Channel 1
extrn	DMABase2:near		;  DMA base register for Channel 2
extrn	DMABase3:near		;  DMA base register for Channel 3
extrn	DMABase5:near		;  DMA base register for Channel 5
extrn	DMABase6:near		;  DMA base register for Channel 6
extrn	DMABase7:near		;  DMA base register for Channel 7
extrn	DMACnt0:near		;  DMA count register for Channel 0
extrn	DMACnt1:near		;  DMA count register for Channel 1
extrn	DMACnt2:near		;  DMA count register for Channel 2
extrn	DMACnt3:near		;  DMA count register for Channel 3
extrn	DMACnt5:near		;  DMA count register for Channel 5
extrn	DMACnt6:near		;  DMA count register for Channel 6
extrn	DMACnt7:near		;  DMA count register for Channel 7
extrn	DMAEISACnt0:near	;  DMA EISA high count register for Channel 0
extrn	DMAEISACnt1:near	;  DMA EISA high count register for Channel 1
extrn	DMAEISACnt2:near	;  DMA EISA high count register for Channel 2
extrn	DMAEISACnt3:near	;  DMA EISA high count register for Channel 3
extrn	DMAEISACnt5:near	;  DMA EISA high count register for Channel 5
extrn	DMAEISACnt6:near	;  DMA EISA high count register for Channel 6
extrn	DMAEISACnt7:near	;  DMA EISA high count register for Channel 7
extrn	DMAPg0:near		;  DMA page register for Channel 0
extrn	DMAPg1:near		;  DMA page register for Channel 1
extrn	DMAPg2:near		;  DMA page register for Channel 2
extrn	DMAPg3:near		;  DMA page register for Channel 3
extrn	DMAPg5:near		;  DMA page register for Channel 5
extrn	DMAPg6:near		;  DMA page register for Channel 6
extrn	DMAPg7:near		;  DMA page register for Channel 7
extrn	DMAEISAPg0:near		;  DMA EISA high page register for Channel 0
extrn	DMAEISAPg1:near		;  DMA EISA high page register for Channel 1
extrn	DMAEISAPg2:near		;  DMA EISA high page register for Channel 2
extrn	DMAEISAPg3:near		;  DMA EISA high page register for Channel 3
extrn	DMAEISAPg5:near		;  DMA EISA high page register for Channel 5
extrn	DMAEISAPg6:near		;  DMA EISA high page register for Channel 6
extrn	DMAEISAPg7:near		;  DMA EISA high page register for Channel 7
extrn	DMARequest1:near	;  DMA S/W request for controller 1
extrn	DMARequest2:near	;  DMA S/W request for controller 2
extrn	DMAStat1:near  		;  DMA status for controller 1
extrn	DMASinMsk1:near		;  DMA Single Mask for controller 1
extrn	DMAMode1:near  		;  DMA Mode register for controller 1
extrn	DMAEISAExt1:near  	;  DMA EISA Extended Mode register/controller 1
extrn	DMAEISAChain1:near  	;  DMA EISA Chain Mode register for controller 1
extrn	DMAClrFF1:near 		;  DMA clear FF cmd for controller 1
extrn	DMAReset1:near 		;  DMA Reset for controller 1
extrn	DMAResMsk1:near		;  DMA Reset Mask for controller 1
extrn	DMAMask1:near  		;  DMA Mask for controller 1
extrn	DMAStat2:near  		;  DMA status for controller 2
extrn	DMASinMsk2:near		;  DMA Single Mask for controller 2
extrn	DMAMode2:near  		;  DMA Mode register for controller 2
extrn	DMAEISAExt2:near  	;  DMA EISA Extended Mode register/controller 2
extrn	DMAEISAChain2:near  	;  DMA EISA Chain Mode register for controller 2
extrn	DMAClrFF2:near 		;  DMA clear FF cmd for controller 2
extrn	DMAReset2:near 		;  DMA Reset for controller 2
extrn	DMAResMsk2:near		;  DMA Reset Mask for controller 2
extrn	DMAMask2:near  		;  DMA Mask for controller 2
extrn	DMA_FuncReg:near
extrn	DMA_ExecFunc:near
extrn	P92_Handler:near

_TEXT	ends

;******************************************************************************
;			S E G M E N T	D E F I N I T I O N
;******************************************************************************

;
;------------------------------------------------------------------------------
_TEXT	segment
	assume	cs:_TEXT, ds:_DATA, es:_DATA, gs:R_CODE, ss:_DATA
;
;   DMATable
;	One entry per port in the I/O space from 00h to FFh.
;
DMATable label	 word
	dw	offset _TEXT:DMABase0  ;  0 DMA base register for Channel 0
	dw	offset _TEXT:DMACnt0   ;  1 DMA count register for Channel 0
	dw	offset _TEXT:DMABase1  ;  2 DMA base register for Channel 1
	dw	offset _TEXT:DMACnt1   ;  3 DMA count register for Channel 1
	dw	offset _TEXT:DMABase2  ;  4 DMA base register for Channel 2
	dw	offset _TEXT:DMACnt2   ;  5 DMA count register for Channel 2
	dw	offset _TEXT:DMABase3  ;  6 DMA base register for Channel 3
	dw	offset _TEXT:DMACnt3   ;  7 DMA count register for Channel 3
	dw	offset _TEXT:DMAStat1  ;  8 DMA status for controller 1
	dw	offset _TEXT:DMARequest1; 9 DMA S/W request for controller 1
	dw	offset _TEXT:DMASinMsk1;  a DMA Single Mask for controller 1
	dw	offset _TEXT:DMAMode1  ;  b DMA Mode register for controller 1
	dw	offset _TEXT:DMAClrFF1 ;  c DMA clear FF cmd for controller 1
	dw	offset _TEXT:DMAReset1 ;  d DMA Reset for controller 1
	dw	offset _TEXT:DMAResMsk1;  e DMA Reset Mask for controller 1
	dw	offset _TEXT:DMAMask1  ;  f DMA Mask for controller 1
	dw	offset _TEXT:IOT_BadT  ; 10
	dw	offset _TEXT:IOT_BadT  ; 11
	dw	offset _TEXT:IOT_BadT  ; 12
	dw	offset _TEXT:IOT_BadT  ; 13
	dw	offset _TEXT:IOT_BadT  ; 14
	dw	offset _TEXT:IOT_BadT  ; 15
	dw	offset _TEXT:IOT_BadT  ; 16
	dw	offset _TEXT:IOT_BadT  ; 17
	dw	offset _TEXT:DMA_FuncReg ; 18
	dw	offset _TEXT:IOT_BadT  ; 19
	dw	offset _TEXT:DMA_ExecFunc; 1a
	dw	offset _TEXT:IOT_BadT  ; 1b
	dw	offset _TEXT:IOT_BadT  ; 1c
	dw	offset _TEXT:IOT_BadT  ; 1d
	dw	offset _TEXT:IOT_BadT  ; 1e
	dw	offset _TEXT:IOT_BadT  ; 1f
ifdef PICtrap
	dw	offset _TEXT:PICCommand1; 20 Master PIC (A0=0)
	dw	offset _TEXT:PICData1	; 21 Master PIC (A0=1)
else
	dw	offset _TEXT:IOT_BadT  ; 20
	dw	offset _TEXT:IOT_BadT  ; 21
endif
	dw	offset _TEXT:IOT_BadT  ; 22
	dw	offset _TEXT:IOT_BadT  ; 23
	dw	offset _TEXT:IOT_BadT  ; 24
	dw	offset _TEXT:IOT_BadT  ; 25
	dw	offset _TEXT:IOT_BadT  ; 26
	dw	offset _TEXT:IOT_BadT  ; 27
	dw	offset _TEXT:IOT_BadT  ; 28
	dw	offset _TEXT:IOT_BadT  ; 29
	dw	offset _TEXT:IOT_BadT  ; 2a
	dw	offset _TEXT:IOT_BadT  ; 2b
	dw	offset _TEXT:IOT_BadT  ; 2c
	dw	offset _TEXT:IOT_BadT  ; 2d
	dw	offset _TEXT:IOT_BadT  ; 2e
	dw	offset _TEXT:IOT_BadT  ; 2f
	dw	offset _TEXT:IOT_BadT  ; 30
	dw	offset _TEXT:IOT_BadT  ; 31
	dw	offset _TEXT:IOT_BadT  ; 32
	dw	offset _TEXT:IOT_BadT  ; 33
	dw	offset _TEXT:IOT_BadT  ; 34
	dw	offset _TEXT:IOT_BadT  ; 35
	dw	offset _TEXT:IOT_BadT  ; 36
	dw	offset _TEXT:IOT_BadT  ; 37
	dw	offset _TEXT:IOT_BadT  ; 38
	dw	offset _TEXT:IOT_BadT  ; 39
	dw	offset _TEXT:IOT_BadT  ; 3a
	dw	offset _TEXT:IOT_BadT  ; 3b
	dw	offset _TEXT:IOT_BadT  ; 3c
	dw	offset _TEXT:IOT_BadT  ; 3d
	dw	offset _TEXT:IOT_BadT  ; 3e
	dw	offset _TEXT:IOT_BadT  ; 3f
	dw	offset _TEXT:IOT_BadT  ; 40
	dw	offset _TEXT:IOT_BadT  ; 41
	dw	offset _TEXT:IOT_BadT  ; 42
	dw	offset _TEXT:IOT_BadT  ; 43
	dw	offset _TEXT:IOT_BadT  ; 44
	dw	offset _TEXT:IOT_BadT  ; 45
	dw	offset _TEXT:IOT_BadT  ; 46
	dw	offset _TEXT:IOT_BadT  ; 47
	dw	offset _TEXT:IOT_BadT  ; 48
	dw	offset _TEXT:IOT_BadT  ; 49
	dw	offset _TEXT:IOT_BadT  ; 4a
	dw	offset _TEXT:IOT_BadT  ; 4b
	dw	offset _TEXT:IOT_BadT  ; 4c
	dw	offset _TEXT:IOT_BadT  ; 4d
	dw	offset _TEXT:IOT_BadT  ; 4e
	dw	offset _TEXT:IOT_BadT  ; 4f
	dw	offset _TEXT:IOT_BadT  ; 50
	dw	offset _TEXT:IOT_BadT  ; 51
	dw	offset _TEXT:IOT_BadT  ; 52
	dw	offset _TEXT:IOT_BadT  ; 53
	dw	offset _TEXT:IOT_BadT  ; 54
	dw	offset _TEXT:IOT_BadT  ; 55
	dw	offset _TEXT:IOT_BadT  ; 56
	dw	offset _TEXT:IOT_BadT  ; 57
	dw	offset _TEXT:IOT_BadT  ; 58
	dw	offset _TEXT:IOT_BadT  ; 59
	dw	offset _TEXT:IOT_BadT  ; 5a
	dw	offset _TEXT:IOT_BadT  ; 5b
	dw	offset _TEXT:IOT_BadT  ; 5c
	dw	offset _TEXT:IOT_BadT  ; 5d
	dw	offset _TEXT:IOT_BadT  ; 5e
	dw	offset _TEXT:IOT_BadT  ; 5f
	dw	offset _TEXT:P60_Handler	; A20 watch on kybd data port
	dw	offset _TEXT:IOT_BadT  ; 61
	dw	offset _TEXT:IOT_BadT  ; 62
	dw	offset _TEXT:IOT_BadT  ; 63
	dw	offset _TEXT:P64_Handler	; A20 watch on kybd cmd port
	dw	offset _TEXT:IOT_BadT  ; 65
	dw	offset _TEXT:IOT_BadT  ; 66
	dw	offset _TEXT:IOT_BadT  ; 67
	dw	offset _TEXT:IOT_BadT  ; 68
	dw	offset _TEXT:IOT_BadT  ; 69
	dw	offset _TEXT:IOT_BadT  ; 6a
	dw	offset _TEXT:IOT_BadT  ; 6b
	dw	offset _TEXT:IOT_BadT  ; 6c
	dw	offset _TEXT:IOT_BadT  ; 6d
	dw	offset _TEXT:IOT_BadT  ; 6e
	dw	offset _TEXT:IOT_BadT  ; 6f
	dw	offset _TEXT:IOT_BadT  ; 70
	dw	offset _TEXT:IOT_BadT  ; 71
	dw	offset _TEXT:IOT_BadT  ; 72
	dw	offset _TEXT:IOT_BadT  ; 73
	dw	offset _TEXT:IOT_BadT  ; 74
	dw	offset _TEXT:IOT_BadT  ; 75
	dw	offset _TEXT:IOT_BadT  ; 76
	dw	offset _TEXT:IOT_BadT  ; 77
	dw	offset _TEXT:IOT_BadT  ; 78
	dw	offset _TEXT:IOT_BadT  ; 79
	dw	offset _TEXT:IOT_BadT  ; 7a
	dw	offset _TEXT:IOT_BadT  ; 7b
	dw	offset _TEXT:IOT_BadT  ; 7c
	dw	offset _TEXT:IOT_BadT  ; 7d
	dw	offset _TEXT:IOT_BadT  ; 7e
	dw	offset _TEXT:IOT_BadT  ; 7f
	dw	offset _TEXT:IOT_BadT  ; 80
	dw	offset _TEXT:DMAPg2    ; 81 DMA page register for Channel 2
	dw	offset _TEXT:DMAPg3    ; 82 DMA page register for Channel 3
	dw	offset _TEXT:DMAPg1    ; 83 DMA page register for Channel 1
	dw	offset _TEXT:P84_Handler	; return to real port
	dw	offset _TEXT:P85_Handler	; return to real port
	dw	offset _TEXT:IOT_BadT  ; 86
	dw	offset _TEXT:DMAPg0    ; 87 DMA page register for Channel 0
	dw	offset _TEXT:IOT_BadT  ; 88
	dw	offset _TEXT:DMAPg6    ; 89 DMA page register for Channel 6
	dw	offset _TEXT:DMAPg7    ; 8a DMA page register for Channel 7
	dw	offset _TEXT:DMAPg5    ; 8b DMA page register for Channel 5
	dw	offset _TEXT:IOT_BadT  ; 8c
	dw	offset _TEXT:IOT_BadT  ; 8d
	dw	offset _TEXT:IOT_BadT  ; 8e
	dw	offset _TEXT:IOT_BadT  ; 8f
	dw	offset _TEXT:IOT_BadT  ; 90
	dw	offset _TEXT:DMAPg2    ; 91 DMA page register for Channel 2
	dw	offset _TEXT:IOT_Chk92 ; 92 real 92/DMA pg 3/or A20 for PS2
	dw	offset _TEXT:DMAPg1    ; 93 DMA page register for Channel 1
	dw	offset _TEXT:IOT_BadT  ; 94
	dw	offset _TEXT:IOT_BadT  ; 95
	dw	offset _TEXT:IOT_BadT  ; 96
	dw	offset _TEXT:DMAPg0    ; 97 DMA page register for Channel 0
	dw	offset _TEXT:IOT_BadT  ; 98
	dw	offset _TEXT:DMAPg6    ; 99 DMA page register for Channel 6
	dw	offset _TEXT:DMAPg7    ; 9a DMA page register for Channel 7
	dw	offset _TEXT:DMAPg5    ; 9b DMA page register for Channel 5
	dw	offset _TEXT:IOT_BadT  ; 9c
	dw	offset _TEXT:IOT_BadT  ; 9d
	dw	offset _TEXT:IOT_BadT  ; 9e
	dw	offset _TEXT:IOT_BadT  ; 9f
ifdef PICtrap
	dw	offset _TEXT:PICCommand2; a0 Slave PIC (A0=0)
	dw	offset _TEXT:PICData2	; a1 Slave PIC (A0=1)
else
	dw	offset _TEXT:IOT_BadT  ; a0
	dw	offset _TEXT:IOT_BadT  ; a1
endif
	dw	offset _TEXT:IOT_BadT  ; a2
	dw	offset _TEXT:IOT_BadT  ; a3
	dw	offset _TEXT:IOT_BadT  ; a4
	dw	offset _TEXT:IOT_BadT  ; a5
	dw	offset _TEXT:IOT_BadT  ; a6
	dw	offset _TEXT:IOT_BadT  ; a7
	dw	offset _TEXT:IOT_BadT  ; a8
	dw	offset _TEXT:IOT_BadT  ; a9
	dw	offset _TEXT:IOT_BadT  ; aa
	dw	offset _TEXT:IOT_BadT  ; ab
	dw	offset _TEXT:IOT_BadT  ; ac
	dw	offset _TEXT:IOT_BadT  ; ad
	dw	offset _TEXT:IOT_BadT  ; ae
	dw	offset _TEXT:IOT_BadT  ; af
	dw	offset _TEXT:IOT_BadT  ; b0
	dw	offset _TEXT:IOT_BadT  ; b1
	dw	offset _TEXT:IOT_BadT  ; b2
	dw	offset _TEXT:IOT_BadT  ; b3
	dw	offset _TEXT:IOT_BadT  ; b4
	dw	offset _TEXT:IOT_BadT  ; b5
	dw	offset _TEXT:IOT_BadT  ; b6
	dw	offset _TEXT:IOT_BadT  ; b7
	dw	offset _TEXT:IOT_BadT  ; b8
	dw	offset _TEXT:IOT_BadT  ; b9
	dw	offset _TEXT:IOT_BadT  ; ba
	dw	offset _TEXT:IOT_BadT  ; bb
	dw	offset _TEXT:IOT_BadT  ; bc
	dw	offset _TEXT:IOT_BadT  ; bd
	dw	offset _TEXT:IOT_BadT  ; be
	dw	offset _TEXT:IOT_BadT  ; bf
	dw	offset _TEXT:IOT_BadT  ; c0 DMA base register for Channel 4
	dw	offset _TEXT:IOT_BadT  ; c1
	dw	offset _TEXT:IOT_BadT  ; c2 DMA count register for Channel 4
	dw	offset _TEXT:IOT_BadT  ; c3
	dw	offset _TEXT:DMABase5  ; c4 DMA base register for Channel 5
	dw	offset _TEXT:IOT_BadT  ; c5
	dw	offset _TEXT:DMACnt5   ; c6 DMA count register for Channel 5
	dw	offset _TEXT:IOT_BadT  ; c7
	dw	offset _TEXT:DMABase6  ; c8 DMA base register for Channel 6
	dw	offset _TEXT:IOT_BadT  ; c9
	dw	offset _TEXT:DMACnt6   ; ca DMA count register for Channel 6
	dw	offset _TEXT:IOT_BadT  ; cb
	dw	offset _TEXT:DMABase7  ; cc DMA base register for Channel 7
	dw	offset _TEXT:IOT_BadT  ; cd
	dw	offset _TEXT:DMACnt7   ; ce DMA count register for Channel 7
	dw	offset _TEXT:IOT_BadT  ; cf
	dw	offset _TEXT:DMAStat2  ; d0 DMA Status for controller 2
	dw	offset _TEXT:IOT_BadT  ; d1
	dw	offset _TEXT:DMARequest2;d2 DMA S/W request for controller 2
	dw	offset _TEXT:IOT_BadT  ; d3
	dw	offset _TEXT:DMASinMsk2; d4 DMA Single Mask for controller 2
	dw	offset _TEXT:IOT_BadT  ; d5
	dw	offset _TEXT:DMAMode2  ; d6 DMA Mode for controller 2
	dw	offset _TEXT:IOT_BadT  ; d7
	dw	offset _TEXT:DMAClrFF2 ; d8 DMA clear FF cmd for controller 2
	dw	offset _TEXT:IOT_BadT  ; d9
	dw	offset _TEXT:DMAReset2 ; da DMA Reset for controller 2
	dw	offset _TEXT:IOT_BadT  ; db
	dw	offset _TEXT:DMAResMsk2; dc DMA Reset Mask for controller 2
	dw	offset _TEXT:IOT_BadT  ; dd
	dw	offset _TEXT:DMAMask2  ; de DMA Mask for controller 2
	dw	offset _TEXT:IOT_BadT  ; df
	dw	offset _TEXT:IOT_BadT  ; e0
	dw	offset _TEXT:IOT_BadT  ; e1
	dw	offset _TEXT:IOT_BadT  ; e2
	dw	offset _TEXT:IOT_BadT  ; e3
	dw	offset _TEXT:IOT_BadT  ; e4
	dw	offset _TEXT:IOT_BadT  ; e5
	dw	offset _TEXT:IOT_BadT  ; e6
	dw	offset _TEXT:IOT_BadT  ; e7
	dw	offset _TEXT:IOT_BadT  ; e8
	dw	offset _TEXT:IOT_BadT  ; e9
	dw	offset _TEXT:IOT_BadT  ; ea
	dw	offset _TEXT:IOT_BadT  ; eb
	dw	offset _TEXT:IOT_BadT  ; ec
	dw	offset _TEXT:IOT_BadT  ; ed
	dw	offset _TEXT:IOT_BadT  ; ee
	dw	offset _TEXT:IOT_BadT  ; ef
	dw	offset _TEXT:IOT_BadT  ; f0
	dw	offset _TEXT:IOT_BadT  ; f1
	dw	offset _TEXT:IOT_BadT  ; f2
	dw	offset _TEXT:IOT_BadT  ; f3
	dw	offset _TEXT:IOT_BadT  ; f4
	dw	offset _TEXT:IOT_BadT  ; f5
	dw	offset _TEXT:IOT_BadT  ; f6
	dw	offset _TEXT:IOT_BadT  ; f7
	dw	offset _TEXT:IOT_BadT  ; f8
	dw	offset _TEXT:IOT_BadT  ; f9
	dw	offset _TEXT:IOT_BadT  ; fa
	dw	offset _TEXT:IOT_BadT  ; fb
	dw	offset _TEXT:IOT_BadT  ; fc
	dw	offset _TEXT:IOT_BadT  ; fd
	dw	offset _TEXT:IOT_BadT  ; fe
	dw	offset _TEXT:IOT_BadT  ; ff

;
;   DMAEISATable
;	One entry per port in the I/O space from 400h to 4FFh.
;
DMAEISATable label	 word

 dw DMA_E_CH1,offset _TEXT:DMAEISAChain1 ;40A EISA DMA Chain/controller 1
 dw DMA_E_CH2,offset _TEXT:DMAEISAChain2 ;4D4 EISA DMA Chain/controller 2
 dw DMA_E_EM1,offset _TEXT:DMAEISAExt1   ;40B EISA DMA Extended/controller 1
 dw DMA_E_EM2,offset _TEXT:DMAEISAExt2   ;4D6 EISA DMA Extended/controller 2

 dw DMA_E_C0,offset _TEXT:DMAEISACnt0 ;401 EISA DMA high count/Channel 0
 dw DMA_E_C1,offset _TEXT:DMAEISACnt1 ;403 EISA DMA high count/Channel 1
 dw DMA_E_C2,offset _TEXT:DMAEISACnt2 ;405 EISA DMA high count/Channel 2
 dw DMA_E_C3,offset _TEXT:DMAEISACnt3 ;407 EISA DMA high count/Channel 3
 dw DMA_E_C5,offset _TEXT:DMAEISACnt5 ;4C6 EISA DMA high count/Channel 5
 dw DMA_E_C6,offset _TEXT:DMAEISACnt6 ;4CA EISA DMA high count/Channel 6
 dw DMA_E_C7,offset _TEXT:DMAEISACnt7 ;4CE EISA DMA high count/Channel 7

 dw DMA_E_P0,offset _TEXT:DMAEISAPg0  ;487 EISA DMA high page/Channel 0
 dw DMA_E_P1,offset _TEXT:DMAEISAPg1  ;483 EISA DMA high page/Channel 1
 dw DMA_E_P2,offset _TEXT:DMAEISAPg2  ;481 EISA DMA high page/Channel 2
 dw DMA_E_P3,offset _TEXT:DMAEISAPg3  ;482 EISA DMA high page/Channel 3
 dw DMA_E_P5,offset _TEXT:DMAEISAPg5  ;48B EISA DMA high page/Channel 5
 dw DMA_E_P6,offset _TEXT:DMAEISAPg6  ;489 EISA DMA high page/Channel 6
 dw DMA_E_P7,offset _TEXT:DMAEISAPg7  ;48A EISA DMA high page/Channel 7

TOTAL_EISA_EXTENDED_DMA_PORTS equ ($-DMAEISATable)/4

	page
;******************************************************************************
;   IO_Trap - Dispatches trap handler for an I/O address
;
;   ENTRY: Protected Mode Ring 0
;		AL = byte to output to port.
;		DX = port address for I/O.
;		SS:BP = points to stack frame on entry to GP fault handler
;		BX = 0 => Emulate Input.
;		   <>0 => Emulate Output.
;
;   EXIT:  Protected Mode Ring 0
;		AL = emulated input value from port.
;		CLC => I/O emulated by LIM_Trap.
;		STC => I/O NOT emulated by LIM_Trap.
;
;   USED:  Flags
;   STACK:
;------------------------------------------------------------------------------
IO_Trap proc	near
;
	push	dx
	push	ds
	push	gs

	push	VDMD_GSEL
	pop	ds			; set DS = _DATA
	push	RCODEA_GSEL
	pop	gs			; set GS = R_CODE

	cmp	dx,0100h		;Q: I/O Addr < 0100h (system brd port)?
	jae	SHORT IOT_NotISASys	;  N: check mapping regs
IOT_Sys:				;  Y: dispatch I/O trap handler
	xchg	bx, dx			; BL = port address
	shl	bx,1			; BX = BX*2 (word table)
	call	cs:DMATable[bx] 	; call handler
					;   ENTRY: entry DX,DS on stack
					;   DS = _DATA selector
					;   BX = port address in 0100h range
					;   DX = input/output flag

	xchg	bx,dx		; reset bx
	pop	gs
	pop	ds		;
	pop	dx		; reset dx
	ret

;
;    Address >= 0100h
;
IOT_NotISASys:
	test	gs:[GenFlags],fEISA   ;Q: EISA system?
	jz	short IOT_NotMap      ; N: reflect in first 1K IO address space
	push	ecx
	mov	ecx,TOTAL_EISA_EXTENDED_DMA_PORTS
IOT_EISAloop:
	cmp	dx,DMAEISATable[ECX*4-4];Q: This EISA port?
	je	short IOT_EISAPort	; Y: trap the EISA port
	loop	IOT_EISAloop		; N: try next EISA port
	pop	ecx
	xchg	bx,dx		; put Input/Output flag in DX for IOT_BadT
	push	ax		; push junk for return address
	jmp	short IOT_BadT	;  and fall thru to IOT_BadT

IOT_EISAPort:
	xchg	bx,dx			; BL = port address
	shl	bx,1			; BX = BX*2 (word table)
	call	cs:DMAEISATable[ecx*4-2]; call handler
					;   ENTRY: entry DX,DS on stack
					;   DS = _DATA selector
					;   BX = port address*2
					;   DX = input/output flag
	xchg	bx,dx		; reset bx
	pop	ecx
	pop	gs
	pop	ds		;
	pop	dx		; reset dx
	ret

;
;   Here if I/O Address >= 0100h and not a mapping register
;	map it into 1k and try system board ports again (NOT EISA or MCA)
;
IOT_NotMap:

ifdef	IOWRAP			; M011 - Start

	;
	;  Test for MCA: If so, don't wrap!
	;

	test	gs:[GenFlags], fMCA	; Q: MCA system
	jnz	short IOT_NoMap		; N: PS2 machine, don't wrap

	and	dx,3FFh 	; map address into 1k range
	cmp	dx,0100h	;Q: I/O Addr < 0100h (system brd port)?
	jb	IOT_Sys 	;  Y: check system ports
				;  N: Unknown port - signal error

endif				; M011 - End

IOT_NoMap:
	xchg	bx,dx		; put Input/Output flag in DX for IOT_BadT
	push	ax		;   push junk for return address
				;  and fall thru to IOT_BadT
;
;   IOT_BadT - GP fault on Unknown I/O address
;		**** return not emulated for now ****
;	ENTRY:	entry DX,DS,ret address on stack
;		DX = entry BX
;
IOT_BadT:
	pop	bx		; dump return address
	mov	bx,dx		; restore BX
	pop	gs
	pop	ds		; restore DS
	pop	dx		; reset port address
	stc			; port not emulated !
	ret			; and return

IOT_Chk92: 		   
			; Port 92h not 82h on password 8042 or PS2s
ifdef	ROMIDMCA
	push	VDMD_GSEL	; _DATA segment
	pop	ds
	cmp	[ROMID],ROMIDPS2;Q: PS2 (A20 toggle)?
	je	short P92_Handler
				; Y: A20 toggle mechanism
endif
	test	gs:[GenFlags], fMCA; Q: MCA (A20 toggle) ?
	jnz	P92_Handler	; Y: A20 toggle mechanism
				
ifndef	MSFLAG
				; N: Check for password 8042 system
				;Q: Is port 92h used to toggle A20?
	test	gs:[GenFlags],fP92 
	jnz	P92_Handler	; Y: virtualize IO

	test	gs:[GenFlags],fP8042 ;Q: Password 8042 but no A20 toggle?
	jne	IOT_BadT	  ; Y: then do real I/O
	jmp	DMAPg3		  ; N: emulate wrap to port 82h
else
	jmp	IOT_BadT	; N: then do real I/O			     *C
endif	

IOTPS2:				; PS2 A20 toggle mechanism
	jmp	P92_Handler

IO_Trap endp

_TEXT	ends

	end
