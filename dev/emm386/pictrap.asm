.386p
	page 58,132
;=============================================================================
	title	P I C T R A P - traps, programs and fields 8259 PIC functions
;=============================================================================
;==
;== (C) Copyright MICROSOFT Computer Corp. 1990-1991
;== (C) Copyright COMPAQ Computer Corp. 1990-1991
;==
;==	Title:	EMM386.EXE - MICROSOFT Expanded Memory Manager 386 Driver
;==
;==	Module: PICTrap  - Programs PIC, traps PIC programming, and fields
;==			   H/W interrupts caused by PIC.
;==
;==	Version: 1.00
;==
;==	Date:	March 10,1990
;==
;==	Author: Leo Cohen
;==
;=============================================================================
;==
;==	Change Log:
;==
;==	DATE	 REVISION	Description
;==	-------- --------	--------------------------------------------
;==     03/10/90 0.00	        Original
;==	04/18/90 		Added handlers for 25h, 26h, 2fh, 2ah, 33h
;==				and 5ch.
;==
;=============================================================================
;==
;==   Functional Description:
;==
;==   This module is responsible for fielding interrupts and then reflecting
;==   them to the virtual 8086 mode interrupt service routines.  It allows
;==   interrupts and reentrancy in protected mode to a nested level limited
;==   only by the stack size.
;==
;==   The algorithm design is based on servicing a H/W interrupt while a
;==   protected	mode service is in progress.  The interrupt must be reflected
;==   to the V86 interrupt service routine, and then return to the interrupted
;==   instruction in the protected mode service.  Because the V86 interrupt
;==   service routine may use a protected mode service, the process must be
;==   reentered multiple times; thus, the nesting level is limited only by the
;==   size of the stack.
;==
;==    PMTF processing (PMTF - Protected Mode Trap Frame)
;==
;== 1) Push segment registers.
;== 2) Subtract size of VMTF from ESP. (VMTF - Virtual Mode Trap Frame)
;== 3) Create dummy VMTF for IRETD. (returns to V86 interrupt service routine)
;== 4) Force real mode stack return to dummy HWInterEnd. (inc real mode SP first!)
;== 5) Push ESP0 into nest stack. (inc nest SP first!)
;== 6) Place ESP plus size of VMTF into ESP0 in TSS.
;== 7) IRETD
;==
;==    HWInterEnd processing
;==
;== 1) Add size of VMTF to ESP (throw VMTF away).
;== 2) Pop nest stack. (dec nest SP AFTER getting value!)
;== 3) Place nest stack value into ESP0 of TSS.
;==
;==    SS:SP (ring 0)     Nest Stack             TSS
;==
;==    ÚÄÄÄÄÄÄÄÄÄÄÄ¿     ÚÄÄÄÄÄÄÄÄÄÄÄ¿      ÚÄÄÄÄÄÄÄÄÄÄÄ¿
;==    ³   VMTF    ³     ³     a     ³      ³           ³
;==  a ÃÄÄÄÄÄÄÄÄÄÄÄ´     ÃÄÄÄÄÄÄÄÄÄÄÄ´      ³           ³
;==    ³ * local * ³     ³     b     ³      ³           ³
;==    ÃÄÄÄÄÄÄÄÄÄÄÄ´     ÃÄÄÄÄÄÄÄÄÄÄÄ´      ³           ³
;==    ³   PMTF    ³     ³     c     ³      ³           ³
;==    ÃÄÄÄÄÄÄÄÄÄÄÄ´     ÃÄÄÄÄÄÄÄÄÄÄÄ´      ÃÄÄÄÄÄÄÄÄÄÄÄ´
;==    ³  Seg Regs ³     ³           ³      ³ ESP0 = x  ³
;==    ÃÄÄÄÄÄÄÄÄÄÄÄ´     ÃÄÄÄÄÄÄÄÄÄÄÄ´      ÀÄÄÄÄÄÄÄÄÄÄÄÙ
;==    ³   VMTF    ³     ³           ³
;==  b ÃÄÄÄÄÄÄÄÄÄÄÄ´     ³           ³
;==    ³ * local * ³     ³           ³
;==    ÃÄÄÄÄÄÄÄÄÄÄÄ´     ³           ³
;==    ³   PMTF    ³     ³           ³
;==    ÃÄÄÄÄÄÄÄÄÄÄÄ´     ÀÄÄÄÄÄÄÄÄÄÄÄÙ
;==    ³  Seg Regs ³
;==    ÃÄÄÄÄÄÄÄÄÄÄÄ´
;==    ³   VMTF    ³
;==  c ÃÄÄÄÄÄÄÄÄÄÄÄ´
;==    ³ * local * ³
;==    ÃÄÄÄÄÄÄÄÄÄÄÄ´
;==    ³   PMTF    ³
;==    ÃÄÄÄÄÄÄÄÄÄÄÄ´
;==    ³  Seg Regs ³
;==  x ÃÄÄÄÄÄÄÄÄÄÄÄ´
;==    ³           ³
;==    ³           ³
;==    ÀÄÄÄÄÄÄÄÄÄÄÄÙ
;==
;=============================================================================
;==
;==	P U B L I C   D E C L A R A T I O N S
;=============================================================================
ifdef PICtrap
	public	EnterVirtPIC
	public	ExitVirtPIC
	public	ProgramPIC
endif
	public	PICInit
	public	PICVecInit

	public	MasterPICVec
	public	SlavePICVec

ifdef PICtrap
	public	VirMasterPICVec
	public	VirSlavePICVec
	public	PICCommand1
	public	PICCommand2
	public	PICData1
	public	PICData2
	public	MasterIS
	public	LastOCW3
endif
	public	pLastVMTF
	public	LastVMTF

	public	ReflectInterrupt
	public	pIRQHandlerMaster
	public	pIRQHandler
	public	pIRQ0Handler
	public	pIRQ1Handler
	public	pIRQ2Handler
	public	pIRQ3Handler
	public	pIRQ4Handler
	public	pIRQ5Handler
	public	pIRQ6Handler
	public	pIRQ7Handler
	public	pIRQ8Handler
	public	pIRQ9Handler
	public	pIRQ10Handler
	public	pIRQ11Handler
	public	pIRQ12Handler
	public	pIRQ13Handler
	public	pIRQ14Handler
	public	pIRQ15Handler
	public	pTrapHandler
	public	pINT25hHandler
	public	pINT26hHandler
	public	pINT2ahHandler
	public	pINT2fhHandler
	public	pINT33hHandler
	public	pINT5chHandler
;=============================================================================
;==	L O C A L   C O N S T A N T S
;=============================================================================
	include allocmem.inc
	include vdmseg.inc
	include vdmsel.inc
	include desc.inc
	include dma.inc
	include vm386.inc
	include	emm386.inc
	include	emmfunct.inc
	include	emmdata.inc
	include	winemm.inc

ifdef PICtrap
PIC_ICW1	equ	00010001b	; ICW1
PIC_ICW2	equ	00000000b	; ICW2 (or with base vector)
MPIC_ICW3	equ	00000100b	; Master ICW3 (slave on IR 2)
SPIC_ICW3	equ	00000010b	; Slave ICW3 (respond to cascade 2)
PIC_ICW4	equ	00000001b	; ICW4

PIC_OCW3	equ	00001010b	; OCW3 (read IRR)

ICW1_DEF	equ	00010000b	; ICW1 definition
OCW2_DEF	equ	00011000b	; OCW2 definition
OCW2_EOI	equ	00100000b	; OCW2/EOI bit
OCW2_SL		equ	01000000b	; OCW2/SL bit
OCW2_R		equ	10000000b	; OCW2/R bit
OCW3_DEF	equ	00001000b	; OCW3 definition
OCW3_RR		equ	00000010b	; RR bit field
OCW3_RIS	equ	00000001b	; RIS bit field
endif

;=============================================================================
;==	E X T E R N A L   R E F E R E N C E S
;=============================================================================
_TEXT	segment
	extrn	DMACheckTC:near
	extrn	PortTrapFar:far
	extrn	pXMMentry:near
	extrn	pWinEMMGlobImpDisp:near
ifdef PICtrap
	extrn	VCPIProgPIC:near
endif
_TEXT	ends

R_CODE	segment
	extrn	DebBreakStr:byte
R_CODE	ends

R1_CODE	segment
	extrn	InstanceData:byte
	extrn	Win386VxDRefDat:byte
	extrn	rQueryXMSpages:near
	extrn	rAllocateXMSblock:near
	extrn	rFreeXMSblock:near
R1_CODE	ends

;=============================================================================
;==	L O C A	L   D A T A
;=============================================================================
_DATA	segment

ifdef PICtrap
even
PICFlags	label	word
	dw	0
	dw	0
fPICMoved	equ 00000001b	; PIC vectors moved by CEMM
fICW1		equ 00000010b	; Indicates ICW1 detected
fICW2		equ 00000100b	; Indicates ICW2 detected
fICW3		equ 00001000b	; Indicates ICW3 detected
fPICMovedbit	equ 	0
fICW1bit	equ 	1
fICW2bit	equ 	2
fICW3bit	equ 	3

ICW1	label	byte
	dw	PIC_ICW1
	dw	PIC_ICW1
ICWs	label	byte
ICW2	label	byte
	dw	DOS_MASTER_VECTOR
	dw	DOS_SLAVE_VECTOR
ICW3	label	byte
	dw	MPIC_ICW3
	dw	SPIC_ICW3
ICW4	label	byte
	dw	PIC_ICW4
	dw	PIC_ICW4
ICWmask	label	byte
	dw	0
	dw	0
endif

pLastVMTF	dw	-1
LastVMTF	label	word
	dw	256 dup (0)

_DATA	ends

_TEXT	segment

rModeCallTable	label	word
	dw	offset R1_CODE:rQueryXMSpages
	.errnz	QUERY_XMS_PAGES - FIRST_RMODE_RTN
	dw	offset R1_CODE:rAllocateXMSblock
	.errnz	ALLOC_XMS_BLOCK - QUERY_XMS_PAGES - 1
	dw	offset R1_CODE:rFreeXMSblock
	.errnz	FREE_XMS_BLOCK - ALLOC_XMS_BLOCK - 1
	.errnz	FREE_XMS_BLOCK - LAST_RMODE_RTN

_TEXT	ends


R_CODE	segment

even

PICVec		label	word
MasterPICVec	dw  DOS_MASTER_VECTOR	; Master PIC vector
SlavePICVec	dw  DOS_SLAVE_VECTOR	; Slave PIC vector

ifdef PICtrap
VirPICVec	label	word
VirMasterPICVec	dw  DOS_MASTER_VECTOR	; Virtual master PIC vector
VirSlavePICVec	dw  DOS_SLAVE_VECTOR	; Virtual mode slave PIC vector

MasterIS	dw	0		; In service levels on master PIC
ZERO		dw	0
LastOCW3	db	PIC_OCW3	; Last OCW3 to master PIC

OldSlaveIRQHandler	label	dword
	dd	8 dup (0)

OldInt21hHandler dd	0

PharLapCopy	db   'Copyright (C) '	; Phar Lap copy right message in PSP
PharLapCopyLen	equ	$-PharLapCopy
PharLapName	db   'Phar Lap Software, Inc.'	; Phar Lap message in PSP
PharLapNameLen	equ	$-PharLapName
PharLapOffs	equ	100h		; offset into PSP for Phar Lap Copy Right message

PICPortList	label	word
PICcommPorts label word
	dw	020h			; PIC1_CMD
	dw	0A0h			; PIC2_CMD
PICdataPorts label word
	dw	021h			; PIC1_INIT
	dw	0A1h			; PIC2_INIT
TOTAL_PIC_PORTS	equ	($-PICPortList)/2
endif

extrn	rI15KeyBoard:near

R_CODE	ends

LAST	segment

ifdef PICtrap
rMasterIRQHandlers	label	word
	dw	offset R_CODE:rIRQ0Handler
	dw	offset R_CODE:rIRQ1Handler
	dw	offset R_CODE:rIRQ2Handler
	dw	offset R_CODE:rIRQ3Handler
	dw	offset R_CODE:rIRQ4Handler
	dw	offset R_CODE:rIRQ5Handler
	dw	offset R_CODE:rIRQ6Handler
	dw	offset R_CODE:rIRQ7Handler

rSlaveIRQHandlers	label	word
	dw	offset R_CODE:rIRQ8Handler
	dw	offset R_CODE:rIRQ9Handler
	dw	offset R_CODE:rIRQ10Handler
	dw	offset R_CODE:rIRQ11Handler
	dw	offset R_CODE:rIRQ12Handler
	dw	offset R_CODE:rIRQ13Handler
	dw	offset R_CODE:rIRQ14Handler
	dw	offset R_CODE:rIRQ15Handler
endif

pMasterIRQHandlers	label	word
	dw	offset _TEXT:pIRQ0Handler
	dw	offset _TEXT:pIRQ1Handler
	dw	offset _TEXT:pIRQ2Handler
	dw	offset _TEXT:pIRQ3Handler
	dw	offset _TEXT:pIRQ4Handler
	dw	offset _TEXT:pIRQ5Handler
	dw	offset _TEXT:pIRQ6Handler
	dw	offset _TEXT:pIRQ7Handler

pSlaveIRQHandlers	label	word
	dw	offset _TEXT:pIRQ8Handler
	dw	offset _TEXT:pIRQ9Handler
	dw	offset _TEXT:pIRQ10Handler
	dw	offset _TEXT:pIRQ11Handler
	dw	offset _TEXT:pIRQ12Handler
	dw	offset _TEXT:pIRQ13Handler
	dw	offset _TEXT:pIRQ14Handler
	dw	offset _TEXT:pIRQ15Handler

pIRQ5xHandlers	label	word
	dw	offset _TEXT:pIRQ50Handler
	dw	offset _TEXT:pIRQ51Handler
	dw	offset _TEXT:pIRQ52Handler
	dw	offset _TEXT:pIRQ53Handler
	dw	offset _TEXT:pIRQ54Handler
	dw	offset _TEXT:pIRQ55Handler
	dw	offset _TEXT:pIRQ56Handler
	dw	offset _TEXT:pIRQ57Handler

LAST	ends
;=============================================================================
;==	C O D E  S E G M E N T
;=============================================================================
_TEXT	segment
	assume	cs:_TEXT,ds:_DATA,es:_DATA,gs:R_CODE,ss:STACK

ifdef PICtrap
;==============================================================================
;==
;== Initialization Command Word Format (ICW1-ICW4) (X-default)
;==
;==
;== ICW1: (Write) 8259A Programmable Interrupt Controller (20h & A0h)
;==
;== 76543210
;== xxxxxxxxÄÄÄ> IC4    0 = No ICW4 needed
;== ³³³³³³³             1 = ICW4 needed     			X
;== ³³³³³³³
;== ³³³³³³ÀÄÄÄÄ> SNGL   0 = Cascade mode    			X
;== ³³³³³³              1 = Single
;== ³³³³³³
;== ³³³³³ÀÄÄÄÄÄ> ADI    0 = Call address interval of 8 		X
;== ³³³³³               1 =  "     "        "     "  4
;== ³³³³³
;== ³³³³ÀÄÄÄÄÄÄ> LTIM   0 = Edge triggered mode  		X
;== ³³³³                1 = Level triggered mode
;== ³³³³
;== ³³³ÀÄÄÄÄÄÄÄ> 1      Indicates ICW1
;== ³³³
;== ÀÁÁÄÄÄÄÄÄÄÄ> 0	A5-A7: Vector address for MCS80/85 mode only
;==
;==
;== ICW2: (Write) 8259A Programmable Interrupt Controller (21h & A1h)
;==
;== 76543210
;== xxxxxxxx
;== ³³³³³ÀÁÁÄÄÄ> 0	A8-A10: Vector address for MCS80/85 mode only
;== ³³³³³
;== ÀÁÁÁÁÄÄÄÄÄÄ> T3-T7  Vector address for 8086/8088 mode only
;==			or A11-A15: Vector address for MCS80/85 mode only
;==
;==
;== ICW3: (Write) 8259A Programmable Interrupt Controller (21h & A1h)
;==
;== 76543210
;== xxxxxxxx
;== ³³³³³ÃÅÅÄÄÄ> SLAVE  ID number of this slave (0-7) 		(02h)
;== ³³³³³³³³
;== ÀÁÁÁÁÁÁÁÄÄÄ> MASTER 0 = IR input does not have a slave 	(04h)
;==			1 = IR input has a slave
;==
;==
;== ICW4: (Write) 8259A Programmable Interrupt Controller (21h & A1h)
;==
;== 76543210
;== xxxxxxxxÄÄÄ> uPM    0 = MCS80/85 mode
;== ³³³³³³³             1 = 8086/8088 mode			X
;== ³³³³³³³
;== ³³³³³³ÀÄÄÄÄ> AEOI   0 = Normal EOI				X
;== ³³³³³³              1 = Auto EOI
;== ³³³³³³
;== ³³³³³ÀÄÄÄÄÄ> M/S    0 = Buffered mode: Slave   		X
;== ³³³³³               1 = Buffered mode: Master
;== ³³³³³
;== ³³³³ÀÄÄÄÄÄÄ> BUF    0 = Not buffered mode      		X
;== ³³³³                1 = Buffered mode
;== ³³³³
;== ³³³ÀÄÄÄÄÄÄÄ> SFNM   0 = Special fully nested mode     	X
;== ³³³                 1 = Not special fully nested mode
;== ³³³
;== ÀÁÁÄÄÄÄÄÄÄÄ> 0
;==
;==============================================================================

;==============================================================================
;==
;== Operation Command Word Format (OCW1-OCW3)
;==
;==
;== OCW1: (Write) 8259A Programmable Interrupt Controller (21h & A1h)
;==
;== 76543210
;== xxxxxxxx
;== ÀÁÁÁÁÁÁÁÄÄÄ> IM	0 = IR mask reset
;==			1 = IR mask set
;==
;==
;== OCW2: (Write) 8259A Programmable Interrupt Controller (20h & A0h)
;==
;== 76543210
;== xxxxxxxx
;== ³³³³³ÀÁÁÄÄÄ> L0-L2  IR level to be acted upon (IR0-IR7) (if bit 6 is set)
;== ³³³ÀÁÄÄÄÄÄÄ> 0      OCW2 definition
;== ³³ÀÄÄÄÄÄÄÄÄ> EOI    End Of Interrupt
;== ³ÀÄÄÄÄÄÄÄÄÄ> SL	Set interrupt Level
;== ÀÄÄÄÄÄÄÄÄÄÄ> R	Rotate Mode
;==
;== R SL EOI
;==
;== 0  0  0	Rotate in auto EOI mode (clear)      Auto Rotation (reserved)
;== 0  0  1	Non-specific EOI command             EOI
;== 0  1  0	No operation			     noop
;== 0  1  1	Specific EOI command		     EOI
;== 1  0  0	Rotate in auto EOI mode (set)        Auto Rotation (reserved)
;== 1  0  1	Rotate on non-specific EOI command   Auto Rotation
;== 1  1  0	Set priority command                 Specific Rotation
;== 1  1  1	Rotate on specific EOI command	     Specific Rotation
;==
;== OCW3: (Write) 8259A Programmable Interrupt Controller (20h & A0h)
;==
;== 76543210
;== xxxxxxxxÄÄÄ> RIS    0 = Read IR register on next read (RR must be set)
;== ³³³³³³³             1 = Read IS register on next read (RR must be set)
;== ³³³³³³³
;== ³³³³³³ÀÄÄÄÄ> RR     0 = Do not read IR/IS register: RIS is a no-op
;== ³³³³³³              1 = Read IR/IS register: RIS determines whether IR/IS
;== ³³³³³³
;== ³³³³³ÀÄÄÄÄÄ> P	0 = No poll command 		X
;== ³³³³³		1 = Poll command
;== ³³³³³
;== ³³³³ÀÄÄÄÄÄÄ> 1      OCW3 definition
;== ³³³ÀÄÄÄÄÄÄÄ> 0       "       "
;== ³³³
;== ³³ÀÄÄÄÄÄÄÄÄ> SMM    0 = Reset special mask          X
;== ³³                  1 = Set special mask
;== ³³
;== ³ÀÄÄÄÄÄÄÄÄÄ> ESMM   0 = SMM is a no-op              X
;== ³                   1 = SMM determines whether a clear/set of special mask
;== ³
;== ÀÄÄÄÄÄÄÄÄÄÄ> 0
;==
;==============================================================================

;==============================================================================
;==
;== PICCommand: Trap PIC command ports 20h & A0h and detect attempt to
;==		reprogram vector via ICWs.
;==
;== Entry: (Protected Mode)
;==	DS = _DATA
;==	GS = R_CODE
;==
;== Exit:  (Protected Mode)
;==
;==============================================================================
PICCommand1:
	push	si
;
;  Check to see if this is an OCW2 doing an EOI for the Master PIC
;
	or	dx,dx			;Q: Is it an OUT command?
	jz	short PICCMasterCont	; N: don't trap

;
;  If H/W interrupts are not entering via base vectors, don't virtualize PIC's
;  IS register.
;
	cmp	gs:[MasterPICVec],DOS_MASTER_VECTOR    ;Q: Virtualize IS register?
	jne	short PICCMasterCont	; N: don't trap

	push	ax

	movzx	si,al
	and	al,OCW2_DEF		;Q: Is it an OCW2?
	jnz	short PICCcheckOCW3	; N: check if OCW3

	test	si,OCW2_EOI		;Q: Is it an EOI?
	jz	short PICCnotEOI	; N: continue

	mov	ax,si			; restore AL
	out	20h,al			; do EOI

	mov	ax,1011b		; read ISR
	out	20h,al
	in	al,20h			; read ISR from master PIC
	mov	gs:[MasterIS],ax	; save ISR state
	mov	al,gs:[LastOCW3]
	out	20h,al			; restore last OCW3

	pop	ax
	pop	si
	clc
	ret

PICCcheckOCW3:
	test	si,OCW3_DEF		;Q: Is it an OCW3?
	jz	short PICCnotEOI	; N: continue
	and	al,not OCW3_DEF		;Q: Is it an OCW3?
	jnz	short PICCnotEOI	; N: continue
	test	si,OCW3_RR		;Q: Is it ISR/IRR command?
	jz	short PICCnotEOI	; N: continue

	pop	ax
	mov	gs:[LastOCW3],al
	jmp	short PICCMasterCont

ifdef 900417
	test	si,OCW2_R		;Q: Is it a rotate priority EOI?
	jnz	short PICCnotEOI	; Y: currently, can not handle! ***QLEO***

	test	si,OCW2_SL		;Q: Is it a specific level EOI?
	jnz	short PICCspecificEOI	; Y: mark EOI

	bsf	ax,gs:[MasterIS]  	;Q: Highest priority level needing EOI?
	jz	short PICCnotEOI	; N: no level needs EOI, exit
	jmp	short PICCEOI		; Y: AX contains highest interrupt level

PICCspecificEOI:
	mov	ax,si
	and	ax,7			; specific level to EOI
PICCEOI:
	btr	gs:[MasterIS],ax
endif


PICCnotEOI:
	pop	ax
PICCMasterCont:
	xor	si,si			; index into controller 1
	jmp	short PICCommand


PICCommand2:
	push	si
	mov	si,2			; index into controller 2

PICCommand:
	and	[PICFlags][si],not (fICW1+fICW2+fICW3)

	or	dx,dx			;Q: Is it an OUT command?
	jz	short PICCnoEmul	; N: don't trap

	test	al,ICW1_DEF		;Q: Is it an ICW1 command?
	jz	short PICCnoEmul	; N: don'trap
	or	[PICFlags][si],fICW1    ; Y: set flag to watch for ICW2
	mov	[ICW1][si],al
	clc
PICCexit:
	pop	si
	ret

PICCnoEmul:
	stc
	jmp	short PICCexit

;==============================================================================
;==
;== PICData: Trap PIC data ports 21h & A1h and detect attempt to
;==	     reprogram vector via ICW2.
;==
;== Entry: (Protected Mode)
;==	DS = _DATA
;==	GS = R_CODE
;==
;== Exit:  (Protected Mode)
;==
;==============================================================================
PICData1:
	push	si
	xor	si,si			; index into controller 1
	jmp	short PICData

PICData2:
	push	si
	mov	si,2			; index into controller 2

PICData:
	btr	[PICFlags][si],fICW3bit	;Q: Was prior command an ICW3?
	jc	short PICDICW4		; Y: virtualize
	btr	[PICFlags][si],fICW2bit	;Q: Was prior command an ICW2?
	jc	short PICDICW3		; Y: virtualize
	btr	[PICFlags][si],fICW1bit	;Q: Was prior command an ICW1?
	jnc	short PICDnoEmul	; N: no emulation, proceed!

PICDICW2:
	and	[PICFlags][si],not (fICW1+fICW2+fICW3)
	or	dx,dx			;Q: Is it an OUT command?
	jz	short PICDnoEmul	; N: don't trap

	push	ax			; virtualize new PIC vector locations
	and	ax,0F8h			; get new vector location
	mov	gs:[VirPICVec][si],ax	; save for real mode IRQ handlers
	pop	ax

	or	[PICFlags][si],fICW2
	test	[PICFlags][si],fPICMoved;Q: Has CEMM moved the PIC vector?
	jnz	short PICDexit		; Y: don't program
	mov	[ICW2][si],al
	jmp	short PICDexit

PICDICW3:
	and	[PICFlags][si],not (fICW1+fICW2+fICW3)
	or	dx,dx			;Q: Is it an OUT command?
	jz	short PICDnoEmul	; N: don't trap
	or	[PICFlags][si],fICW3	; Y: virtualize
	test	[PICFlags][si],fPICMoved;Q: Has CEMM moved the PIC vector?
	jnz	short PICDexit		; Y: don't program
	mov	[ICW3][si],al
	jmp	short PICDexit

PICDICW4:
	and	[PICFlags][si],not (fICW1+fICW2+fICW3)
	or	dx,dx			;Q: Is it an OUT command?
	jz	short PICDnoEmul	; N: don't trap
	test	[PICFlags][si],fPICMoved;Q: Has CEMM moved the PIC vector?
	jnz	short PICDexit		; Y: don't program
	mov	[ICW4][si],al		; N: virtualize

	pushad
	mov	bx,gs:[VirMasterPICVec]
	mov	cx,gs:[VirSlavePICVec]

	call	VCPIProgPIC
	popad

PICDexit:
	clc
	pop	si
	ret

PICDnoEmul:
	stc
	pop	si
	ret

;==============================================================================
;==
;== EnterVirtPIC: Program IRQ0-IRQ15 vectors to the values in
;==		  R_CODE:[MasterPICVec] & R_CODE:[SlavePICVec] locations.
;==
;== Entry: (Protected Mode)
;==	CLI  interrupts must be off
;==	DS = _DATA
;==	GS = R_CODE
;==
;== Exit:  (Protected Mode)
;==
;==
;==============================================================================
EnterVirtPIC	proc	near
	push	ax
;
;  Assume master PIC is being programmed to base vector locations
;
	mov	gs:[MasterIS],0

	test	[PICFlags],fPICMoved	;Q: Need to program PIC?
	jnz	short EVPcont		; Y: program
;
;  Initialize PICs to base vector locations
;
	mov	gs:[MasterPICVec],DOS_MASTER_VECTOR
	mov	gs:[VirMasterPICVec],DOS_MASTER_VECTOR
	mov	gs:[SlavePICVec],DOS_SLAVE_VECTOR
	mov	gs:[VirSlavePICVec],DOS_SLAVE_VECTOR
	jmp	short EVPexit

EVPcont:
	mov	al,PIC_ICW1
	out	020h,al
	out	0A0h,al

	mov	ax,gs:[MasterPICVec]
	out	021h,al
	mov	ax,gs:[SlavePICVec]
	out	0A1h,al

	mov	al,MPIC_ICW3
	out	021h,al
	mov	al,SPIC_ICW3
	out	0A1h,al

	mov	al,PIC_ICW4
	out	021h,al
	out	0A1h,al

	mov	al,PIC_OCW3
	out	020h,al
	out	0A0h,al
EVPexit:
	pop	ax
	ret
EnterVirtPIC	endp

;==============================================================================
;==
;== ExitVirtPIC: Program IRQ0-IRQ15 vectors to the values in
;==		 R_CODE:[VirMasterPICVec] & R_CODE:[VirSlavePICVec] locations.
;==
;== Entry: (Protected Mode)
;==	CLI  interrupts must be off
;==	GS = R_CODE
;==
;== Exit:  (Protected Mode)
;==
;==
;==============================================================================
ExitVirtPIC	proc	near
	push	ax

	test	[PICFlags],fPICMoved	;Q: Need to program PIC?
	jz	short XVPexit		; Y: leave PICs alone

	mov	al,PIC_ICW1
	out	020h,al
	out	0A0h,al

	mov	ax,gs:[VirMasterPICVec]
	out	021h,al
	mov	ax,gs:[VirSlavePICVec]
	out	0A1h,al

	mov	al,MPIC_ICW3
	out	021h,al
	mov	al,SPIC_ICW3
	out	0A1h,al

	mov	al,PIC_ICW4
	out	021h,al
	out	0A1h,al

	mov	al,PIC_OCW3
	out	020h,al
	out	0A0h,al
XVPexit:
	pop	ax
	ret
ExitVirtPIC	endp

;==============================================================================
;==
;== ProgramPIC: Program PIC to last virtual ICWs (or default values).
;==
;== Entry: (Protected Mode)
;==	CLI  interrupts must be off
;==	DS = _DATA
;==	GS = R_CODE
;==
;== Exit:  (Protected Mode)
;==
;==
;==============================================================================
ProgramPIC proc	near
	push	esi
	push	edi
	push	ax
	push	dx
	push	cx

	test	[PICFlags][0],fPICMoved	;Q: Did CEMM move master PIC?
	jnz	short PPexit		; Y: no virtualization of ICWs
;
;  Get current mask register and then program ICW1 for each PIC
;
	xor	esi,esi
	mov	cx,2
PPloop:
;
;  Get and save mask register
;
	mov	dx,PICdataPorts[esi*2]	; get current mask register
	in	al,dx
	mov	[ICWmask][esi*2],al
;
;  Program ICW1
;
	mov	dx,[PICcommPorts][esi*2] ; program ICW1 to command port
	mov	al,[ICW1][esi*2]
	out	dx,al
;
;  Program ICW2-4 and mask for each PIC
;
	push	cx
	mov	cx,4			; program ICW2-4 and mask register
	xor	edi,edi
PPdata:
	mov	dx,PICdataPorts[esi*2]	; get data port
	mov	al,[ICWs][edi][esi*2]	; get virtual data
	out	dx,al			; program PIC
	add	di,4			; next ICW/mask
	loop	PPdata
	pop	cx
;
;  Program OCW3
;
	mov	al,PIC_OCW3
	mov	dx,PICcommPorts[esi*2]	; program OCW3 to command port
	out	dx,al

	inc	si
	loop	PPloop

PPexit:
	pop	cx
	pop	dx
	pop	ax
	pop	edi
	pop	esi
	ret
ProgramPIC	endp
endif

	assume ds:nothing,es:nothing,fs:nothing,gs:nothing
;==============================================================================
;==
;==  pIRQxHandler: H/W protected mode interrupt handlers.  These routines will
;==		   reflect HW interrupts to the real mode interrupt handlers.
;==
;==  Entry:  (Protected Mode via 386 Interrupt gate)
;==	SS:SP = Depending on the processor mode when the interrupt occurred:
;==		1) V8086 mode: the virtual mode 8088 stack frame
;==		2) Protected mode: 32-bit EIP, CS, & EFLAGS.
;==
;==  Exit:   EBP pushed on stack
;==	BP = stack frame pointer
;==
;==============================================================================
ALIGN 16
pIRQ0Handler:
	push	ebp
	push	0
	jmp	short pIRQHandlerMaster

ifdef LC910610
pIRQ1Handler:
	push	ebp
	push	1
	jmp	short pIRQHandlerMaster
endif

pIRQ2Handler:
	push	ebp
	push	2
	jmp	short pIRQHandlerMaster

pIRQ3Handler:
	push	ebp
	push	3
	jmp	short pIRQHandlerMaster

pIRQ4Handler:
	push	ebp
	push	4
	jmp	short pIRQHandlerMaster

pIRQ5Handler:
	push	ebp
	push	5
	jmp	short pIRQHandlerMaster

pIRQ6Handler:
	push	ebp
	push	6
	jmp	short pIRQHandlerMaster

pIRQ7Handler:
	push	ebp
	push	7
	jmp	short pIRQHandlerMaster

pIRQ8Handler:
	push	ebp
	push	0
	jmp	short pIRQHandlerSlave

pIRQ9Handler:
	push	ebp
	push	1
	jmp	short pIRQHandlerSlave

pIRQ10Handler:
	push	ebp
	push	2
	jmp	short pIRQHandlerSlave

pIRQ11Handler:
	push	ebp
	push	3
	jmp	short pIRQHandlerSlave

pIRQ12Handler:
	push	ebp
	push	4
	jmp	short pIRQHandlerSlave

pIRQ13Handler:
	push	ebp
	push	5
	jmp	short pIRQHandlerSlave

pIRQ14Handler:
	push	ebp
	push	6
	jmp	short pIRQHandlerSlave

pIRQ15Handler:
	push	ebp
	push	7
	jmp	short pIRQHandlerSlave

pIRQHandlerSlave:
	mov	bp,sp
	push	ebx
	mov	ebx,2
	jmp	short pIRQHandlerN

ALIGN	16
pIRQHandlerMaster:
	movzx	ebp,sp
	push	ebx
	xor	ebx,ebx

pIRQHandlerN:
	push	esi
	push	ds

	mov	si,DATA32_GSEL
	mov	ds,si

	mov	si,seg R_CODE
	movzx	esi,si
	shl	esi,4				; DS:[ESI] point to R_CODE
	assume	ds:R_CODE
ifdef PICtrap
	mov	bx,[esi][VirPICVec][ebx]
else
	mov	bx,[esi][PICVec][ebx]
endif
	add	[bp],bx
	add	bp,2

pIRQHandler:
;
;  Check if interrupt occurred in virtual8086 or protected mode.
;
	test	[bp][VTFO].VMTF_EFLAGShi,FLAGS_VM ;Q: Virtual Mode?
	jnz	vReflectInterrupt		  ; Y: vReflectInterrupt
						  ; N: pReflectInterrupt
;===============================================================================
;==
;== pReflectInterrupt:  This procedure reflects interrupts which occurred
;==		        while protected mode code was executing.
;==
;== Entry: (Protected Mode)
;==	BP    = Points to Protected Mode Interrupt Stack Frame (no error)
;==	DS    = DATA32_GSEL (4GB, zero based selector)
;==	ESI   = 32-bit pointer to base of R_CODE segment
;==	[pLastVMTF]= pointer into nesting stack
;==	[LastVMTF] = nesting stack: pointers to last Virtual Mode Trap Frame
;==	SS:SP =
;==               HIword LOword
;==              ÉÍÍÍÍÍÍÍÍÍÍÍÍÍ»
;==              º    EFLAGS   º  +0Ch
;==              ÇÄÄÄÄÄÄÂÄÄÄÄÄÄ¶
;==              º 0000 ³  CS  º  +08h
;==              ÇÄÄÄÄÄÄÁÄÄÄÄÄÄ¶
;==              º     EIP     º  +04h
;==              ÇÄÄÄÄÄÄÄÄÄÄÄÄÄ¶
;==              º     EBP     º  +00h
;==              ÌÍÍÍÍÍÍÍÍÍÍÍÍÍ¹ <Ä SS:BP
;==              º Int. Vector º  -02h (word)
;==              ÌÍÍÍÍÍÍÍÍÍÍÍÍÍ¹
;==              º     EBX     º  -06h (dword)
;==              ÌÍÍÍÍÍÍÍÍÍÍÍÍÍ¹
;==              º     ESI     º  -0Ah (dword)
;==              ÌÍÍÍÍÍÍÍÍÍÍÍÍÍ¹
;==              º     DS      º  -0Ch (word)
;==              ÈÍÍÍÍÍÍÍÍÍÍÍÍÍ¼ <Ä SS:SP
;==
;== Processing for HLT instruction:
;==
;==	If the VM has executed an sti hlt, then we know that the cs, eip
;== above is pointing to the instruction after the hlt that we executed in
;== EmHalt (vminst.asm). We also know that the VMTF immediatedly above the
;== EFLAGS is the one caused by the user's sti hlt plus EBP (see emhalt).
;== We wasnt to reflect this HW int down and set up the real mode stack so
;== that when the HW ISR does it's IRET control goes back to the instruction
;== after the user's hlt instruction. So we set up the stack for
;== vreflectinterrupt as follows:
;==
;==                ÉÍÍÍÍÍÍÍÍÍÍÍÍÍ»
;==                º 0000 ³  GS  º
;==                ÇÄÄÄÄÄÄÅÄÄÄÄÄÄ¶
;==                º 0000 ³  FS  º  +20h
;==                ÇÄÄÄÄÄÄÅÄÄÄÄÄÄ¶
;==                º 0000 ³  DS  º  +1Ch
;==                ÇÄÄÄÄÄÄÅÄÄÄÄÄÄ¶
;==                º 0000 ³  ES  º  +18h
;==                ÇÄÄÄÄÄÄÅÄÄÄÄÄÄ¶
;==                º 0000 ³  SS  º  +14h
;==                ÇÄÄÄÄÄÄÁÄÄÄÄÄÄ¶
;==                º     ESP     º  +10h
;==                ÇÄÄÄÄÄÄÄÄÄÄÄÄÄ¶
;==                º    EFLAGS   º  +0Ch
;==                ÇÄÄÄÄÄÄÂÄÄÄÄÄÄ¶
;==                º 0000 ³  CS  º  +08h  user segent
;==                ÇÄÄÄÄÄÄÁÄÄÄÄÄÄ¶
;==                º     EIP     º  +04h  instruction after hlt
;==                ÇÄÄÄÄÄÄÄÄÄÄÄÄÄ¶
;==                º     EBP     º  +00h     Start of VM_TRAP_FRAME (VMTF)
;==                ÌÍÍÍÍÍÍÍÍÍÍÍÍÍ¹ <Ä SS:BP
;==                º Int. Vector º  -02h (word)
;==                ÌÍÍÍÍÍÍÍÍÍÍÍÍÍ¹
;==                º     EBX     º  -06h (dword)
;==                ÌÍÍÍÍÍÍÍÍÍÍÍÍÍ¹
;==                º     ESI     º  -0Ah (dword)
;==                ÌÍÍÍÍÍÍÍÍÍÍÍÍÍ¹
;==                º DS (dummy)  º  -0Ch (word)
;==                ÈÍÍÍÍÍÍÍÍÍÍÍÍÍ¼ <Ä SS:SP
;==
;==
;== Exit: (Protected Mode)
;==	BP    = Points to new Virtual Mode Interrupt Stack Frame (no error)
;==	DS    = DATA32_GSEL (4GB, zero based selector)
;==	ESI   = 32-bit pointer to base of R_CODE segment
;==	[pLastVMTF]= pointer into nesting stack incremented
;==	[LastVMTF] = nesting stack: pointers to last Virtual Mode Trap Frame
;==	SS:SP =
;==                 HIword LOword
;==                ÉÍÍÍÍÍÍÍÍÍÍÍÍÍ»
;==                º    EFLAGS   º
;==                ÇÄÄÄÄÄÄÂÄÄÄÄÄÄ¶
;==                º 0000 ³  CS  º
;==                ÇÄÄÄÄÄÄÁÄÄÄÄÄÄ¶
;==                º     EIP     º
;==                ÇÄÄÄÄÄÄÂÄÄÄÄÄÄ¶
;==                º  ES  ³  DS  º
;==                ÇÄÄÄÄÄÄÅÄÄÄÄÄÄ¶
;==                º  GS  ³  FS  º
;==                ÇÍÍÍÍÍÍØÍÍÍÍÍÍ¹
;==                º 0000 ³  GS  º
;==                ÇÄÄÄÄÄÄÅÄÄÄÄÄÄ¶
;==                º 0000 ³  FS  º  +20h
;==                ÇÄÄÄÄÄÄÅÄÄÄÄÄÄ¶
;==                º 0000 ³  DS  º  +1Ch
;==                ÇÄÄÄÄÄÄÅÄÄÄÄÄÄ¶
;==                º 0000 ³  ES  º  +18h
;==                ÇÄÄÄÄÄÄÅÄÄÄÄÄÄ¶
;==                º 0000 ³  SS  º  +14h
;==                ÇÄÄÄÄÄÄÁÄÄÄÄÄÄ¶
;==                º     ESP     º  +10h
;==                ÇÄÄÄÄÄÄÄÄÄÄÄÄÄ¶
;==                º    EFLAGS   º  +0Ch
;==                ÇÄÄÄÄÄÄÂÄÄÄÄÄÄ¶
;==                º 0000 ³  CS  º  +08h  R_CODE
;==                ÇÄÄÄÄÄÄÁÄÄÄÄÄÄ¶
;==                º     EIP     º  +04h  HWInterEnd
;==                ÇÄÄÄÄÄÄÄÄÄÄÄÄÄ¶
;==                º     EBP     º  +00h     Start of VM_TRAP_FRAME (VMTF)
;==                ÌÍÍÍÍÍÍÍÍÍÍÍÍÍ¹ <Ä SS:BP
;==                º Int. Vector º  -02h (word)
;==                ÌÍÍÍÍÍÍÍÍÍÍÍÍÍ¹
;==                º     EBX     º  -06h (dword)
;==                ÌÍÍÍÍÍÍÍÍÍÍÍÍÍ¹
;==                º     ESI     º  -0Ah (dword)
;==                ÌÍÍÍÍÍÍÍÍÍÍÍÍÍ¹
;==                º DS (dummy)  º  -0Ch (word)
;==                ÈÍÍÍÍÍÍÍÍÍÍÍÍÍ¼ <Ä SS:SP
;==
;== Description:
;==	This routine will use the last VMTF on the stack to create a dummy VMTF
;==	which will be used to IRETD to the real mode interrupt handler.  The
;==	real mode stack will include an IRET frame to HWInterEnd, which will
;==	trap to pTrapHandler.  In pTrapHandler, the stack will be cleared,
;==	segment registers will be loaded, and finally an IRETD will return to
;==	the protected mode code which was originally interrupted.
;==
;===============================================================================
;==
;==    PMTF processing (PMTF - Protected Mode Trap Frame)
;==
;== 1) Push segment registers.
;== 2) Subtract size of VMTF from ESP. (VMTF - Virtual Mode Trap Frame)
;== 3) Create dummy VMTF for IRETD. (returns to V86 interrupt service routine)
;== 4) Force real mode stack return to dummy HWInterEnd. (inc real mode SP first!)
;== 5) Push ESP0 into nest stack. (inc nest SP first!)
;== 6) Place ESP plus size of VMTF into ESP0 in TSS.
;== 7) IRETD
;==
;==    HWInterEnd processing
;==
;== 1) Add size of VMTF to ESP (throw VMTF away).
;== 2) Pop nest stack. (dec nest SP AFTER getting value!)
;== 3) Place nest stack value into ESP0 of TSS.
;==
;==    SS:SP (ring 0)     Nest Stack             TSS
;==
;==    ÚÄÄÄÄÄÄÄÄÄÄÄ¿     ÚÄÄÄÄÄÄÄÄÄÄÄ¿      ÚÄÄÄÄÄÄÄÄÄÄÄ¿
;==    ³   VMTF    ³     ³     a     ³      ³           ³
;==  a ÃÄÄÄÄÄÄÄÄÄÄÄ´     ÃÄÄÄÄÄÄÄÄÄÄÄ´      ³           ³
;==    ³ * local * ³     ³     b     ³      ³           ³
;==    ÃÄÄÄÄÄÄÄÄÄÄÄ´     ÃÄÄÄÄÄÄÄÄÄÄÄ´      ³           ³
;==    ³   PMTF    ³     ³     c     ³      ³           ³
;==    ÃÄÄÄÄÄÄÄÄÄÄÄ´     ÃÄÄÄÄÄÄÄÄÄÄÄ´      ÃÄÄÄÄÄÄÄÄÄÄÄ´
;==    ³  Seg Regs ³     ³           ³      ³ ESP0 = x  ³
;==    ÃÄÄÄÄÄÄÄÄÄÄÄ´     ÃÄÄÄÄÄÄÄÄÄÄÄ´      ÀÄÄÄÄÄÄÄÄÄÄÄÙ
;==    ³   VMTF    ³     ³           ³
;==  b ÃÄÄÄÄÄÄÄÄÄÄÄ´     ³           ³
;==    ³ * local * ³     ³           ³
;==    ÃÄÄÄÄÄÄÄÄÄÄÄ´     ³           ³
;==    ³   PMTF    ³     ³           ³
;==    ÃÄÄÄÄÄÄÄÄÄÄÄ´     ÀÄÄÄÄÄÄÄÄÄÄÄÙ
;==    ³  Seg Regs ³
;==    ÃÄÄÄÄÄÄÄÄÄÄÄ´
;==    ³   VMTF    ³
;==  c ÃÄÄÄÄÄÄÄÄÄÄÄ´
;==    ³ * local * ³
;==    ÃÄÄÄÄÄÄÄÄÄÄÄ´
;==    ³   PMTF    ³
;==    ÃÄÄÄÄÄÄÄÄÄÄÄ´
;==    ³  Seg Regs ³
;==  x ÃÄÄÄÄÄÄÄÄÄÄÄ´
;==    ³           ³
;==    ³           ³
;==    ÀÄÄÄÄÄÄÄÄÄÄÄÙ
;==
;===============================================================================
pReflectInterrupt:

	btr	ds:[esi][GenFlags],fEMMhltBit	;Q: Did we issue an sti hlt
	jc	PRI_hlt				; N: normal processing

;
;  Create space in SS0:SP for a VMTF: reflect interrupt to real mode handler
;
	sub	sp,size VM_TRAP_FRAME+8		; allocate space in SP
	sub	bp,size VM_TRAP_FRAME+8
	push	eax
	push	edx

;
;  Copy EBP and interrupt number down to current SP/BP
;
	mov	ebx,[bp][size VM_TRAP_FRAME][4]	; interrupt vect/EBX
	mov	eax,[bp][size VM_TRAP_FRAME][8]	; get EBP
	mov	[bp][-4],ebx
	mov	[bp],eax
	mov	ebx,[bp][size VM_TRAP_FRAME][-4]; ESI/DS (BX=original DS)
	mov	eax,[bp][size VM_TRAP_FRAME][0]	; EBX/ESI
	ror	ebx,16
	mov	[bp][-8],eax
	mov	[bp][-10],bx
;
;  Save segment registers on stack
;
	mov	ax,fs
	mov	bx,es
	shl	eax,16
	mov	ax,gs
	mov	[bp][VTFO][size VM_TRAP_FRAME][4],ebx ; DS/ES from stack
	mov	[bp][VTFO][size VM_TRAP_FRAME][0],eax ; FS/GS
;
;  Get access to _DATA
;
	assume	ds:R_CODE		; DS:[ESI] must point to R_CODE
	mov	eax,[esi][p_DATA]	; DS:[EAX] points to _DATA
	assume	ds:_DATA

;
;  Get pointer to last VMTF on the stack from TSS ESP0
;
	mov	ebx,[eax][TSS].TSS386_ESP0
	sub	bx,[VTFO][size VM_TRAP_FRAME]

;
;  Copy last VMTF into current SS0:SP
;
	mov	edx,dword ptr ss:[bx][VTFO].VMTF_GS
	mov	dword ptr [bp][VTFO].VMTF_GS,edx

	mov	edx,dword ptr ss:[bx][VTFO].VMTF_FS
	mov	dword ptr [bp][VTFO].VMTF_FS,edx

	mov	edx,dword ptr ss:[bx][VTFO].VMTF_DS
	mov	dword ptr [bp][VTFO].VMTF_DS,edx

	mov	edx,dword ptr ss:[bx][VTFO].VMTF_ES
	mov	dword ptr [bp][VTFO].VMTF_ES,edx

	mov	edx,dword ptr ss:[bx][VTFO].VMTF_SS
	mov	dword ptr [bp][VTFO].VMTF_SS,edx

	mov	edx,dword ptr ss:[bx][VTFO].VMTF_ESP
	mov	dword ptr [bp][VTFO].VMTF_ESP,edx

	mov	edx,dword ptr ss:[bx][VTFO].VMTF_EFLAGS
	mov	dword ptr [bp][VTFO].VMTF_EFLAGS,edx
;
;  Make return address to R_CODE:HWInterEnd: traps back to protected mode
;
	mov	dx,offset R_CODE:HWInterEnd	; offset must be < 64K
	movzx	edx,dx
	mov	dword ptr [bp][VTFO].VMTF_EIP,edx

	mov	dx,seg R_CODE			; NOTE: above instruction reset
	mov	dword ptr [bp][VTFO].VMTF_CS,edx; upper 16 bits

;
;  Update ESP0 on TSS so an interrupt in VM doesn't destroy information on stack
;
	movsx	edx,[eax][pLastVMTF]
	inc	edx
	mov	[eax][pLastVMTF],dx
	mov	[eax][LastVMTF][edx*2],bx

	mov	[eax][TSS].TSS386_ESP0,ebp
	add	word ptr [eax][TSS].TSS386_ESP0,[VTFO][size VM_TRAP_FRAME]

;
;  Fall through to vReflectInterrupt
;
	pop	edx
	pop	eax

;===============================================================================
;==
;== vReflectInterrupt: This procedure builds the protected and real mode
;==		       stacks so that an IRETD emulates a real mode interrupt.
;==		       The IRETD will cause the execution of the real mode
;==		       interrupt service.  The real mode stack is modified
;==		       so an IRET at the end of the real mode interrupt service
;==		       routine will cause a return to the code specified by
;==		       the CS:EIP in the VMTF.
;==
;==		       The IF and TF are zeroed when entering the real
;==		       mode interrupt service routine, but preserved when
;==		       returned to the code which was interrupted.
;==
;== Entry: (Protected Mode)
;==	BP    = Points to Virtual/Protected Mode Interrupt Stack Frame (no error)
;==	DS    = DATA32_GSEL (4GB, zero based selector)
;==	ESI   = 32-bit pointer to base of R_CODE segment
;==	SS:SP =
;==                 HIword LOword
;==                ÉÍÍÍÍÍÍÑÍÍÍÍÍÍ»
;==                º 0000 ³  GS  º
;==                ÇÄÄÄÄÄÄÅÄÄÄÄÄÄ¶
;==                º 0000 ³  FS  º  +20h
;==                ÇÄÄÄÄÄÄÅÄÄÄÄÄÄ¶
;==                º 0000 ³  DS  º  +1Ch
;==                ÇÄÄÄÄÄÄÅÄÄÄÄÄÄ¶
;==                º 0000 ³  ES  º  +18h
;==                ÇÄÄÄÄÄÄÅÄÄÄÄÄÄ¶
;==                º 0000 ³  SS  º  +14h
;==                ÇÄÄÄÄÄÄÁÄÄÄÄÄÄ¶
;==                º     ESP     º  +10h
;==                ÇÄÄÄÄÄÄÄÄÄÄÄÄÄ¶
;==                º    EFLAGS   º  +0Ch
;==                ÇÄÄÄÄÄÄÂÄÄÄÄÄÄ¶
;==                º 0000 ³  CS  º  +08h
;==                ÇÄÄÄÄÄÄÁÄÄÄÄÄÄ¶
;==                º     EIP     º  +04h
;==                ÇÄÄÄÄÄÄÄÄÄÄÄÄÄ¶
;==                º     EBP     º  +00h     Start of VM_TRAP_FRAME (VMTF)
;==                ÌÍÍÍÍÍÍÍÍÍÍÍÍÍ¹ <Ä SS:BP
;==                º Int. Vector º  -02h (word)
;==                ÌÍÍÍÍÍÍÍÍÍÍÍÍÍ¹
;==                º     EBX     º  -06h (dword)
;==                ÌÍÍÍÍÍÍÍÍÍÍÍÍÍ¹
;==                º     ESI     º  -0Ah (dword)
;==                ÌÍÍÍÍÍÍÍÍÍÍÍÍÍ¹
;==                º DS (dummy)  º  -0Ch (word)
;==                ÈÍÍÍÍÍÍÍÍÍÍÍÍÍ¼ <Ä SS:SP
;==
;==
;== Exit: (Virtual Mode)
;==	IRETD to real mode interrupt handler.
;==
;===============================================================================
vReflectInterrupt:
	add	sp,2			; throw dummy DS away

	assume	ds:R_CODE		; DS:[ESI] must point to R_CODE
	mov	ebx,[esi][p_DATA]
	assume	ds:_DATA

	cmp	[ebx][DMAActive],0	;Q: Any DMA activity going on?
	jnz	RICheckDMA		; Y: check for DMA TC

RIreflect:
	assume	ds:R_CODE
	test	[esi][GenFlags], fTurnOff
					; Q: Do we ned to set up an 
					; additional IRET frame so that 
					; control is first obtained in 
					; rI15KeyBoard (see int15.asm).
	assume	ds:_DATA
	jnz	vRIsetupI15ret		; Y: set up the additonal frame

;  Create space in real mode (user) stack for the IRET frame
;
	movzx	ebx,[bp][VTFO].VMTF_ESP	; get DS:BX pointing to user stack
	sub	bx,6			; make room for IRET frame
	mov	[bp][VTFO].VMTF_ESP,bx
	push	ax
	movzx	esi,[bp][VTFO].VMTF_SS
	shl	esi,4

RIsetIretFrame:
;
;  Build real mode (user) stack: IP and CS for IRET frame
;
	mov	ax,[bp][VTFO].VMTF_EIP	; IP
	mov	[esi][ebx],ax

	inc	bx
	inc	bx
	mov	ax,[bp][VTFO].VMTF_CS	; CS
	mov	[esi][ebx],ax
;
;  Clear IF and TF to correctly emulate the interrupt
;
	inc	bx
	inc	bx
	mov	ax,[bp][VTFO].VMTF_EFLAGS
	mov	[esi][ebx],ax
	and	ax,not (FLAGS_IF+FLAGS_TF)	; reset IF and TF
	mov	[bp][VTFO].VMTF_EFLAGS,ax

	pop	ax
;
;  Replace CS:EIP of SS0:ESP so IRETD goes to interrupt handler pointed by IVT
;
	movzx	ebx,word ptr [bp][-2]	; interrupt vector
	cmp	bx, FIRST_RMODE_RTN
	jb	RIuseIVT
	sub	bx, FIRST_RMODE_RTN
	mov	bx, cs:[rModeCallTable][ebx*2]
	mov	[bp][VTFO].VMTF_EIP,bx	; move the IP
	mov	bx, seg R_CODE
	shl	ebx, 4			; high word already zeroed above
	assume	ds:R_CODE
	mov	bx, [ebx][segR1_CODE]
	assume	ds:_DATA
	mov	[bp][VTFO].VMTF_CS, bx	; set the CS
	jmp	short RIsetCSIP
RIuseIVT:
	mov	ebx,[ebx*4]
	mov	[bp][VTFO].VMTF_EIP,bx	; move the IP
	shr	ebx,16
	mov	[bp][VTFO].VMTF_CS,bx	; move the CS
RIsetCSIP:
;
;   IRETD back to real mode interrupt handler
;
	pop	esi			; restore local regs
	pop	ebx
	add	sp,2			; throw interrupt number away
	pop	ebp
	iretd

RICheckDMA:
	mov	bx,VDMD_GSEL		; access _DATA
	mov	ds,bx
	call	DMACheckTC		; check DMA operations have complete
	mov	bx,DATA32_GSEL		; restore: DS:[ESI] points to _DATA
	mov	ds,bx
	jmp	RIreflect

vRIsetupI15ret:

	;
	; Here we set up the CS, IP and flags on the (user) stack frame to
	; R_CODE:rI15KeyBoard in int15.asm. Note that sufficient space is
	; reserved on the stack to add the IRET stack frame that tranfers
	; control back to the user.
	;
     ;;;assume	ds:R_CODE
     ;;;and	[esi][GenFlags], NOT fTurnOff
	assume	ds:_DATA

;
;  Create space in real mode (user) stack for the IRET frame
;
	movzx	ebx,[bp][VTFO].VMTF_ESP	; get DS:BX pointing to user stack
	sub	bx,12			; make room for 2 IRET frames
	mov	[bp][VTFO].VMTF_ESP,bx
	push	ax
	movzx	esi,[bp][VTFO].VMTF_SS
	shl	esi,4
;
;  Build real mode (user) stack: IP and CS for IRET frame
;
	mov	ax, OFFSET R_CODE:rI15KeyBoard	; IP
	mov	[esi][ebx],ax

	inc	bx
	inc	bx
	mov	ax, seg R_CODE		; CS
	mov	[esi][ebx],ax
;
;  Clear IF and TF to correctly emulate the interrupt
;
	inc	bx
	inc	bx
	mov	ax,[bp][VTFO].VMTF_EFLAGS
	mov	[esi][ebx],ax
	inc	bx
	inc	bx
	jmp	RIsetIretFrame		; Go set up the IRET frame for 
					; return to user.


ALIGN	16
;===============================================================================
;==
;== ReflectInterrupt:  This procedure prepares the stack for the
;==		       vReflectInterrupt routine which reflects an interrupt
;==		       to the appropriate real mode interrupt service routine.
;==		       Thus, emulating an interrupt occurring in real mode.
;==
;== Entry: (Protected Mode)
;==	BP    = Points to Virtual/Protected Mode Interrupt Stack Frame (no error)
;==	SS:SP =
;==                 HIword LOword
;==                ÉÍÍÍÍÍÍÑÍÍÍÍÍÍ»
;==                º 0000 ³  GS  º
;==                ÇÄÄÄÄÄÄÅÄÄÄÄÄÄ¶
;==                º 0000 ³  FS  º  +20h
;==                ÇÄÄÄÄÄÄÅÄÄÄÄÄÄ¶
;==                º 0000 ³  DS  º  +1Ch
;==                ÇÄÄÄÄÄÄÅÄÄÄÄÄÄ¶
;==                º 0000 ³  ES  º  +18h
;==                ÇÄÄÄÄÄÄÅÄÄÄÄÄÄ¶
;==                º 0000 ³  SS  º  +14h
;==                ÇÄÄÄÄÄÄÁÄÄÄÄÄÄ¶
;==                º     ESP     º  +10h
;==                ÇÄÄÄÄÄÄÄÄÄÄÄÄÄ¶
;==                º    EFLAGS   º  +0Ch
;==                ÇÄÄÄÄÄÄÂÄÄÄÄÄÄ¶
;==                º 0000 ³  CS  º  +08h
;==                ÇÄÄÄÄÄÄÁÄÄÄÄÄÄ¶
;==                º     EIP     º  +04h
;==                ÇÄÄÄÄÄÄÄÄÄÄÄÄÄ¶
;==                º     EBP     º  +00h
;==                ÌÍÍÍÍÍÍÍÍÍÍÍÍÍ¹ <Ä SS:BP
;==                º Int. Vector º
;==                ÈÍÍÍÍÍÍÍÍÍÍÍÍÍ¼ <Ä SS:SP
;==
;==
;== Exit: (Protected Mode)
;==	BP    = Points to Virtual/Protected Mode Interrupt Stack Frame (no error)
;==	DS    = DATA32_GSEL (4GB, zero based selector)
;==	ESI   = 32-bit pointer to base of R_CODE segment
;==	SS:SP =
;==                 HIword LOword
;==                ÉÍÍÍÍÍÍÑÍÍÍÍÍÍ»
;==                º 0000 ³  GS  º
;==                ÇÄÄÄÄÄÄÅÄÄÄÄÄÄ¶
;==                º 0000 ³  FS  º  +20h
;==                ÇÄÄÄÄÄÄÅÄÄÄÄÄÄ¶
;==                º 0000 ³  DS  º  +1Ch
;==                ÇÄÄÄÄÄÄÅÄÄÄÄÄÄ¶
;==                º 0000 ³  ES  º  +18h
;==                ÇÄÄÄÄÄÄÅÄÄÄÄÄÄ¶
;==                º 0000 ³  SS  º  +14h
;==                ÇÄÄÄÄÄÄÁÄÄÄÄÄÄ¶
;==                º     ESP     º  +10h
;==                ÇÄÄÄÄÄÄÄÄÄÄÄÄÄ¶
;==                º    EFLAGS   º  +0Ch
;==                ÇÄÄÄÄÄÄÂÄÄÄÄÄÄ¶
;==                º 0000 ³  CS  º  +08h
;==                ÇÄÄÄÄÄÄÁÄÄÄÄÄÄ¶
;==                º     EIP     º  +04h
;==                ÇÄÄÄÄÄÄÄÄÄÄÄÄÄ¶
;==                º     EBP     º  +00h     Start of VM_TRAP_FRAME (VMTF)
;==                ÌÍÍÍÍÍÍÍÍÍÍÍÍÍ¹ <Ä SS:BP
;==                º Int. Vector º  -02h
;==                ÌÍÍÍÍÍÍÍÍÍÍÍÍÍ¹
;==                º     EBX     º  -06h
;==                ÌÍÍÍÍÍÍÍÍÍÍÍÍÍ¹
;==                º     ESI     º  -0Ah
;==                ÌÍÍÍÍÍÍÍÍÍÍÍÍÍ¹
;==                º DS (dummy)  º  -0Ch (word)
;==                ÈÍÍÍÍÍÍÍÍÍÍÍÍÍ¼ <Ä SS:SP
;==
;== Description:
;== 	Setup stack for reflection via vReflectInterrupt.
;==
;===============================================================================
ReflectInterrupt:
	push	ebx
	push	esi
	sub	sp,2			; dummy ES value

	mov	bx,DATA32_GSEL
	mov	ds,bx

	mov	si,seg R_CODE		; DS:[ESI] points to R_CODE
	movzx	esi,si
	shl	esi,4
	jmp	vReflectInterrupt

PRI_hlt:
	push	eax
	mov	eax, [bp][-4]		; get int vect and high ebx
	mov	[bp][0ch], eax
	mov	eax, [bp][-8]		; get low ebx and high esi
	mov	[bp][8], eax
	mov	eax, [bp][-0ch]		; get low esi and ds
	mov	[bp][4], eax
	pop	eax

	add	bp, 4			; make sp point to ds
	mov	sp, bp
	add	bp, 0ch			; make bp point to ebp
    	jmp	vReflectInterrupt

	assume ds:nothing,es:nothing,fs:nothing,gs:nothing
;==============================================================================
;==
;==  CallRealModeRtn:	This routine 'calls' a real/virtual mode routine from
;==			protected mode code.
;==
;==  Entry:  (Protected Mode)
;==	AX = real mode routine id
;==
;==  Exit:   (Protected Mode)
;==	Registers as set by real mode routine
;==
;==============================================================================
	public	CallRealModeRtn

CallRealModeRtn proc	near

	pushfd				; fake a pMode IRET frame that will
	cli				;   return to this routine
	push	0
	push	cs
	push	0
	push	offset _TEXT:CRMR_back	; offset must be < 64k

	push	ebp			; setup stack the way pIRQHandler
	movzx	ebp, sp 		;   wants it

	push	ax			; routine id

	push	ebx			; setup the regs that pIRQHandler wants
	push	esi
	mov	bx, DATA32_GSEL
	push	ds
	mov	si, seg R_CODE
	mov	ds, bx
	movzx	esi, si
	shl	esi, 4
	jmp	pIRQHandler

	public	CRMR_back
CRMR_back:

	ret

CallRealModeRtn endp

	assume ds:nothing,es:nothing,fs:nothing,gs:nothing
ALIGN	16
;==============================================================================
;==
;==  pTrapHandler: Routine which either reflects S/W interrupt or processes
;==		   a protected mode trap via:
;==
;==		   a) returning to the protected mode code interrupted by a
;==		      HW interrupt.
;==		   b) trapping into protected mode for processing an XMS function
;==
;==  Entry:  (Virtual Mode)
;==
;==
;==  Exit:   (Protected Mode)
;==
;==============================================================================
pTrapHandler:
	push	ebp
	mov	bp,sp
	push	ProtTrap

	push	RCODEA_GSEL
	pop	gs
	assume	gs:R_CODE

	btr	gs:[TrapFlags],fXMMtrapBit	;Q: XMS service request?
	jc	pTHXMM				; Y: service it

	btr	gs:[TrapFlags],fWinTrapBit	;Q: EMM Global Import service request?
	jc	pTHEMMGI			; Y: service it

	btr	gs:[TrapFlags],fSetInstPtrBit	;Q: Set Instance data ptr
	jc	pTHSetInst			; Y: service it

ifdef DEBUG
	btr	gs:[TrapFlags],fpModeDebInitBit ;Q: Protected mode debugger init
	jc	pTHDebInit
endif
	btr	gs:[TrapFlags],fIntEndBit	;Q: End of H/W interrupt
	jnc	ReflectInterrupt		; N: reflect to real mode handler
	jmp	pHWInterEnd			; Y: iret to protected mode service

pTHEMMGI:
	call	pWinEMMGlobImpDisp
	jmp	short pTHexit

pTHSetInst:
	push	ds

	mov	di, DATA32_GSEL
	mov	ds, di

	mov	edi, gs:[pGDT]
	mov	dl, ah
	shr	dl, 4
	shl	eax, 4
	mov	word ptr ds:[edi][R1CODE_GSEL][2], ax
	mov	byte ptr ds:[edi][R1CODE_GSEL][4], dl
	mov	word ptr ds:[edi][R1CODEA_GSEL][2], ax
	mov	byte ptr ds:[edi][R1CODEA_GSEL][4], dl

	add	eax, offset R1_CODE:[InstanceData]
	mov	edi, gs:[p_DATA];
	add	edi,offset _DATA:[Win386VxDRefDat].RDSdata
;;	mov	ds:[Win386VxDRefDat].RDSdata,eax
	mov	ds:[edi],eax
	pop	ds
	jmp	short ptHexit

ifdef DEBUG
pTHDebInit:
	; This conditional breakpoint causes the debugger to break if
	; /B is given on the command line.  Plus it has the side effect
	; of allowing the debugger to initialize the protected mode IDT.

	push	ds
	push	esi
	mov	si, RCODEA_GSEL
	mov	ds, si

	mov	esi,offset R_CODE:[DebBreakStr]
	mov	ax, 0F001h
	int	41h

	pop	esi
	pop	ds
	jmp	short pTHexit
endif

pTHXMM:
	call	pXMMentry

pTHexit:
	add	sp,2
	pop	ebp
	iretd

;===========================================================================
;
;	The foll. are Protect mode handlers for ints 25h, 26h, 2ah, 2fh
;	5ch and 33h. These handlers will just reflect the interrupts
;	thru the real mode IDTs.
;
;===========================================================================

pINT25hHandler	proc	far

	push	ebp
	mov	bp, sp
	push	25h
	jmp	ReflectInterrupt

pint25hHandler	endp

pINT26hHandler	proc	far

	push	ebp
	mov	bp, sp
	push	26h
	jmp	ReflectInterrupt

pint26hHandler	endp

pINT2ahHandler	proc	far

	push	ebp
	mov	bp, sp
	push	2ah
	jmp	ReflectInterrupt

pint2ahHandler	endp

pINT2fhHandler	proc	far

	push	ebp
	mov	bp, sp
	push	2fh
	jmp	ReflectInterrupt

pint2fhHandler	endp


pINT33hHandler	proc	far

	push	ebp
	mov	bp, sp
	push	33h
	jmp	ReflectInterrupt

pint33hHandler	endp

pINT5chHandler	proc	far

	push	ebp
	mov	bp, sp
	push	5ch
	jmp	ReflectInterrupt

pint5chHandler	endp

ALIGN	16
;===============================================================================
;==
;==    PMTF processing (PMTF - Protected Mode Trap Frame)
;==
;== 1) Push segment registers.
;== 2) Subtract size of VMTF from ESP. (VMTF - Virtual Mode Trap Frame)
;== 3) Create dummy VMTF for IRETD. (returns to V86 interrupt service routine)
;== 4) Force real mode stack return to dummy HWInterEnd. (inc real mode SP first!)
;== 5) Push ESP0 into nest stack. (inc nest SP first!)
;== 6) Place ESP plus size of VMTF into ESP0 in TSS.
;== 7) IRETD
;==
;==    HWInterEnd processing
;==
;== 1) Add size of VMTF to ESP (throw VMTF away).
;== 2) Pop nest stack. (dec nest SP AFTER getting value!)
;== 3) Place nest stack value into ESP0 of TSS.
;==
;==    SS:SP (ring 0)     Nest Stack             TSS
;==
;==    ÚÄÄÄÄÄÄÄÄÄÄÄ¿     ÚÄÄÄÄÄÄÄÄÄÄÄ¿      ÚÄÄÄÄÄÄÄÄÄÄÄ¿
;==    ³   VMTF    ³     ³     a     ³      ³           ³
;==  a ÃÄÄÄÄÄÄÄÄÄÄÄ´     ÃÄÄÄÄÄÄÄÄÄÄÄ´      ³           ³
;==    ³ * local * ³     ³     b     ³      ³           ³
;==    ÃÄÄÄÄÄÄÄÄÄÄÄ´     ÃÄÄÄÄÄÄÄÄÄÄÄ´      ³           ³
;==    ³   PMTF    ³     ³     c     ³      ³           ³
;==    ÃÄÄÄÄÄÄÄÄÄÄÄ´     ÃÄÄÄÄÄÄÄÄÄÄÄ´      ÃÄÄÄÄÄÄÄÄÄÄÄ´
;==    ³  Seg Regs ³     ³           ³      ³ ESP0 = x  ³
;==    ÃÄÄÄÄÄÄÄÄÄÄÄ´     ÃÄÄÄÄÄÄÄÄÄÄÄ´      ÀÄÄÄÄÄÄÄÄÄÄÄÙ
;==    ³   VMTF    ³     ³           ³
;==  b ÃÄÄÄÄÄÄÄÄÄÄÄ´     ³           ³
;==    ³ * local * ³     ³           ³
;==    ÃÄÄÄÄÄÄÄÄÄÄÄ´     ³           ³
;==    ³   PMTF    ³     ³           ³
;==    ÃÄÄÄÄÄÄÄÄÄÄÄ´     ÀÄÄÄÄÄÄÄÄÄÄÄÙ
;==    ³  Seg Regs ³
;==    ÃÄÄÄÄÄÄÄÄÄÄÄ´
;==    ³   VMTF    ³
;==  c ÃÄÄÄÄÄÄÄÄÄÄÄ´
;==    ³ * local * ³
;==    ÃÄÄÄÄÄÄÄÄÄÄÄ´
;==    ³   PMTF    ³
;==    ÃÄÄÄÄÄÄÄÄÄÄÄ´
;==    ³  Seg Regs ³
;==  x ÃÄÄÄÄÄÄÄÄÄÄÄ´
;==    ³           ³
;==    ³           ³
;==    ÀÄÄÄÄÄÄÄÄÄÄÄÙ
;==
;===============================================================================
pHWInterEnd:
	push	eax
	push	ebx

	mov	ax,DATA32_GSEL
	mov	ds,ax

	mov	ax,seg R_CODE
	movzx	eax,ax
	shl	eax,4			; DS:[ESI] point to R_CODE
	assume	ds:R_CODE

	assume	ds:R_CODE
	mov	eax,[eax][p_DATA]	; DS:[EAX] points to _DATA
	assume	ds:_DATA

	movsx	ebx,[eax][pLastVMTF]
	movzx	ebp,[eax][LastVMTF][ebx*2]
	dec	bx
	mov	[eax][pLastVMTF],bx

	add	bp,[VTFO][size VM_TRAP_FRAME]
	mov	[eax][TSS].TSS386_ESP0,ebp

	pop	ebx
	pop	eax

;
;  Need to restore stack
;
	add	sp,2			; throw interrupt number away
	pop	ebp			; restore EBP
	add	sp,[size VM_TRAP_FRAME]	; throw VMTF away
;
;  Restore segment registers & return to protected mode code interrupted
;
	pop	gs
	pop	fs
	pop	es
	pop	ds
	iretd

;==============================================================================
;==
;==  pIRQ5xHandler: H/W protected mode interrupt handlers.  These interrupt
;==		    handlers are used when an application programs the master
;==		    PIC while CEMM is ON/AUTO. (i.e. DesqView)
;==
;==  Entry:  (Protected Mode via 386 Interrupt gate)
;==	SS:SP = Depending on the processor mode when the interrupt occurred:
;==		1) V8086 mode: the virtual mode 8088 stack frame
;==		2) Protected mode: 32-bit EIP, CS, & EFLAGS.
;==
;==  Exit:   EBP pushed on stack
;==	BP = stack frame pointer
;==
;==============================================================================
ALIGN 16
pIRQ50Handler:
	push	ebp
	push	50h
	jmp	short pIRQ5xHandlerN

pIRQ51Handler:
	push	ebp
	push	51h
	jmp	short pIRQ5xHandlerN

pIRQ52Handler:
	push	ebp
	push	52h
	jmp	short pIRQ5xHandlerN

pIRQ53Handler:
	push	ebp
	push	53h
	jmp	short pIRQ5xHandlerN

pIRQ54Handler:
	push	ebp
	push	54h
	jmp	short pIRQ5xHandlerN

pIRQ55Handler:
	push	ebp
	push	55h
	jmp	short pIRQ5xHandlerN

pIRQ56Handler:
	push	ebp
	push	56h
	jmp	short pIRQ5xHandlerN

pIRQ57Handler:
	push	ebp
	push	57h

pIRQ5xHandlerN:
	mov	bp,sp
	push	ebx
	push	esi
	push	ds

	mov	si,DATA32_GSEL
	mov	ds,si

	mov	si,seg R_CODE
	movzx	esi,si
	shl	esi,4				; DS:[ESI] point to R_CODE
	assume	ds:R_CODE

	add	bp,2
	jmp	pIRQHandler

ifndef LC910610
;==============================================================================
;==
;==  pIRQ1Handler: H/W protected mode interrupt handler for the keyboard
;==		   controller (8042).  This will allow CEMM to virtualize the
;==		   8042 data register (port 60h).  Three modes of operation will
;==		   exist:
;==		   1) Reflect the IRQ1 to the appropriate ISR.  Every time a
;==		      scan code is read from the 8042 data register, CEMM traps
;==		      the I/O read, reads the physical 8042 data register and
;==		      passes the data (scan code) to the VDM code.  This is the
;==		      traditional method used by CEMM. (default)
;==		   2) Reflect the IRQ1 to the appropriate ISR.  CEMM will not
;==		      trap accesses to the 8042.  This method may cause the
;==		      system to crash if somebody diddles with the A20 line via
;==		      port 60h and 64h.
;==		   3) Use IRQ1 as a flag to read the scan code into a virtual
;==		      8042 data register.  This forces the 8042 data register
;==		      to be read only once per IRQ1.  This fixes problems caused
;==		      by multiple TSRs reading scan codes and thus causing
;==		      early scan code propagation.  This technique will only
;==		      work if the keyboard ISR is interrupt driven.  If an ISR
;==		      is polling for scan codes, this technique will cause a
;==		      system hang.
;==
;==  Entry:  (Protected Mode via 386 Interrupt gate)
;==	SS:SP = Depending on the processor mode when the interrupt occurred:
;==		1) V8086 mode: the virtual mode 8088 stack frame
;==		2) Protected mode: 32-bit EIP, CS, & EFLAGS.
;==
;==  Exit:   EBP pushed on stack
;==	BP = stack frame pointer
;==
;==============================================================================
ALIGN 16
pIRQ1Handler:
	push	ebp			; save base pointer
	push	1			; IRQ1

	movzx	ebp,sp			; set base pointer
	push	ebx			; save local registers
	push	esi
	push	ds
	mov	si,DATA32_GSEL
	mov	ds,si

	mov	si,seg R_CODE
	movzx	esi,si
	shl	esi,4			; DS:[ESI] point to R_CODE
	assume	ds:R_CODE

	mov	ebx,[esi][p_DATA]	; DS:[EBX] points to _DATA
	assume	ds:_DATA
	mov	[ebx][IRQ1Event],TRUE	; set flag indicating IRQ1 ocurred
	assume	ds:R_CODE

ifdef PICtrap
	mov	bx,[esi][VirPICVec][0]
else
	mov	bx,[esi][PICVec][0]	; get base address for master PIC
endif
	add	[bp],bx			; adjust interrupt number on stack
	add	bp,2			; base pointer pointing to VMTF
	jmp	pIRQHandler		; reflect interrupt
endif

_TEXT	ends
;=============================================================================
;==	C O D E  S E G M E N T
;=============================================================================
R_CODE	segment
	assume cs:R_CODE,ds:nothing,es:nothing,fs:nothing,gs:nothing
;==============================================================================
;==
;==  HWInterEnd: Routine which traps to protected mode code executing during
;==		 last H/W interrupt.
;==
;==  Entry:  (Virtual Mode)
;==
;==
;==  Exit:   (Protected Mode)
;==
;==============================================================================
HWInterEnd:
	cli				; make sure ProtTrap occurs w/out interrupt
	or	cs:[TrapFlags],fIntEnd	; end of H/W interrupt
	int	ProtTrap		; will never return
	cli
	hlt

ifdef PICtrap
;==============================================================================
;==
;==  rIRQxHandler: H/W real mode interrupt handlers.  These routines will
;==		   reflect HW interrupts to the H/W interrupt service handlers.
;==
;==  Entry:  (Real Mode)
;==
;==
;==  Exit:   (Real Mode)
;==		H/W Interrupt service handler has been executed.
;==
;==============================================================================
rIRQ0Handler:
	push	0
	jmp	short rIRQHandlerMaster

rIRQ1Handler:
	push	1
	jmp	short rIRQHandlerMaster

rIRQ2Handler:
	push	2
	jmp	short rIRQHandlerMaster

rIRQ3Handler:
	push	3
	mov	bl,3
	jmp	short rIRQHandlerMaster

rIRQ4Handler:
	push	4
	jmp	short rIRQHandlerMaster

rIRQ5Handler:
	push	5
	jmp	short rIRQHandlerMaster

rIRQ6Handler:
	push	6
	jmp	short rIRQHandlerMaster

rIRQ7Handler:
	push	7
	jmp	short rIRQHandlerMaster

rIRQ8Handler:
	push	0
	jmp	short rIRQHandlerSlave

rIRQ9Handler:
	push	1
	jmp	short rIRQHandlerSlave

rIRQ10Handler:
	push	2
	jmp	short rIRQHandlerSlave

rIRQ11Handler:
	push	3
	jmp	short rIRQHandlerSlave

rIRQ12Handler:
	push	4
	jmp	short rIRQHandlerSlave

rIRQ13Handler:
	push	5
	jmp	short rIRQHandlerSlave

rIRQ14Handler:
	push	6
	jmp	short rIRQHandlerSlave

rIRQ15Handler:
	push	7
	jmp	short rIRQHandlerSlave

rIRQHandlerMaster:
	push	bx
	mov	bx,sp
	mov	bx,ss:[bx][2]
	add	bx,DOS_MASTER_VECTOR

rIRQHandler:
	push	ds

	mov	ds,cs:[ZERO]
	xor	bh,bh
	shl	bx,2

	pushf
	call	dword ptr [bx]

	pop	ds
	pop	bx
	add	sp,2
	iret

rIRQHandlerSlave:
	push	bx

	mov	bx,sp
	mov	bx,ss:[bx][2]

	add	bx,DOS_SLAVE_VECTOR
	cmp	cs:[SlavePicVec],DOS_SLAVE_VECTOR
	jne	short rIRQHandler

	sub	bx,DOS_SLAVE_VECTOR
	xor	bh,bh
	shl	bx,2

	pushf
	call	dword ptr cs:[OldSlaveIRQHandler][bx]
	pop	bx
	add	sp,2
	iret

;==============================================================================
;==
;==  Must hook INT 21h for Phar-Lap's DOS Extender. (Look like DesqView)
;==
;==  A bug in DOS Extender does not allow reprogramming of the PICs by CEMM.
;==  The problem has been fixed for QuarterDeck's DesqView.  Thus by hooking
;==  INT 21h and responding a DesqView signature to DOS Extender apps,
;==  this should fix the problem.
;==
;==  NOTE: INT 21h is not hooked if PIC is not programmed by CEMM.
;==
;==============================================================================
rINT21hHandler:
	cmp	ah,2Bh
	jne	short rI21Hchain
	cmp	cx,'DE'
	jne	short rI21Hchain
	cmp	dx,'SQ'
	jne	short rI21Hchain

	test	cs:[Current_State],fState_Active ;Q: in Virtual mode ?
	jz	short rI21Hchain		 ; N: don't need to process

;
;QLEO: Need to add code to detect a PharLap DOS Extender App (Get PSP and
;      look for Copyright information.  If it is a PharLap App, return
;      BX=0215h (version 2.21).
;
	push	ax	; save registers
	push	bx
	push	cx
	push	si
	push	di
	push	ds
	push	es

	push	cs	; access R_CODE
	pop	ds

	mov	ah,62h	; get PSP address in BX
	int	21h
	mov	es,bx

	lea	si,R_CODE:PharLapCopy
	mov	di,PharLapOffs
	mov	cx,PharLapCopyLen
	cld
;
;  Is Pharlap Copyright notice in PSP?
;
	repe cmpsb		;Q: Is Copyright at 100h into PSP?
	jne short rI21Hexit 	; N: no, not a Phar Lap app, chain to DOS
				; Y: skip dates, and look for Phar Lap.
	mov	al,[PharLapName]; search for "P"
	mov	cx,20h		; skip up to 32 bytes worth of dates
	repne scasb		; find "P" of Phar Lap
	dec	di		; incase it was found, return to "P"

	lea	si,R_CODE:PharLapName ; compare to "Phar Lap Software, Inc."
	mov	cx,PharLapCopyLen
	repe cmpsb		;Q: Is it a Phar Lap Dos Extender application?

rI21Hexit:
	pop	es	; restore registers
	pop	ds
	pop	di
	pop	si
	pop	cx
	pop	bx
	pop	ax

	jne	short rI21Hchain 	;N: no, not a Phar Lap app, chain to DOS
	mov	bx,0215h		;Y: return version 2.21
	iret

rI21Hchain:
	jmp	dword ptr cs:[OldInt21hHandler]
endif

R_CODE	ends
;=============================================================================
;==	C O D E  S E G M E N T
;=============================================================================
LAST	segment
	assume	cs:LAST,ds:_DATA,es:_DATA,gs:R_CODE
;==============================================================================
;==
;==  PICInit: This routine initializes necessary data structures for
;==	      handling H/W interrupts and trapping PIC programming in
;==	      virtual 8088 mode.  Places correct IDT interrupt gates
;==	      for the H/W interrupt handlers.
;==
;==  Entry: (Real Mode)
;==	DS = _DATA
;==	GS = R_CODE
;==
;==  Exit:  (Real Mode)
;==
;==============================================================================
PICInit	proc	near
	push	fs

ifdef PICtrap
	test	gs:[GenFlags],fNoINT	;Q: Need to trap PIC?
	jnz	short PIvectors		; N: just update IVT and IDT
;
; The addresses of the PIC ports are set in the IO bit map so that accesses
; to them while in virtual 8088 mode will be trapped.
;
	xor	bx,bx			; 64K IO address space

	test	gs:[GenFlags],fEISA+fMCA;Q: EISA or MCA machine?
	jnz	short PIPorts		; Y: continue

ifdef	ROMIDMCA
	cmp	[ROMID],ROMIDPS2	;Q: MCA machine?
	je	short PIPorts		; Y: continue
endif
;
;  ISA system: Alias every 1K of IO space
;
	mov	bx,8000h		; 1K IO address space

PIPorts:
;
;  Trap PIC ports
;
	mov	cx,TOTAL_PIC_PORTS 	; trap EISA DMA ports
	xor	esi,esi
PISetPorts:
	mov	ax,gs:[PICPortList][esi*2]
	call	_TEXT:PortTrapFar
	inc	si
	loop	PISetPorts

;
;  Place real/protected mode handlers in IVT/IDT: Master/Slave PIC
;
PIvectors:
endif
	movzx	ebx,gs:[MasterPICVec]	; base vector for master PIC
	movzx	edx,gs:[SlavePICVec]	; base vector for slave PIC

ifdef PICtrap
	cmp	bx,DOS_MASTER_VECTOR	;Q: Will CEMM move master PIC?
	jz	short PIslave		; N: how about slave?
	or	[PICFlags][0],fPICMoved	; Y: CEMM has moved master PIC vectors
PIslave:
	cmp	dx,DOS_SLAVE_VECTOR	;Q: Will CEMM move master PIC?
	jz	short PIhandle		; N: continue
	or	[PICFlags][2],fPICMoved	; Y: CEMM has moved slave PIC vectors
PIhandle:
endif
	xor	esi,esi

	mov	cx,IDT
	mov	fs,cx			; FS = IDT (protected mode)
	assume	fs:IDT

	mov	ax,VDMC_GSEL		; master PIC (IDT)
	shl	eax,16

	shl	bx,3			; convert vector to offset in IDT
	shl	dx,3
	mov	cx,8			; 8 interrupts per PIC
PIIRQHandlers:
;
;  Place protected mode interrupt gates in IDT
;
	mov	ax,cs:[pIRQ5xHandlers][esi*2]		; IRQs at 50h
	mov	fs:[50h*8][esi*8],eax
	mov	byte ptr fs:[50h*8][esi*8][5],D_386INT0

ifdef PICtrap
	test	[PICFlags],fPICMoved	;Q: Master PIC handlers needed?
	jz	short PIcont		; N: don't need them (exception handlers)

	mov	ax,cs:[pMasterIRQHandlers][esi*2]
	mov	fs:[ebx][esi*8],eax
	mov	byte ptr fs:[ebx][esi*8][5],D_386INT0
PIcont:
endif
	mov	ax,cs:[pSlaveIRQHandlers][esi*2]	; slave PIC (IDT)
	mov	fs:[edx][esi*8],eax
	mov	byte ptr fs:[edx][esi*8][5],D_386INT0

	inc	si
	loop	PIIRQHandlers

;
;  Place multi-purpose protected mode trap selector
;
	lea	ax,[pTrapHandler]			; into protected mode
	mov	fs:[ProtTrap*8],eax			; save VDMC_GSEL:pTrapHandler
	mov	byte ptr fs:[ProtTrap*8][5],D_386INT3	; accessible via S/W

	pop	fs
	ret
PICInit	endp

;==============================================================================
;==
;==  PICVecInit: This routine places correct IVT vectors for the H/W
;==		 interrupt handlers.
;==
;==  Entry: (Real Mode)
;==	GS = R_CODE
;==
;==  Exit:  (Real Mode)
;==
;==============================================================================
PICVecInit proc	near
	push	es

ifdef PICtrap
;
;  Place real mode handlers in IVT: Master/Slave PIC
;
	movzx	ebx,gs:[MasterPICVec]	; base vector for master PIC
	movzx	edx,gs:[SlavePICVec]	; base vector for slave PIC
	xor	esi,esi

	mov	es,si			; ES = IVT (real mode)
	mov	cx,IDT
	assume	es:ABS0

	shl	bx,2			; convert vector to offset in IVT
	shl	dx,2
	mov	cx,8			; 8 interrupts per PIC

PVIIRQHandlers:
;
;  Place real mode vectors in IVT
;
	cmp	gs:[MasterPICVec],DOS_MASTER_VECTOR ;Q: Master PIC handlers needed?
	je	short PVIslave			    ; N: don't need them

	mov	ax,R_CODE				; master PIC (IVT)
	shl	eax,16
	mov	ax,cs:[rMasterIRQHandlers][esi*2]
	mov	es:[ebx][esi*4],eax

PVIslave:
	cmp	gs:[SlavePICVec],DOS_SLAVE_VECTOR ;Q: Slave PIC handlers needed?
	je	short PVIcont			    ; N: don't need them

	mov	ax,R_CODE				; slave PIC (IVT)
	shl	eax,16
	mov	ax,cs:[rSlaveIRQHandlers][esi*2]
	xchg	es:[edx][esi*4],eax
	mov	gs:[OldSlaveIRQHandler][esi*4],eax	; save original vector
PVIcont:
	inc	si
	loop	PVIIRQHandlers

	cmp	gs:[MasterPICVec],DOS_MASTER_VECTOR ;Q: Master PIC handlers needed?
	jne	short PVIhook			    ; Y: hook INT 21h
	cmp	gs:[SlavePICVec],DOS_SLAVE_VECTOR ;Q: Slave PIC different?
	je	short PVIexit			  ; N: don't hook
PVIhook:
;==============================================================================
;==
;==  Must hook INT 21h for Phar-Lap's DOS Extender. (Look like DesqView)
;==
;==  A bug in DOS Extender does not allow reprogramming of the PICs by CEMM.
;==  The problem has been fixed for QuarterDeck's DesqView.  Thus by hooking
;==  INT 21h and responding a DesqView signature to DOS Extender apps,
;==  this should fix the problem.
;==
;==============================================================================
	mov	eax,es:[int21]			; get original vector
	mov	gs:[OldInt21hHandler],eax	; save it
	mov	ax,R_CODE			; CEMM's Int21h handler in IVT
	shl	eax,16
	lea	ax,R_CODE:[rINT21hHandler]
	mov	es:[int21],eax
PVIexit:
endif
	pop	es
	ret
PICVecInit	endp

LAST	ends

	end

