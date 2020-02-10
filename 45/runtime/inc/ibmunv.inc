;***
;ibmunv.inc - 20-Mar-86
;***
	.XLIST
;***
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;******************************************************************************

;******************************************************************************

;	Chess ROS I/O Macro Definitions for the IBM PC

;******************************************************************************

;	VECTOR INTERRUPT EQUATES

;******************************************************************************

	I_SCNIO=10H 		;VIDEO_IO
	I_CASIO=15H 		;CASSETTE_IO
	I_KYBIO=16H 		;KEYBOARD_IO
	I_PTRIO=17H 		;PRINTER_IO
	I_DOSIO=21H 		;MSDOS_IO

;******************************************************************************

;	ROS MONITOR INTERRUPT CALL MACRO DEFINITIONS

;******************************************************************************

; The ROSMAC macro is used to define other macros. These new macros can then
; be used to access the IBM ROM bios level services. Refer to the IBM TechRef
; Manual for additional information.

ROSMAC	MACRO	NAM,REG
NAM	MACRO	FUNC
IFNB	<REG>
	PUSH	BP
	PUSH	SI
	PUSH	DI
ENDIF				;IFNB <REG>
IFNB	<FUNC>
	MOV	AH,FUNC
ENDIF				;IFNB <FUNC>
	INT	I_&NAM
IFNB	<REG>
	POP	DI
	POP	SI
	POP	BP
ENDIF				;IFNB <REG>
	ENDM			;&NAM	MACRO
	ENDM			;ROSMAC	MACRO

; The SCNIOS macro is used to save a bit of code. Invoking this macro
; causes a call to the B$SCNIO routine rather than invoking the SCNIO
; macro directly. This saves 3 PUSHes and 3 POPs for all screen
; related activity.

SCNIOS	MACRO	FUNC		;;VIDEO_IO
IFNB	<FUNC>
	MOV	AH,FUNC
ENDIF
	CALL	B$SCNIO
	ENDM

KYBIO	macro	func		;;[1]
	PUSH	BP		;;[1]
	PUSH	SI		;;[1]
	PUSH	DI		;;[1]
ifnb	<func>			;;[1]
	MOV	AH,FUNC		;;[1]
  ifndef	b$RcoFlg	;;[1]
	extrn	b$RcoFlg:byte	;;[1]
  endif				;;[1] ifndef
	OR	AH,b$RcoFlg	;;[1] b$RcoFlg is either 0 or 10H in DOS3
endif				;;[1] ifnb <func>
	INT	I_KYBIO		;;[1]
	POP	DI		;;[1]
	POP	SI		;;[1]
	POP	BP		;;[1]
ENDM				;;[1] KYBIO

	ROSMAC	SCNIO,R		;;VIDEO_IO
	ROSMAC	CASIO		;;CASSETTE_IO
	ROSMAC	PTRIO,R		;;PRINTER_IO
	ROSMAC	DOSIO		;;MSDOS_IO

;******************************************************************************

;	BIOS Video Support Functions (use with SCNIO macro):

;******************************************************************************

	vSetMode EQU	0		;set screen mode
	vSetCursorType EQU	1	;set cursor type
	vSetCursorPos EQU	2	;set cursor position
	vReadCursorPos EQU	3	;read cursor position
	vReadLightPen EQU	4	;read light pen position
	vSelActivePage EQU	5	;select active display page
	vScrollPageUp EQU	6	;scroll active page up
	vScrollPageDown EQU	7	;scroll active page down
	vReadChar EQU	8		;read character and attribute
	vWriteChar EQU	9h	;write character and attribute
	vWriteCharOnly EQU	0ah	;write character only
	vSetPalette EQU	0bh	;set color palette
	vWritePixel EQU	0ch	;write pixel to screen
	vReadPixel EQU	0dh	;read pixel from screen
	vWriteTTY EQU	0eh	;write tty to active page
	vGetVideoState EQU	0fh	;gets current video state
	vSetEgaPalette EQU	010h	;set palette registers
	vCharGen EQU	011h	;character generator routine
	vAltSelect EQU	012h	;alternate select - get ega info
	vWriteString EQU	013h	;write string to screen
	vGetVgaInfo EQU 01bh	;[5]get vga information

;******************************************************************************

;	BIOS Cassette Support Functions (use with CASIO macro):

;******************************************************************************

	cMotorOn EQU	0		;turn cassette motor on
	cMotorOff EQU	1		;turn cassette motor off
	cReadCassette EQU	2	;read block from cassette device
	cWriteCassette EQU	3	;write block to cassette device

;******************************************************************************

;	BIOS Keyboard Support Functions (use with KYBIO macro):

;******************************************************************************

	kReadKeyboard EQU	0	;read character from keyboard
	kPollKeyboard EQU	1	;poll keyboard for presence of character
	kReadShiftKeys EQU	2	;get status of shift keys

;******************************************************************************

;	BIOS Printer Support Functions (use with PTRIO macro):

;******************************************************************************

	lptPrintChar EQU	0	;print character on printer
	lptInitPrinter EQU	1	;initialize printer device
	lptReadStatus EQU	2	;read status of printer device

;******************************************************************************

;	BIOS RS232C COM Port Support Functions:

;******************************************************************************

	comInitPort EQU	0	;initialize COM port
	comSendByte EQU	1	;send byte to COM port
	comRecByte EQU	2	;receive byte from COM port
	comGetStatus EQU	3	;get COM port status info

;******************************************************************************

;	Interrupts / Interrupt Vectors / I/O Ports / Misc Constants

;	The following Offsets are for Cells assuming DS:0

;******************************************************************************

;	Timer Constants:

	TIMINT	= 8H		;timer interrupt
	CLKINT	= 20h		;X'20' Clock Interrupt Vector
	CLKVEC	= 3C0h		;X'3C0' (ROM) Clock Interrupt vector
	ROMCLK	= 240		;ROM Clock Interrupt number
	TIMADR	= 70H		;X'70' Timer Int Vector
	TICSAV	= 512H		;X'512-515' Clock Int Vec save area
	TMRCTL	= 1CH		;timer control interrupt
	TIMER0	= 40h		;X'40' Timer 0 Port Address
	TIMER2	= 42h		;X'42' Timer 2 Port Address
	TMRCMD	= 43h		;X'43' Timer Control Port

;	8259 constants:

	INTA0	= 20h		;X'20' 8259 Interrupt Control Port
	INTA1	= 21H		;8259 Interrupt Mask Register Port
	EOI	= 20h		;X'20' 8259 End-of-Interrupt ACK
	MSKREG	= 21H		;8259 interrupt mask register

;	Joystick Event Constants:

	GAMECD	= 201H		;Game Card I/O address
	TRIG_A1	= 10H		;Trigger 'A1' Mask, Active Low
	TRIG_A2	= 20H		;Trigger 'A2' Mask, Active Low
	TRIG_B1	= 40H		;Trigger 'B1' Mask, Active Low
	TRIG_B2	= 80H		;Trigger 'B2' Mask, Active Low

;	Music / Sound constants:

	SPEAKER	= 61h		;X'61' Speaker Latch Port Address.
	SpkrOn	= 3		;Speaker On Mask
	SpkrEn	= 10h		;Speaker Enable/Disable Mask
	SQUARE	= 0B6h		;X'B6' Timer 2, Square Wave (Mode 3).
	SND_MAX	= 32		;Max number of Notes Que can hold
	TIADDR	= 0C0H		;X'C0' TI sound chip address

;	Keyboard Constants:

	KBDINT	= 24H		;X'24' Keyboard Interrupt Vector
	KBDVEC	= 3BCH		;X'3BC' (ROS) KBD Int Vector
	KYBINT	= 9H		;keyboard interrupt
	ROMKBD	= 239D		;ROM KBD INT vectors saved at 239*4
	KBDFLG	= 417H		;keyboard special status flag
	KBDATA	= 60H		;keyboard data

;	Screen related constants:

	PALSIZ	= 16		;palette size
	CHREXT	= 7Ch		;X'7C' Extended Character Set pointer.
	CRT_LEN = 44Ch		;[12] crt page length
	CRT_START= 44eh		;crt start address
	ACTIVE_PAGE = 462H	;[4] Active page# is stored here by BIOS
	BIOS_CursorPos = 450H	;[9]start of BIOS cursor position table
	CGA_STATUS = 3DAH	;[9]CGA card status port
	VERT_SYNC = 8		;[9] 1 on status port means vertical sync
	HORZ_SYNC = 1		;[9] 1 on status port means horizontal sync
	MEMRIO	= 23Ch		;Port for MS Jr Booster Card signature byte
	CR	= 13D		;ASCII carriage return
	LF	= 10D		;ASCII  line feed
	BiosEquip= 410h		;bios equipment register


;[7]	This macro is used to choose between word OUTs and byte OUTs when
;[7]	writing to the EGA card.  The routine B$OutWord does the equivalent
;[7]	of a word out 1 byte at a time because of rumoured problems with
;[7]	word OUTs.  Currently, the AT&T 6300 is the only machine known to
;[7]	have problems with word OUTs.  If it is decided that the speed gained
;[7]	by using word OUTs is more important than supporting the AT&T thing,
;[7]	change this macro and remove B$OutWord from LLCGRP.

OutWord MACRO
	CALL	B$OutWord	; or this line can read "OUT DX,AX"
;	OUT	DX,AX		; or this line can read "CALL B$OutWord"
	ENDM



;	EGA (Enhanced Graphics Adapter) constants:


	OFFSCN=6D60H 		;first even off-screen location
				;we need an off-screen location in
				; video memory for writing a tiling
				; pattern

;	Accessing screen memory through the EGA card is handled by reading
;	or writing either directly from/to memory or through a set of latch
;	registers which represent each of the four color memory planes which
;	are supported.  This access is controlled by sending information in
;	the form of binary numbers or bit patterns to the data port associated
;	with the graphics registers. The graphics register selected to receive
;	the data is specified as an offset index, this offset being written to
;	an address port associated with a particular data port.

;	The following definitions and routines are a convenient means for
;	coding these staged interactions with the hardware.

;	Addresses of, or offsets to, registers used :

;	SEQUENCER -----------------------------------------------------

	SEQADD=3C4H 		;sequencer address register(takes index)
	SEQDAT=3C5H 		;sequencer data port (receives data for
				; register indexed above)
	MMREG=2			;offset to map mask register
				;/ ENABLES PLANES 0-3
				;/ FOR 32-BIT WRITE
	MMDREG=4 		;memory mode register

;	GRAPHICS --------------------------------------------------------

	GRPADD=3CEH 		;graphics address register
	GRPDAT=3CFH 		;graphics data port
	SRSREG=0 		;set/reset register
				;/ IF WRITE MODE 0, BINARY
				;/ VALUE OF COLOR TO WRITE
	ENBREG=1 		;enable set/reset register
				;/ BITS 0-3 ENABLE PLANES
				;/ FOR WRITING FROM SETRES
				;/ IF 0, WRITE IS FROM
				;/ PROCESSOR DATA
	CLCREG=2 		;color compare register
				;/ BITS 0-3 ARE COLOR VALUE
				;/ TO BE COMPARED
	DTRREG=3 		;data rotate register
				;/ BITS 0-2 = POSITIONS TO
				;/ ROTATE FOR WRITE
				;/ BITS 3-4 = LOGICAL OPS
				;/ WITH LATCHED DATA
	RMPREG=4 		;read map select register
				;/ BINARY REP OF PLANE FOR
				;/ READ
	RWMREG=5 		;read/write mode register
				;/ WRITES : BITS 1 0
				;/ 00 = DATA WRITE FROM
				;/ PROCESSOR OR SET-RESET
				;/ 01 = LATCH WRITE
				;/ 10 = BYTE <-- COLOR
				;/ READS : BIT 3
				;/ 0 = READ FROM PLANE
				;/ SPECIFIED BY READMP
				;/ 1 = COMPARE PIXEL COLOR
				;/ WITH COLOR COMPARE
				;/ ODD/EVEN : BIT 4
				;/ 1 = ODD/EVEN ADDRESS MODE
	CDCREG=7 		;color don't care register
				;/ BITS 0 - 3 REPRESENT 4
				;/ PLANES; 0 SPECIFIES IGNORE
				;/ PLANE WHEN DOING COLOR
				;/ COMPARE READ
	BMKREG=8 		; bit mask register
				;/ BITS 0 - 7 WHEN SET TO
				;/ 0 PROTECT BIT FROM WRITE

;	ATTRIBUTE -----------------------------------------------------

	ATTADD=3C0H 		;attribute address register - used also for data
				;via alternating OUTs


;[8] Constants and macros used by EGAINT10 interface.

EGAINT	MACRO	FUNC		;[8]run EGA outs through egaint10 interface
IFNB	<FUNC>
	MOV	AH,FUNC
ENDIF
	INT	10H
	ENDM

EGAINT10CLI MACRO		;[8]disable interrupts if HG_EGAINT10
	ENDM

EGAINT10STI MACRO		;[8]enable interrupts if HG_EGAINT10
	ENDM


;	COM port constants:

	IRQ3	= 11d*4		;COM2 interrupt level - Int loc for COM card @ 2xxh
	IRQ4	= 12d*4		;COM1 interrupt level - Int loc for COM card @ 3xxh

	RS232B	= 400H		; X'400' RS232 Card(s) I/O addr Save area.

	RDAIE	= 1		; Read Data Available   Interrupt Enable
	TBEIE	= 2		; Transmit Buffer Empty Interrupt Enable
	MSRIE	= 8		; Modem Status          Interrupt Enable

	CTS	= 10H		;Clear To Send
	DSR	= 20H		;Data Set Ready
	RLSD	= 80H		;Received Line Signal Detect (CD)

	XMIT_BUF_SIZE = 80H	;[6] length of output buffer

	PAGE

;******************************************************************************

;	enable and disable

;******************************************************************************

	ENABLE	EQU	STI
	DISABLE	EQU	CLI

;******************************************************************************

; The PAUSE macro is required because a number of Intel's early 286 processors
; had defects. The PAUSE macro is required whenever there is a number of
; IN and OUT instructions in close proximity to each other.
; This macro insures that an instruction fetch occurs between IN and/or OUT
; instructions on the IBM PC AT machine.

; Note:	OEMs should be able to disregard this macro by removing the JMP
;	instruction.

;******************************************************************************

PAUSE	MACRO			;macro to insure that an instruction
	JMP	$+2		;fetch occurs between IN and/or OUT
	ENDM			;instructions on the IBM PC AT machine

;***************************************************************************
;
;       8086 Interrupt Handling Macros
;       MS-DOS 1.0 does not have INT 21 to GET a vector
;       hence SAVINT just moves interrupts around.
;
;***************************************************************************

	SVINT	MACRO   savloc,intloc,reg
	IFB	<reg>
	SVINT	savloc,intloc,AX
	ELSE
	MOV	reg,intloc
	MOV	savloc,reg
	MOV	reg,intloc+2
	MOV	savloc+2,reg
	ENDIF
	ENDM

;****************************************************************************
;
;	The following macros use the MS-DOS INT 21 call to change
;	interrupt vectors.
;
;****************************************************************************

savint	macro	savloc,intvec 
	MOV	AX,3500H+INTVEC/4 ;;[2]AH=get int - AL=interrupt number
	int	21h
	mov	word ptr savloc,bx ;;savloc = offset
	mov	word ptr savloc+2,es ;;savloc+2 = segment
	endm

setvec	macro	interrupt,offset_adr ;;set interrupt vector function call
	mov	dx,offset offset_adr
	MOV	AX,2500H+INTERRUPT ;;[2]AH=set int call - AL=interrupt
	int	21h
	endm

rstvec	macro	interrupt,save_adr ;;restore interrupt from saved
	lds	dx,dword ptr save_adr
	MOV	AX,2500H+INTERRUPT ;;[2]AH=set int call - AL=interrupt
	int	21h
	endm

XFRINT	MACRO	TOINT,FROMINT	;;move FROMINT to TOINT
	PUSH	ES		;;save register...
	MOV	AX,3500H+FROMINT ;;want to get FROMINT
	INT	21H		;;vector now in ES:BX
	MOV	AX,ES		;;keep segment around
	POP	ES		;;restore register...
	MOV	DX,BX		;;move offset for put
	PUSH	DS		;;save register...
	MOV	DS,AX		;;segment for put
	MOV	AX,2500H+TOINT	;;want to put TOINT
	INT	21H		;;vector now in DS:DX
	POP	DS		;;restore register
	ENDM

;******************************************************************************

;	Queue Structures for music and communications

;******************************************************************************

; This queue structure is used by the MUSIC queues and
; the COM buffer queues (both input and output). The
; field QUNOTE is not used by the COM buffer queues
; but will be present as a dummy field so that the same
; queue manipulation routines may be used.

;******************************************************************************

QUE_CTRL_BLOCK STRUC

	QUNOTE	DW ?		; Number of notes in Bkgnd queue
	QUETOP	DW ?		; Top of Queue Location
	QUEBOT	DW ?		; Bot of Queue Location
	QUELEN	DW ?		; Length of Queue
	QUENUM	DW ?		; Number of bytes in Que
	QUEGET	DW ?		; Queue get pntr
	QUEPUT	DW ?		; Queue put pntr

QUE_CTRL_BLOCK ENDS

QUSIZE	=	SIZE QUE_CTRL_BLOCK

QLENTH	=	32*6		;room for 32 notes (exactly)

QUE_CTRL_LEN =	QUSIZE		;Length of Queue Control Block

	SPCSIZ=	QLENTH+16*3	;non-IBMPCJr queue size
				;one full size; two 16-byte
				;for SYNC's; NOISE queue
	HALSIZ=	QLENTH*3+QLENTH/2 ;PC Jr queue size; Three full
				;size queues; half size
				;queue for noise

;******************************************************************************
;	EQUates for $DONOTE input value in AL
;	The value of these equates is quite important as $DONOTE depends on
;	them.
;	Function codes to be passed to B$DONOTE
;******************************************************************************

	QUERST=0D		;[3] Queue an internote pause (rest)
	QUENOT=1D		;[3] Queue a note
	TSTVOC=2D		;[3] Test if voice is active
	QUESYN=3D		;[3] Queue synchronization mark
	SETESH=4D		;[3] Queue an envelope shape request
	SETEPR=5D		;[3] Queue an envelope period request
	MKNOIS=6D		;[3] Support NOISE statement
	SNDONF=252D		;[3] SOUND ON/OFF
	BEPONF=253D		;[3] BEEP  ON/OFF
	STRSND=254D		;Start sound
	STPSND=255D		;Stop sound and flush the queues

	DFLVOL=-1		;[3] Default Volume (-1)

;******************************************************************************
	.LIST
