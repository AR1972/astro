.386p
page 58,132
;******************************************************************************
	TITLE	INIT - initialization code for CEMM
;******************************************************************************
;
;   (C) Copyright MICROSOFT Corp. 1986-1991
;   (C) Copyright COMPAQ Computer Corp. 1986-1991
;
;   Title:	EMM386.EXE - MICROSOFT Expanded Memory Manager 386 Driver
;
;   Module:	INIT - initialization code for CEMM
;
;   Version:	2.02
;
;   Date:	May 24,1986
;
;   Author:	Steve Preston,Brad Tate
;
;******************************************************************************
;
;	Change Log:
;
;	DATE	 REVISION	Description
;	-------- --------	--------------------------------------------
;	05/24/86 Original
;	06/18/86 0.01		Added AUTO as a valid config line parameter.
;	06/25/86 0.02		Added call to debug init.
;	06/27/86 0.02		Check for Mx length = 2 and only 2 (SBP).
;	06/28/86 0.02		Change name from CEMM386 to CEMM (SBP).
;	06/29/86 0.02		Size > 8192 were used instead of converted
;				to 256 (SBP).
;	07/03/86 0.04		Added TEXT_Seg (SBP).
;	07/06/86 0.04		changed assume to _DATA (SBP).
;	07/06/86 0.04		moved init messages to LAST (SBP).
;	07/10/86 0.05		added int15 patch and int67 patch here (SBP).
;	07/11/86 0.06		check processor type before checking for
;				install (SBP).
;	01/09/87 0.10 (*A)  GE	Set flag to disable RAM search if M1-M5 is specified
;	05/18/87 2.00		New state variables for CEMM state/modes and
;				reworked AUTO/ON/OFF init code.
;	05/29/87 2.00		Reworked Page Frame selection logic (SBP).
;	06/07/87 2.00		Add Weitek detection & arguments WON/WOFF (SBP).
;	02/20/88 3.30 (*B)	Init check for P9 processor (RDV)
;	07/14/88 3.31 (*C)	Init check for Taurus and Horizon (RDV)
;				Add WAIT (pause routine) for init errors (RDV).
;				Move DOS version chk to cemm386.asm (stratini).
;	01/12/89 4.00 (*D)	User aborted install added (RDV)
;				Generic 8042 detect and flag
;	08/07/89 4.10   	Add DEB386 support (LC).
;	08/07/89 4.10		Add CEMM to the int 2Fh chain (HKN&LC)
;	08/23/89 4.10		New memory allocation (INT15h,BIM,XMS) (LC)
;
;	02/13/91 M008		Detect a Compaq Deskpro 386/16 or a Compaq
;				portable 386.
;
;	02/14/91 M010		Added support for Y path parameter for
;			  	load hi Vxd.
;
;******************************************************************************
;   Functional Description:
;	This module allocates the pool of extended memory to be used for
;   expanded memory, then call routines to initialize the data structures
;   for EMM and VDM.
;
;******************************************************************************
.lfcond
	page
;******************************************************************************
;			P U B L I C   D E C L A R A T I O N S
;******************************************************************************
;
	public	Init_CEMM386
	public	EndDriver
	public	parser
	public	Pn
	public	PnSet
	public	EstWorkSpace

	public	eXcSet
	public	IncSet
	public	DfltSet
	public	VGAROM
	public	E000ROM
	public	ROMparm
	public	RAMSet
	public	ROMSet
	public	EMSSet
	public	WINset
	public	HighSet

	public	max_pool_size
	public	min_pool_size
	public	high_memory_address
	public	ext_memory_address
	public	ext_size
	public	hi_size
	public	ext_mem
	public	UMBset
	public	HMAonSet
	public	Highscan
	public	NoTR

	public 	pCOMPAQ
	public	szCOMPAQ
	public	CDPSCSI

	public	XmmControlBase
	public	XmmControlJmpVal

	public	DOS_version
	public	XMMHookAddr

	page
;******************************************************************************
;			L O C A L   C O N S T A N T S
;******************************************************************************
	include vdmseg.inc
	include emm386.inc
	include driver.equ
	include driver.inc
	include ascii_sm.equ
	include	emmfunct.inc
	include	emmdata.inc
	include	winemm.inc
	include allocmem.inc
	include dma.inc
	include vdmsel.inc

;******************************************************************************
;			E X T E R N A L    R E F E R E N C E S
;******************************************************************************
R_CODE	segment
	EXTRN	DDT: WORD
	EXTRN	ttl_hndls: WORD
	extrn	DEVHEAD:word
	extrn	DEVHEAD2:word
	extrn	rINT13hHandler:near	; int13h patch code
	extrn	rINT15hHandler:near	; int15h patch code
	extrn	rINT19hHandler:near	; int19h patch code
;;	extrn	rINT4BhHandler:near	; int4Bh patch code
	extrn	EMM_rEntry:near 	; int67h patch code
	extrn	FarGoVirtual:far	; go to virtual mode
	extrn	Devname:byte
	extrn	MasterPICVec:word
	extrn	SlavePICVec:word

;;	extrn	b2asc10_far:far
;;	extrn	b2ascHEX_far:far
;;	extrn	I_Message_Display:far
;;	extrn	E_XStatus_Display:far

	extrn 	rINT2FhHandler:near
;;	extrn	XMMAllocateHMA:far
;;	extrn	check_XMM:far
;;	extrn	chk_machine_state:far

	extrn	EMM_rFarEntry:word
	extrn	end_of_R1_CODE:byte
	extrn	UMBFARTABLE:word
	extrn	UMBADDRLEN:abs
	extrn	checkXMMFar:dword
	extrn	chkMcStateFar:dword

CDPSCSI	db	0  ; A CDP (Columbia Data Products) SCSI fixed drive is detected.

R_CODE	ends

R1_CODE	segment

	extrn	rINT4BhHandler:near	; int4Bh patch code
	extrn	OldInt13:dword
	extrn	PrevInt4B:dword
	extrn	rXMMentry:far
	extrn	PrevInt10:dword
	extrn	PrevInt11:dword

R1_CODE	ends

_DATA	segment
	extrn	HMAmin:word
_DATA	ends

GDT	segment
	extrn	GDTLEN:abs
GDT	ends

IDT	segment
	extrn	IDTLEN:abs
IDT	ends

ifdef TSSQLEO
TSS	segment
	extrn	TSSLEN:abs
TSS	ends
endif

LAST	segment
	extrn	Inst_chk:near		; check for CEMM already installed
	extrn	r_cmos:near
	extrn	InstallMess:byte
	extrn	ISizeMess:byte
	extrn	ActiveMess:byte
	extrn	InactiveMess:byte
	extrn	OFFMess:byte
	extrn	AutoMess:byte
	extrn	InvParm:byte
	extrn	InvPFBA:byte
	extrn	InvMRA:byte
	extrn	Adj_Size:byte
	extrn	InsfMem:byte
	extrn	Incorrect_DOS:byte
	extrn	Incorrect_PRT:byte
	extrn	Already_Inst:byte
	extrn	No_PF_Avail:byte
	extrn	PFWarning:byte
	extrn	NoWeitek:byte
	EXTRN	PF_not_3_2: BYTE
	extrn	HMAonMsg:byte
	extrn	NumOfUMBwindows:byte
	extrn	UMBmemMsg:byte
	extrn	OverlapWarn:byte
	extrn	WeitekONMess:byte
	extrn	WeitekOFFMess:byte
	extrn	WeitekNAMess:byte
	extrn	UserAbortMsg:byte
	extrn	OF_won_err:byte
	extrn	PFB_warning:word
	extrn	bad_mc_state:near
	extrn	Inv_DOS_msg:near
	extrn	Is386:far		; check for 386
	extrn	Is386s:far		;
	extrn	IsP8042:far		; generic check for password 8042
	extrn	EMM_Init:near		; initialization for EMM data structs
	extrn	DMAInit:near
	extrn	PICVecInit:near
	extrn	VDM_Init:near		; initialize VDM
	extrn	WSInit:near
	extrn	WSMove:near
	extrn	FindWindowLocations:near ; look for option roms in PF area
	extrn	InitDeb:near
	extrn	MemInit:near
	extrn	MemGet:near
	extrn	MemExit:near
	extrn	GetPathName:near
	extrn	StorePath:near		; M010
	extrn	NoXMM:byte
	extrn	OtherEMM:byte
	extrn	BadXMM:byte
	extrn	XMMcheck:near
	extrn	get_XMM_ver:near
	extrn	XMMQueryExtended:near
	extrn	IsCompaq:near
	extrn	segfixup:near
	extrn	UMBlink:near
	extrn	NoResetRoutine:near
	extrn	Page4K:byte
	extrn	handle0_PTEs:word
	extrn	DoMoveBlock:near

ifdef ROMcomp
	extrn	FixROMptrs:near
	extrn	FixIVTptrs:near
endif

	extrn	b2asc10:near
	extrn	b2ascHEX:near
	extrn	I_Message_Display:near
	extrn	E_XStatus_Display:near
	extrn	wait_key:near

ifdef DEBUG
	extrn	DebDefineSeg:near
endif

LAST	ends

TB			EQU	09h
MIN_SIZE 		EQU	64
MAX_ALT_REG_SETS	EQU	254

LAST	SEGMENT				; (PIW)
switch	DD	Invalid_Parameter	;	0
	DD	Get_P_Handle_Page_Frame	; /     1
	DD	Get_Size		; 0     2
	DD	Get_Size		; 1     3
	DD	Get_Size		; 2     4
	DD	Get_Size		; 3     5
	DD	Get_Size		; 4     6
	DD	Get_Size		; 5     7
	DD	Get_Size		; 6     8
	DD	Get_Size		; 7     9
	DD	Get_Size		; 8    10
	DD	Get_Size		; 9     1
	DD	Get_Mode_Alt_Reg_Sets	; A     2
	DD	Get_Lowest_Frame	; B     3
	DD	Get_DMA_Buffer_Size	; D     4
	DD	Get_Page_Frame		; F     5
	DD	Get_H			; H     6
	DD	Get_Left_Alone_Size	; L     7
	DD	Get_M			; M	8
	DD	Get_On_Off_Mode		; O     9
	DD	Get_I_Page_Frame	; P    20
	DD	Get_Wetek_Mode		; W     1
	DD	GetExcludeRange		; X     2
	DD	GetRAM			; R     3
	DD	NoMoveXBDA		; N     4
	DD	GetIncludeRange		; I	5
	DD	EMSInts			; E	6
	DD	GetFullPath		; Y	7  (M010)
	DD	Vir8042Parm		; V	8  (LC910611)
case	DB	00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00
	DB	00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00
	DB	00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,01	; /
	DB	02,03,04,05,06,07,08,09,10,11,00,00,00,00,00,00	; 0123456789
	DB	00,12,13,00,14,26,15,00,16,25,00,00,17,18,24,19	; ABDEFHILMNO
	DB	20,00,23,00,00,00,28,21,22,27,00,00,00,00,00,00 ; PQRVWXY
	DB	00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00
	DB	00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00
	DB	00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00
	DB	00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00
	DB	00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00
	DB	00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00
	DB	00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00
	DB	00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00
	DB	00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00
	DB	00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00
$Hset	DB	0
altRset	DB	0
baseset	DB	0
bDMAset	DB	0

;------------------------------------------------------------------------
; Keep all parameter ranges together here so they can be saved/restored
; together.

	public	RangeSets, RANGESIZE, RangeSave

RangeSets label byte
eXcSet	dw	EXCLUDE shl 8 + 2, 24 dup (0)	; eXclude range specified by user
IncSet	dw	RAM 	shl 8 + 2, 24 dup (0)	; Include range specified by user
RAMSet	dw	RAM   	shl 8 + 2, 24 dup (0)	; RAM range specified by user
EMSSet	dw	EMS   	shl 8 + 2, 24 dup (0)	; EMS range specified by user
ROMSet	dw	ROM   	shl 8 + 2+4*1
	dw	0FF00h,0FF00h,24 dup (0)	; ROM range specified by user
WINset	dw	WIN	shl 8 + 2, 24 dup (0)	; Win range specified by user
HighSet dw	2, 24 dup (0)			; Highscan detected ranges

DfltSet dw	INUSE 	shl 8 + 2+3*4
	dw	00001h				; from 1
Bparm	dw	03FFFh				; to   3fffh
	dw	0A000h				; from a000
VGAROM	dw	0BFFFh				; to   bfffh
E000ROM	dw	0F000h, 0FFFFh			; exc f000h - ffffh
	dw	0,0				; Terminating zeroes
RANGESIZE equ	$-RangeSets

RangeSave db	RANGESIZE dup (?)

;------------------------------------------------------------------------

frames	DW	0C000h			; M1
	DW	0C400h			; M2
	DW	0C800h			; M3
	DW	0CC00h			; M4
	DW	0D000h			; M5
	DW	0D400h			; M6
	DW	0D800h			; M7
	DW	0DC00h			; M8
	DW	0E000h			; M9
	DW	08000h			; M10
	DW	08400h			; M11
	DW	08800h			; M12
	DW	08C00h			; M13
	DW	09000h			; M14
numFram	EQU	($-frames) SHR 1
	DW	09400h			; Pddd
	DW	09800h			; Pddd
	DW	09C00h			; Pddd
	DW	0E400h			; Pddd
	DW	0E800h			; Pddd
	DW	0EC00h			; Pddd
numPFrm	EQU	($-frames) SHR 1
framadr		DD	frames
tknBuf		DB	128 DUP (0)
op_on		DB	"ON", 0
ONLEN		EQU	$-op_on
op_off		DB	"OFF", 0
OFFLEN		EQU	$-op_off
op_auto		DB	"AUTO", 0
AUTOLEN		EQU	$-op_auto
frame$		DB	"FRAME="
framLen		EQU	$-frame$
ROMCompParm	db	"ROMCOMPRESS"
ROMCompLen	equ	$-ROMCompParm
verbose$	db	"VERBOSE", 0
verboseLen	equ	$-verbose$
ext_mem		dd	FREE
Pn		DB	6 DUP (FREE)
PnSet		DW	FALSE
UMBset		dw	FALSE
UMBman		dw	FALSE
HMAonSet 	db	FALSE
ROMparm		db	FALSE
NoPFCset	db	FALSE
NoHigh		db	FALSE
Highscan	db	FALSE
Verbose 	db	FALSE
NoVCPI		db	FALSE
NoTR		db	FALSE

smartdrv_status	status	<>
smartdrv_name	db	'SMARTAAR',0
CACHEname	db	'CACHCMPQ',0
CACHEIOCTL	dw	1
		dd	0
		dd	0

min_pool_set		db	FALSE
max_pool_set		db	FALSE
min_pool_size		dw	256
max_pool_size		dw	0
high_memory_address     dd	0
ext_memory_address      dd	0
ext_size                dw	0
hi_size                 dw	0

set_int13	label	byte
		dw	000dh	; cmd byte for setting Smartdrv's old int 13
		dd	?	; address to which Smartdrv's old int 13 s
				; should be set

EndDriver	dw	0	; last segment of driver to load

XmmControlBase	label	dword	; base address of XMM control headers
		dw	-1
		dw	-1

XmmControlJmpVal db	?	; offset byte of short jmp instruction

SYSSS		dw	?
SYSSP		dw	?

DOS_version	dw	0
XMMHookAddr	dd	0

; Toshiba specific data items

	public	HRAMSet
	public	BRAMSet
	public	ToshSet
	public	toshiba_machine
	public	TOSHbeg
	public	TOSHend

HRAMSet dw	INUSE	shl 8 + 2+1*4		; Toshiba Hard Ram exclusions
HRAMbeg	dw	0h				; HardRAM Win begin (e000 dflt)	
HRAMend	dw	0h				; HardRAM Win end   (e800 dflt)
	dw	0,0				; Terminating zeroes

BRAMSet dw	INUSE	shl 8 + 2+1*4		; Toshiba Backup Ram exclusions
BRAMbeg	dw	0h				; Bkup RAM begin (e800 dflt) 	
BRAMend	dw	0h				; Bkup RAM end   (efff dflt)
	dw	0,0				; Terminating zeroes

ToshSet dw	INUSE	shl 8 + 2+1*4		; Toshiba Misc. exclusions
TOSHbeg	dw	0h				; Misc. Toshiba resrvd area begin (e000 dflt) 	
TOSHend	dw	0h				; Misc. Toshiba resrvd area end   (efff dflt)
	dw	0,0				; Terminating zeroes	

pTOSHIBA	label	dword		; Pointer to TOSHIBA signature
		dw	0E010h
		dw	0F000h

szTOSHIBA	db     'TOSHIBA '	; TOSHIBA signature

toshiba_machine db	0		; NZ if on a Toshiba machine

LAST	ENDS

LAST	segment
	assume	cs:LAST


pCOMPAQ 	label	dword		; Pointer to COMPAQ signature
		dw	0FFE8h
		dw	0F000h

szCOMPAQ    	db     '03COMPAQ'	; COMPAQ signature

pZENITH 	label	dword		; Pointer to ZENITH signature
		dw	0800Ch
		dw	0F000h

szZENITH    	db     'ZDS'		; ZENITH signature

;   local data
;
pEISAROM	label	dword
		dw	0FFD9h		; "EISA" in ROM
		dw	0F000h

AUTO_parm	db	"auto"
AUTO_LEN	equ	$-AUTO_parm

msg_tbl 	label	word
		dw	offset LAST:OverlapWarn ; User specified ranges overlap
		dw	offset LAST:Incorrect_DOS ; Incorrect Version of DOS
		dw	offset LAST:InsfMem	; Insufficient Memory
		dw	offset LAST:Already_Inst; Already Installed
		dw	offset LAST:No_PF_Avail ; No Page Frame Space Avail
		dw	offset LAST:Adj_Size	; Pool Size Adjusted
		dw	offset LAST:InvPFBA	; Page Frame Base Addr Adjusted
		dw	offset LAST:InvMRA	; Map Register Adjusted
		dw	offset LAST:InvParm	; Invalid Parameter msg
		dw	offset LAST:PFWarning	; Page Frame warning message
		dw	offset LAST:NoWeitek	; Weitek not installed
		dw	offset LAST:NoXMM	; XMS manager not installed
		dw	offset LAST:BadXMM	; Possible bad HIMEM version
		dw	offset LAST:OF_won_err	; Unable to enable WEITEK
		dw	OFFSET LAST:PF_not_3_2	; Use Pn without a page frame
		dw	offset LAST:OtherEMM	; Other EMM on system already
		dw	offset LAST:HMAonMsg 	; Virtual HMA is invalid
		dw	offset LAST:UMBmemMsg 	; No more UMB/HMA memory
		dw	offset LAST:UserAbortMsg; User aborted installation
MAX_MSG 	equ	(this byte - msg_tbl)/2 ; # of messages to display

	;   macro for printing messages located in LAST segment
	;	ENTRY: DX = offset LAST:message
PRINT_MSG	macro
	push	ds
	mov	ax,seg LAST
	mov	ds,ax		; ds = LAST
	mov	ah,PRINT_STRING
	int	MS_DOS		; output init message
	pop	ds
ENDM

page
;******************************************************************************
;	Init - Initialization routine for CEMM.
;
;	ENTRY: DS:BX pts to INIT request header.
;
;	EXIT:  AX = INIT status for request header
;		if NO errors :
;			CEMM initialized.
;			if [ON] parameter specified on command line
;			    exit in Virtual mode and CEMM active.
;			else ( [OFF] parameter specified )
;			    exit in Real mode and CEMM inactive.
;		if errors:
;			Real Mode
;	USED: none
;
;******************************************************************************
Init_CEMM386	proc	far
.8086				; 8086 code only
	mov	cs:[SYSSS],ss
	mov	cs:[SYSSP],sp

	mov	ax,seg L_STACK
	mov	ss,ax
	lea	sp,L_STACK:[LastStackTop]

	push	bx		; BP+10
	push	dx		; BP+8
	push	bp		; BP+6
	push	di		; BP+4
	push	ds		; BP+2
	push	es		; BP+0
	mov	bp,sp

;
;  initialize debugger ***** this assumes we are on a 386 *****
;
	mov	ax,seg _DATA
	mov	ds,ax
	assume	ds:_DATA

ifdef	BugMode
	mov	al,0FFh
	call	InitDeb
endif
ifdef DEBUG
	call	InitDeb
endif
;
;	initialize break address to not install
;
	mov	bx,[bp+2]		; get entry DS from stack
	mov	es,bx
	assume	es:ABS0
	mov	bx,[bp+10]		; ES:BX pts to req hdr
	mov	word ptr es:[bx.BRK_OFF],0000	; set it
	mov	ax,seg R_CODE		; get brk addr segment
     ;;;inc	ax			; reserve dos link pointer
	mov	es:[bx.BRK_SEG],ax	; break addr = cs - don't install
	mov	byte ptr es:[bx.NUM_UNITS],0	; 0 - don't install
	mov	ax,seg STACK
	mov	cs:[EndDriver],ax

;
;  verify DOS version
;
	call	chk_DOS
	jnc	short IE_proc
	jmp	ICinvdos

IE_proc:
;
;  verify processor type
;
	call	Is386			;Q: is this a 386 ?
	jnc	short IE_state 		; Y: check machine state
	jmp	ICnon386 		; N: no, set error
ifdef 910318
	jc	short inc_prc 		;  N: no, set error
                                        ;  Y: check machine state
endif
IE_state:
;
;  verify machine state
;
	push	es
	mov	ax,seg R_CODE
	mov	es,ax
	call	dword ptr es:[chkMcStateFar]
	pop	es
					;Q: is the machine in real mode
	jnc	short IE_machine	; Y: check machine type
	jmp	ICbadstate 		; N: no, set error
ifdef 910318
        jc      short IE_badstate       ; N: error
endif

.386P					; atleast a 386 at this point
IE_machine:
	mov	ax,X_HI_MEM_SEG 	; ROM segment
	mov	es,ax			; into es
	assume	es:ABS0
	mov	ax,es:X_MT_AT		; get machine type
	mov	[ROMID],al		; save machine type

ifdef 910318
	jmp	short IE_EISACheck

ifdef	ROMIDMCA
	cmp	al,ROMIDISA 		;Q: Is this an ISA/EISA class machine?
	je	short IE_EISACheck	; Y: continue
	cmp	al,ROMIDPS2		;Q: Is this a PS2 with 386 proc?
	je	short IE_EISACheck	; Y: continue
endif

inc_prc:
	jmp	ICnon386

;;IE_badstate:
;;	jmp	ICbadstate

.386P					; atleast a 386 at this point
IE_EISACheck:
endif

ifdef MSFLAG
	call	segfixup		; fix up GS & FS (see segfix.asm)
endif
	mov	ax,seg _DATA		; setup new segments
	mov	ds,ax
	mov	ax,seg R_CODE
	mov	gs,ax
	ASSUME	ds:_DATA
	ASSUME	gs:R_CODE

	call	EISACheck		; check if this is an EISA machine

	call	MCACheck		; check if this is an MCA machine

	call	IsZenith		; check to see if this is a ZENITH

IE_UserAbort:
;
;  check if special key-sequence is being pressed - if so, don't load
;
	call    Check_Install_Abort	;Q: Is user requesting an abort?
	jnc	short IE_otherEMM	; N: check for another EMM
	or	gs:[msg_flag],UserAbort_MSG
	jmp	IE_exit

IE_otherEMM:
;
;  Verify CEMM is not installed or another EMM is present
;
	call	Inst_chk
	or	ax,ax			;Q: Int 67h vector zero?
	jz	short IE_chk_pt		; Y: OK

	or	al,al			;Q: CEMM Already installed?
	jnz	short IECEMMInstalled	; Y: error, exit
;
;  Check if another EMM is already on system.
;
	mov	ah,40h			; get status function
	int	67h
	or	ax,ax			;Q: Other EMM already installed?
	jnz	short IE_chk_pt		; N: OK
	or	gs:[msg_flag],OTHER_EMM_MSG
	jmp	IE_exit
IECEMMinstalled:
	or	gs:[msg_flag],INS_ERR_MSG	; y: error
	jmp	IE_exit 		; quit
IE_chk_pt:
	call	Is386s			;Q: Is it a COMPAQ 386SX based machine?
	jne	short IE_chk_pt1	; N: no, continue
	or	gs:[GenFlags],f386SX	; Y: set flag
IE_chk_pt1:

	; Detect if channel 6 of 8237 is connected to a Columbia Data
	; Products SCSI drive.

	mov	ax, 0100h
	mov	si, 6a6ah
	xor	di,di
	int	11h
	cmp	di, 6a6ah		; Q: did DI come back with 6a6a
	jne	IE_no_CDP_SCSI		; N: No CDP SCSI
	cmp	ah, 33h			; Y: is the version > 33h
	ja	IE_no_CDP_SCSI		; Y: the drive's softare will use
					;    VDS
	mov	gs:[CDPSCSI], 1		; N: set flag to indicate that a CDP
					;    SCSI is on channel 6. Setting
					;    this flag indicates that the
					;    initial values of base,page and
					;    count on channel 6 as maintained
					;    by our data structures will be 0.
IE_no_CDP_SCSI:

					; M008 - Start
	call	IsCompaq		;Q: is it a Compaq 386
	jnz	IE_Weitek_chk		; N: no, continue
	call	IsCPQ16			; Set bit if it is a 386/16
					; M008 - end

ifndef	MSFLAG
ifdef 900105
	call	NoResetRoutine		; Y: don't need a reset routine
endif
 	call	IsP8042			;Q: Is this a password 8042 machine?
	jne	short IE_Weitek_chk	; N: no, continue
	or	gs:[GenFlags],fP8042	; Y: set flag
	mov	al,26h			; check if A20 is toggled via port 92h
	out	70h,al			; byte 26h contains CMOS bit
	in	al,71h			; get CMOS value
	test	al,20h			;Q: Port 92h used in A20 toggle?
	jz	short IE_Weitek_chk	; N: continue
	or	gs:[GenFlags],fP92	; Y: set flag
endif

IE_Weitek_chk:
	xor	eax,eax			; clear high word of AX
	int	11h
	test	eax,fI11h_Weitek_Inst SHL 16	;Q: Weitek installed ?
	jz	short IE_Toshiba		; N: check for Toshiba stuff
	or	gs:[Weitek_State],fWeitek_Inst	; Y: set installed flag

; Do Toshiba specific checks to exclude some upper memory regions
; that are used by their BIOS and power management hardware.

IE_Toshiba:
	call	SetToshibaOptions
;
;  parse command line for
;	(1) requested size for expanded memory
;	(2) page frame base address
;	(3) I/O addresses for board emulations
;	(4) Driver exit mode (virtual or real)
;
IE_parse:
	mov	bx,[bp+2]		; get entry DS from stack
	mov	es,bx
	assume	es:ABS0
	mov	bx,[bp+10]		; ES:BX pts to req hdr
	les	di,ES:[bx.ARG_PTR]	; ES:DI pts to config.sys command
					;	line parameters

	call	GetPathName		; get drivers path and name
	call	parser			; parse the parameters

	mov	ah, 2			; Force VERBOSE mode if user is
	int	16h			;   holding down the Alt key
	test	al, 08h 		; Int 16h/2 == Get Keyboard Flags
	jz	short @f		;   08h set if Alt key down
	mov	[Verbose], TRUE
@@:
	test	gs:[msg_flag],KILLER_MSG; Q: any killer messages?
	JNZ	IE_exit			; Y: exit with error

;
;  Check for memory above 16 meg on Compaq ISA systems, must buffer DMA
;  above 16 meg on these systems
;
	call	IsCompaq		; Is this a Compaq?
	jnz	IE_xmm_check

	mov	ax, 0E801h		; Compaq Get Memory Size Function
	int	15h
	jc	short IE_xmm_check	; If not supported, continue
	or	dx,dx			;Q: Is there memory configured >16MB?
	jz	short IE_xmm_check	; N: continue
	or	gs:[genflags],fabove16M ; Y:Set flag to indicate support>16M

IE_xmm_check:
	call	$chkCPQxmm		; activate non CPQ XMMs on EISA machines

	call	XMMcheck
	test	gs:[msg_flag],KILLER_MSG; Q: any killer messages?
	jnz	IE_exit			; Y: exit with error

;
;  Setup for extended memory allocation
;
IE_alloc:
;
;  Estimate amount of memory needed in extended memory!
;
	movzx	eax,cs:[max_pool_size]
	shl	eax,10			; requested EMS pool

	mov	ebx,eax
	shr	ebx,10			; memory needed for PHs
	add	eax,ebx			; 4K per PH
	add	eax,7*1024*4		; space for PTs(4),PFs(2),PD(1)

	add	eax,[DMABufferSize]	; space for DMA buffer
	movzx	ebx,[NumOfUMBwindows]	; number of UMBs
	shl	ebx,12			; each UMB is 4K
	add	eax,ebx

	cmp	cs:[HMAonSet],TRUE	;Q: Virtual HMA required?
	jne	short IEmemInit		; N: continue
	add	eax,10000h		; Y: add 64K for a virtual HMA
IEmemInit:
	call	EstWorkSpace		; EBX = estimated work space (code/data)
	add	eax,ebx
	add	eax,10000h		; leave HMA area available

	call	MemInit			; free memory in system (above 1M)
	mov	[TopOfPhysicalMemory],eax ; save top of memory
	add	eax,1000h-1		; next 4K page
	shr	eax,12			; number of PTE's needed
	mov	[MaxPTEIndex],eax 	; save max PTE index
	or	eax,eax			;Q: Atleast 4K free above 1M?
	jnz	short IE_got_mem	; Y: continue
	or	gs:[msg_flag],MEM_ERR_MSG;N: do not load
	jmp	IE_exit 		;
IE_got_mem:

; Now that memory size is know, check that min_pool_size <= max_pool_size <=
; total free memory.

	call	TotalFreeMem		; make sure max_pool <= free mem size
	shr	eax, 10 		; in k
	and	al, 0F0h		; round down to nearest 16k
	cmp	eax, MAX_SIZE
	jbe	short cmp_max_to_free
	mov	ax, MAX_SIZE
cmp_max_to_free:
	cmp	ax, [max_pool_size]
	jae	short cmp_min
	mov	[max_pool_size], ax
	cmp	[max_pool_set], TRUE	; if max was explictly set, tell user
	jne	short cmp_min		;   we changed it
	or	gs:[msg_flag], SIZE_ADJ_MSG
cmp_min:
	cmp	ax, [min_pool_size]	; check that min_pool <= free mem size
	jae	short in_range
	mov	[min_pool_size], ax
	cmp	[min_pool_set], TRUE
	jne	short in_range
	or	gs:[msg_flag], SIZE_ADJ_MSG
in_range:


ifdef ROMcomp
IE_ROMcompress:
;
; If necessary, fix all ROM pointers
;
	call	FixROMptrs
endif
;
;  Get DMA buffer and initialize for virtualizing DMA chips
;
IE_DMABuffer:
	call	DMAInit			; initialize DMA support
	test	gs:[msg_flag],KILLER_MSG;Q: Enough memory?
	jz	short IE_InitVDM	; Y: init VDM
	jmp	IE_exit			; N: do not load

;
;  Init VDM - GDT,IDT,TSS,Page Tables, WEITEK
;
IE_InitVDM:
	call	VDM_Init
	test	gs:[msg_flag],KILLER_MSG	;Q: any killer messages?
	JNZ	IE_exit	 			;  Y: exit
;
;  Allocate work space and update GDT entries.
;
IE_InitWS:
	call	WSInit
	test	gs:[msg_flag],KILLER_MSG	;Q: any killer messages?
	JNZ	IE_exit	 			;  Y: exit

IE_InitPool:
	call	GetEMSPool
	test	gs:[msg_flag],KILLER_MSG	;Q: any killer messages?
	JNZ	IE_exit	 			;  Y: exit
;
;  init EMM data
;
IE_InitEMM:
	call	EMM_Init
	test	gs:[msg_flag],KILLER_MSG	;Q: any killer messages?
ifdef	BugMode
	jz	short IE_MoveWS			;  N: move work space
else
	jz	short IE_InitDEB		;  N: init debugger
endif
	jmp	IE_exit	 			;  Y: exit
ifndef	BugMode
;
; initailize variables for debugger
;
IE_InitDEB:
ifndef DEBUG
	;call	 InitDeb
endif
endif
;
;  Move work space up to extended memory.
;
IE_MoveWS:
	call	WSMove
	test	gs:[msg_flag],KILLER_MSG	;Q: any killer messages?
	JNZ	IE_exit	 			;  Y: exit

IFDEF	QHKN
IE_chkbase:

	call	MoveExtBIOS		; move extended BIOS data area
	xor	eax,eax			; clear high order word
	int	12h			; get base memory size
	push	ax			; save it

		; The end of the driver's resident part is rounded up to
		; a 1k boundary.
	mov	ax,cs:[EndDriver]
	add	ax,40h - 1
	shr	eax,6
	add	ax,DOS3X_ADJ + 64 	; add in dos 3.xx adjustment and 64k
	pop	dx			; get base memory back
	cmp	dx,ax			; q: do we have enough?
	jae	short IE_setbrk		; y: continue
	or	gs:[msg_flag],MEM_ERR_MSG	; n: set memory error
	jmp	short IE_exit 		;    and exit

;
;   set driver break addr in Init Request Header
;
IE_setbrk:
	mov	bx,[bp+2]			; get entry DS from stack
	mov	es,bx
	assume	es:ABS0
	mov	bx,[bp+10]			; ES:BX pts to req hdr
	mov	ES:[bx.BRK_OFF],10h		; set it
	mov	ax,cs:[EndDriver]		; get brk addr segment
	mov	ES:[bx.BRK_SEG],ax		; set it
ENDIF

;
; Allocate all extended memory used
;
	xor	eax,eax				; normal allocation
	call	MemExit				; allocate all memory
	call	dword ptr gs:[checkXMMFar]	; check XMM status

ifdef	BugMode
;
;  initialize debugger before going to virtual/protected mode
;
	mov	al,0FFh
	call	InitDeb
endif
;
;   Check exit status of VDM/CEMM
;   and set CEMM active/inactive depending on the
;   mode (ON/OFF/AUTO) selected by the user.
;   In Auto Mode,
;	Weitek mapped => always ON.
;
	; default to inactive
	mov	gs:[Auto_State],0			; default to AUTO & inactive
	and	gs:[Current_State],NOT fState_Active

	; Weitek mapped ?
	test	gs:[Weitek_State],fWeitek_Map	;Q: Weitek mapping selected?
	jz	short IE_InitState		;  N: AUTO => inactive
	or	gs:[Auto_State],fAuto_Weitek	;  Y: AUTO => active

IE_InitState:
	mov	al,gs:[Initial_Mode]		; get initial mode
	mov	gs:[Current_Mode],al		; set it

	cmp	al,MODE_AUTO			;Q: exit in Auto mode ?
	jne	short IE_ModeChkOFF		;  N: check for mode = OFF
	cmp	gs:[Auto_State],0		;  Y: Q: go active ?
	je	short IE_ModeSet		;	N: ok, mode is set
	jmp	short IE_ModeActive		;	Y: go active now ...

IE_ModeChkOFF:
	cmp	al,MODE_OFF			;Q: exit mode is OFF ?
	jne	short IE_ModeActive		;  N: mode is ON, turn CEMM on
	mov	[Devname],'$'
	jmp	short IE_ModeSet		;  Y: ok, mode is set
IE_ModeActive:
	call	FarGoVirtual			; go to virtual mode
	jc	ICnon386
	or	gs:[Current_State],fState_Active; set active flag

IE_ModeSet:
;
;  exit - display status messages and set exit status
;
IE_exit:
;
;  display signon message first
;
	mov	ax,R_CODE
	mov	ds,ax
	assume	ds:R_CODE
	cmp	[msg_flag], 0		; Q: any msgs to display?
	jnz	short disp_signon	; Y: disp signon msg even if !verbose
	cmp	[Verbose], TRUE 	; N: skip signon unless verbose mode
	jnz	short skip_signon
disp_signon:
	mov	[Verbose], TRUE
	call	I_Message_Display
skip_signon:

;
; check for messages to display
;
	mov	ecx,MAX_MSG		; number of potential msgs
	mov	si,offset msg_tbl	; table of messages
	mov	ebx,1			;
m_loop:
	test	[msg_flag],ebx		; q:is this one set?
	jz	short m_inc_ptr		; n: increment table pointer
	mov	dx,cs:[si]		; y: display message
	PRINT_MSG
m_inc_ptr:
	ADD	SI, 2			; Increment msg table ptr
	shl	ebx,1			; Look for next flag
	loop	m_loop

	TEST	[msg_flag],KILLER_MSG	;Q: Is there a killer?
	jnz	IE_not_installed	; Y: Don't install

	CMP	[msg_flag], 0		; Q: Is there a warning?
	JE	SHORT no_KLLR_MSG	; N: Continue
	CALL	wait_key		; Wait for 10 seconds

no_KLLR_MSG:

ifdef DEBUG
	bt	[GenFlags],fDebugActiveBit
	jnc	IE_NoDebug

	or	[TrapFlags],fpModeDebInit	; protected mode debugger
	int	ProtTrap			;   initialization

IE_NoDebug:
endif
	pushf
	cli
;
; Initialize Virtual HMA
;
	call	VirHMAinit
;
; Add real mode interrupt handlers
;
	call	PICVecInit

ifdef ROMcomp
;
; Fixup IVT for ROM compression
;
	call	FixIVTptrs
endif
	xor	ax,ax
	mov	es,ax
	ASSUME	es:ABS0

; Add an INT 15h handler
	mov	ax,seg R_CODE
	shl	eax,16
	mov	ax,offset R_CODE:rINT15hHandler
	xchg	es:[int15],eax
	mov	[PrevInt15],eax

; Add CEMM to INT 19h chain.
	mov	ax,seg R_CODE
	shl	eax,16
	mov	ax,offset R_CODE:rINT19hHandler
	xchg	es:[int19],eax
	mov	[PrevInt19],eax

; Add CEMM to INT 2Fh chain.
	mov	ax,seg R1_CODE
	mov	fs, ax
	shl	eax,16
	mov	ax,offset R1_CODE:rINT2FhHandler
	xchg	es:[int2f],eax
	mov	dword ptr fs:[PrevInt2f],eax

; Add CEMM to INT 4Bh chain.
	mov	ax,seg R1_CODE
	shl	eax,16
	mov	ax,offset R1_CODE:rINT4BhHandler
	xchg	es:[int4B],eax
	mov	fs:[PrevInt4B],eax

; The int 67h vector is now patched out with CEMM's routine.
	mov	es:[int67],offset R_CODE:EMM_rEntry
	mov	es:[int67+2],seg R_CODE

; Add an INT 13h handler
	call	Install_I13handler

;
; Chain possible UMB links
;
	call	UMBlink

	call	MovUmbSeg
	popf				; restore interrupt flag state

	call	MoveExtBIOS		; move extended BIOS data area
	xor	eax,eax			; clear high order word
	int	12h			; get base memory size
	push	ax			; save it

		; The end of the driver's resident part is rounded up to
		; a 1k boundary.
	mov	ax,cs:[EndDriver]
	add	ax,40h - 1
	shr	eax,6
	add	ax,DOS3X_ADJ + 64 	; add in dos 3.xx adjustment and 64k
	pop	dx			; get base memory back
	cmp	dx,ax			; q: do we have enough?
	jae	short IE_setbrk		; y: continue
	or	gs:[msg_flag],MEM_ERR_MSG	; n: set memory error
	jmp	IE_exit 		;    and exit

;
;   set driver break addr in Init Request Header
;
IE_setbrk:
	mov	bx,[bp+2]			; get entry DS from stack
	mov	es,bx
	assume	es:ABS0
	mov	bx,[bp+10]			; ES:BX pts to req hdr
	mov	ES:[bx.BRK_OFF],10h		; set it
	mov	ax,cs:[EndDriver]		; get brk addr segment
	mov	ES:[bx.BRK_SEG],ax		; set it

	cmp	[Verbose], TRUE 		; Exit vocally?
	jnz	IE_Success			;  N: keep quiet

	mov	ax,seg LAST
	mov	es,ax
	assume	es:LAST                 ; destination of message

	mov	dx,offset LAST:InstallMess
	PRINT_MSG

	mov	ax,R_CODE
	mov	ds,ax
	assume	ds:R_CODE

	call	E_XStatus_Display
	;
	; output messages displaying state of Weitek support
	;
	test	[Weitek_State],fWeitek_Inst	;Q: Weitek installed ?
	jz	short prnw_done			;  N: no messages then
	mov	dx,offset LAST:WeitekONMess	;  Y: assume Weitek ON
	test	[Weitek_State],fWeitek_Map	;Q: Weitek enabled ?
	jnz	short print_weitek			;  Y: display message
	mov	dx,offset LAST:WeitekOFFMess	;  N: assume OFF
print_weitek:
	PRINT_MSG

	; chk for Weitek not accessible
	cmp	[Initial_Mode],MODE_OFF 	;Q: CEMM OFF specified ?
	jne	short prnw_done			;  N: no more messages
	mov	dx,offset LAST:WeitekNAMess	;  Y: W not accessible
	PRINT_MSG
prnw_done:
;
;   output messages displaying current mode
;
	mov	dx,offset LAST:OFFMess
	cmp	[Current_Mode],MODE_OFF		;Q: OFF mode ?
	je	short print_mode		;  Y: print it's message

	cmp	[Current_Mode],MODE_AUTO	;Q: auto mode ?
	jne	short print_ONOFF		;  N: check for ON/OFF
	mov	dx,offset LAST:AutoMess 	;  Y: display AUTO mode message
	PRINT_MSG				;

print_ONOFF:
	mov	dx,offset LAST:InactiveMess	; assume inactive
	test	[Current_State],fState_Active	;Q: Active ?
	jz	short print_mode		;   N: display OFF message
	mov	dx,offset LAST:ActiveMess	;   Y: display ON message
print_mode:
	PRINT_MSG
;
; all done with no errors
;
IE_Success:
	xor	ax,ax			; NO errors
;
.8086					; Must be in 8086 instructions
IE_leave:
	pop	es
	pop	ds
	pop	di
	pop	bp
	pop	dx
	pop	bx

	mov	ss,cs:[SYSSS]	; restore DOS stack
	mov	sp,cs:[SYSSP]

	ret
;
.386p
IE_not_installed:
	mov	eax,-1			; abort in progress
	call	MemExit			; put back memory we took

.8086					; Must be in 8086 instructions
ICerror:
	call	ErrorExit		; Clean up on error condition
ICerror1:
	call	wait_key		; pause to let user read error

	mov	ax,ERROR		; error return
	jmp	short IE_leave

ICnon386:
	mov	dx,offset LAST:Incorrect_PRT
        jmp     short ICmessPrint

ICinvdos:
	mov	dx, offset LAST:Inv_DOS_msg
	jmp	short ICmessPrint

ICbadstate:
	mov	dx, offset LAST:bad_mc_state

ICmessPrint:
	PRINT_MSG
        jmp     short ICerror1
;
Init_CEMM386	endp


.386p					; Here can be 386P?
page
;******************************************************************************
;
;	parser - parse out CEMM parameters and set appropriate values.
;
;			entry:	es:di ==  config.sys command line parameters
;				ds = _DATA
;                     		386 systems only
;			exit:	[min/max_pool_size] = expanded memory sizes
;				[Initial_Mode] = mode for driver exit
;				[msg_flag] = appropriate messages to display
;				... many others ...
;
;			used:	none
;
;******************************************************************************
;
	ASSUME	CS: LAST, DS: _DATA, GS: R_CODE
Parser	PROC
	PUSHA
	PUSH	DS
	PUSH	ES
	PUSH	FS
	MOV	SI, _DATA
	MOV	FS, SI
	ASSUME	FS: _DATA

	MOV	SI, DI
	MOV	DI, ES			; Move ES:DI to DS:SI
	MOV	DS, DI

	MOV	DI, LAST		; ES:DI points to token buffer
	MOV	ES, DI
	ASSUME	ES: LAST

	MOV	DI, OFFSET tknBuf
	CLD				; Direction set to increment
	CALL	Get_Token		; Skip the first token
	JCXZ	no_token
nexToken:
;
; CX = Get_Token(DS:SI, ES:DI);
;
	CALL	Get_Token
	JCXZ	no_token
	CALL	Parse_Token
	JMP	nexToken
no_token:
	MOV	AX, _DATA
	MOV	DS, AX
	ASSUME	DS: _DATA

	cmp	gs:[Initial_Weitek],0FFh; Q: did they specifiy Weitek mode ?
	je	short def_mode		; N: ok, leave it off
	test	gs:[Weitek_State],fWeitek_Inst	;Q: Weitek installed ?
	jz	short setwm_notpres		;  N: set msg flag
	cmp	gs:[Initial_Weitek],0		;  Y:Q: Weitek OFF ?
	je	short setwm_done		;      Y: leave flag off
	or	gs:[Weitek_State],fWeitek_Map	;      N: set flag -> ON
	jmp	SHORT setwm_done
setwm_notpres:
	or	gs:[msg_flag],W_NI_MSG	; warning flag set
setwm_done:

def_mode:
	cmp	gs:[Initial_Mode],0FFh		;Q: did they specify mode ?
	jne	short def_mode2			; Y: ok ...
ifdef MSFLAG
	mov	gs:[Initial_Mode],MODE_ON	; N: default is ON MODE (MS)
else
	mov	gs:[Initial_Mode],MODE_AUTO	; N: default is AUTO MODE (CPQ)
endif
def_mode2:
	cmp	byte ptr cs:[RAMset],2	;Q: RAM specified?
	jne	short def_ON		; Y: turn CEMM ON
ifdef 901022
	cmp	byte ptr cs:[ROMset],2	;Q: ROM specified?
	jne	short def_ON		; Y: turn CEMM ON
endif
	cmp	cs:[UMBset],TRUE	;Q: RAM specified?
	je	short def_ON		; Y: turn CEMM ON
	cmp	cs:[HMAonSet],TRUE	;Q: Virtual HMA requested?
	je	short def_ON		; Y: turn CEMM ON
	cmp	gs:[NoPFset],TRUE	;Q: FRAME=NONE specified?
	je	short def_ON		; Y: turn CEMM ON
	cmp	gs:[NoEMSset],TRUE	;Q: NoEMS specified?
	jne	short def_size		; Y: turn CEMM ON
def_ON:
	mov	gs:[UMBHMA],TRUE	; CEMM is providing UMBs or an HMA
	mov	gs:[Initial_Mode],MODE_ON; turn CEMM ON

def_size:
ifdef 910611
	cmp	cs:[pool_size],0	;Q: Did user specify size?
	jnz	short P_locate_windows	; Y: OK
	mov	cs:[pool_size],256	; N: default to 256K
P_locate_windows:
endif

	call	NoEMScheck

;  Make sure that the MIN and MAX EMS pool sizes make sense.  The
;  min_pool_size must be <= max_pool_size which must be <= total free memory.
;  If the max or min need to be adjusted from a value explictly set by the
;  user, flag the SIZE_ADJ_MSG.  Either or both parameters can be defaulted,
;  and if the user selects a min > max, raise the max.	We don't know how
;  much free memory is available at this time so that needs to be checked
;  later.

	mov	ax, [min_pool_size]	; make sure min_pool_size <= max_pool
	cmp	ax, [max_pool_size]
	jbe	short sizes_ok
	cmp	[min_pool_set], TRUE	; if MIN=size explictly given, adjust
	je	short adj_max		;   the max value up
	mov	ax, [max_pool_size]	; otherwise adjust the defaulted min
	mov	[min_pool_size], ax	;   value down
	jmp	short sizes_ok
adj_max:
	mov	[max_pool_size], ax
	cmp	[max_pool_set], TRUE	; if max was explictly set, tell user
	jne	short sizes_ok		;   we changed it
	or	gs:[msg_flag], SIZE_ADJ_MSG
sizes_ok:

;
; The EMS windows are located and the page frame is selected.
;
	call	FindWindowLocations
	cmp	gs:[PF_Base], FREE
	jne	SHORT P_check_for_warning

	cmp	gs:[NoEMSset],TRUE	;Q: NoEMS set?
	je	short parse_xit		; Y: continue

	cmp	gs:[NoPFset],TRUE	;Q: FRAME=NONE set?
	je	short parse_xit		; Y: continue

		; If this is reached then there are no valid page frames
	CMP	[xma2ems], TRUE		; Q: Is it in XMA2EMS mode?
	JNE	SHORT no_PF_no_XMA	; N: Bad!
	OR	GS:[msg_flag], NO_LIM_PF_MSG ; Set error flag
	JMP	SHORT P_check_for_warning
no_PF_no_XMA:
		; Set the message flag and load with NOEMS
	OR	GS:[msg_flag], NO_PF_MSG

	mov	gs:[NoEMSset], TRUE	; set NOEMS mode
	JMP	def_mode		; and try again

		; If the user supplied page frame has ROM in it then print
		; out a warning to him.
P_check_for_warning:
	cmp	cs:[PFB_warning],FALSE
	je	short parse_xit
	or	gs:[msg_flag],PF_WARN_MSG
parse_xit:				; restore registers and exit

	POP	FS
	POP	ES
	POP	DS
	POPA
	RET
Parser	ENDP

;******************************************************************************
;   Get_Token - Retrieves a token from DS:SI and stores it at ES:DI
;
;   ENTRY:	DS:SI points to string where a token is to be retrieved
;		ES:DI points to a buffer where a token is to be stored
;   EXIT:	DS:SI points to string where tokens remain to be retrieved
;		CX is the number of characters in ES:DI
;   USED:	CX, SI
;   STACK:	<amount of stack used by routine>
;------------------------------------------------------------------------------
	public	Get_Token
Get_Token	PROC
	PUSH	AX
	PUSH	DI
;
; while (DS[SI] == ' ' || DS[SI] == '\t') SI++;
;
SPorTB:					; Skip all white characters
	LODSB				; Get a character
	CMP	AL, ' '			; Q: Is it a space?
	JE	SPorTB			; Y: Skip it
	CMP	AL, TB			; Q: Is it a tab?
	JE	SPorTB			; Y: Skip it
;
; CX = 0;
; do {
;   if (DS[SI] == CR || DS[SI] == LF || DS[SI] == ' ' || DS[SI] == '\t')
;     break;
;   CX++;
;   ES[DI++] = toupper(DS[SI++]);
; } while (1);
; ES[DI++] = '\0';
;
	XOR	CX, CX			; Initialize counter
checkCH:
	CMP	AL, CR			; Q: Is this character a CR?
	JE	SHORT EOToken		; Y: it's the end of the token
	CMP	AL, LF			; Q: Is this character a LF?
	JE	SHORT EOToken		; Y: it's the end of the token
	CMP	AL, ' '			; Q: Is this character a space?
	JE	SHORT EOToken		; Y: it's the end of the token
	CMP	AL, TB			; Q: Is this character a tab?
	JE	SHORT EOToken		; Y: it's the end of the token
	INC	CX			; N: it's a valid character in token
	CMP	AL, 'a'			; Q: Is it less than 'a'?
	JB	SHORT storeAL		; Y: not 'a'..'z'
	CMP	AL, 'z'			; Q: Is it greater than 'z'?
	JA	SHORT storeAL		; Y: not 'a'..'z'
	AND	AL, NOT 20h		; Convert to 'A'..'Z'
storeAL:
	STOSB				; Store the character
	LODSB				; Retrieve another character
	JMP	checkCH
EOToken:
	XOR	AL, AL			; End of string
	STOSB
	POP	DI
	POP	AX
	RET
Get_Token	ENDP

;******************************************************************************
;   Parse_Token - Checks the validity of a token, and keeps it when valid
;
;   ENTRY:	ES:DI points to a buffer where a token is stored
;   EXIT:
;   USED:
;   STACK:	<amount of stack used by routine>
;------------------------------------------------------------------------------
	public	Parse_Token
Parse_Token	PROC
	PUSHAD
	PUSH	DS
	MOV	AX, LAST
	MOV	DS, AX
	ASSUME	DS: LAST

	MOV	SI, DI
	LODSB				; Get a character
	MOVZX	BX, AL
	MOVZX	EBX, BYTE PTR case[BX]
	JMP	switch[EBX*4]

Get_H:
	cmp	dword ptr ds:[si-1], 'HGIH' ; Q: HIGHSCAN?
	jne	HMAonParm
	cmp	dword ptr ds:[si+3], 'NACS'
	jne	HMAonParm
	mov	cs:[Highscan], TRUE
	add	si, 7
	jmp	PT_exit

HMAonParm:
	cmp	dword ptr ds:[si],'NOAM';Q: Is it HMAon parameter?
	je	short HMAon		; Y: process it
	LODSB
	CMP	AL, '='			; Is it H=ddd?
	JNE	inv_parm
is_$H:
;
; DX:AX = Get_Decimal(DS:SI);
;
	CMP	[$Hset], 0
	JNE	inv_parm
	CALL	Get_Decimal
	JC	inv_parm
	OR	DX, DX
	JNZ	inv_parm
	CMP	AX, 2
	JB	inv_parm
	CMP	AX, 255
	JA	inv_parm
	MOV	FS:[total_handles], AX
	MOV	GS:[ttl_hndls], AX
	MOV	[$Hset], NOT 0
	JMP	PT_exit
HMAon:
	add	si,4
	lodsb
	cmp	al,'='			;Q:HMAon=n?
	jne	short HoCont		; N: continue
	call	Get_Decimal		; Y: get minimum HMA size for allocation
	jc	inv_parm
	or	dx,dx			;Q: Invalid number (0<=n<=63)
	jnz	inv_parm		; Y: invalid parameter
	cmp	ax,63                   ;Q: Valid range (0<=n<=63)?
	ja	inv_parm		; N: invalid parameter
	shl	ax,10			; Y: mult by 1024 (1K)
	mov	fs:[HMAmin],ax		; and save for HMA min alloc size
;
;  Make sure this machine needs a virtual HMA
;
HoCont:
	mov	cs:[HMAonSet],TRUE
	mov	ax,8800h
	int	15h
	or	ax,ax
	jnz	short HoMsg
	mov	al,31h
	call	r_cmos
	mov	ah,al
	mov	al,30h
	call	r_cmos
	or	ah,al
	jnz	short HoMsg
	jmp	PT_exit
HoMsg:
	or	gs:[msg_flag],HMAon_MSG
	mov	cs:[HMAonSet],FALSE
	jmp	PT_exit

Get_P_Handle_Page_Frame:		; /
	LODSB
	CMP 	AL,'H'   		; Is it /Hddd?
	je	is_$H
	cmp	al,'P'			; Is it /Phhhh?
	je	short check_$P
	movzx	bx,al			; skip the "/" and continue processing
	movzx	ebx,byte ptr case[bx]
	jmp	switch[ebx*4]

check_$P:
	CMP	AL, 'P'			; Is it /Phhhh?
	JNE	inv_parm
;
; DX:AX = Get_Hexadecimal(DS:SI);
;
	CMP	GS:[PF_Base], FREE	; Q: Has the page frame been set?
	JNE	inv_parm		; Y: bad!
is_$P:
	CALL	Get_Hexadecimal
	JC	inv_parm
	OR	DX, DX
	JNZ	inv_parm
;
; ZF = Frame_Check(AX, CX);
;
	cmp	cs:[NoPFCset],TRUE	;Q: Check for valid page frame?
	je	short is_$P1		; N: skip check
	mov	cx,numFram		; Total number of valid page frames
	CALL	Frame_Check
	JNZ	inv_parm
is_$P1:
	MOV	GS:[PF_Base], AX
	JMP	PT_exit

Get_Size:				; 0123456789
	cmp	cs:[max_pool_set], TRUE ; Q: Has the max pool size been set?
	je	inv_parm		; Y: error
	DEC	SI
	CALL	Get_Decimal
	JC	inv_parm
	OR	DX, DX
	JZ	SHORT checkAX
	MOV	AX, MAX_SIZE
	JMP	SHORT size_err
checkAX:
	CMP	AX, MIN_SIZE
	JAE	SHORT check_MAX
	MOV	AX, MIN_SIZE
	JMP	SHORT size_err
check_MAX:
	CMP	AX, MAX_SIZE
	JA	SHORT use_MAX
	TEST	AL, 0Fh 		; Q: Is it a multiple of 16K?
	JZ	SHORT size_ok		; Y: Set pool size
	AND	AL, NOT 0Fh		; N: Round it down
	JMP	SHORT size_err
use_MAX:
	MOV	AX, MAX_SIZE
size_err:
	OR	GS:[msg_flag], SIZE_ADJ_MSG
size_ok:
	MOV	cs:[max_pool_size], AX
	mov	cs:[max_pool_set], TRUE
	JMP	PT_exit

Get_Mode_Alt_Reg_Sets:			; A
	LODSB
	CMP	AL, '='
	JNE	SHORT AltBootParm
	CMP	[altRset], 0
	JNE	inv_parm
	CALL	Get_Decimal
	JC	inv_parm
	OR	DX, DX
	JNZ	inv_parm
	CMP	AX, MAX_ALT_REG_SETS
	JA	inv_parm
	INC	AX
	MOV	FS:[total_register_sets], AL
	MOV	[altRset], NOT 0
	JMP	PT_exit

AltBootParm:
	cmp	dword ptr ds:[si-1], 'OBTL' ; Q: ALTBOOT?
	jne	short check_AUTO
	cmp	word ptr ds:[si+3], 'TO'
	jne	short check_AUTO
	.erre	fTurnOffNOW AND 0FF00h
	or	byte ptr gs:[genflags+1], fTurnOffNOW SHR 8
	add	si, 6
	jmp	PT_exit

check_AUTO:
	CMP	GS:[Initial_Mode], FREE	; Q: Has the mode been set?
	JNE	inv_parm		; Y: bad!
	SUB	SI, 2
	MOV	DI, OFFSET op_auto
	MOV	CX, AUTOLEN
	REPE	CMPSB
	JNZ	inv_parm
	MOV	GS:[Initial_Mode], MODE_AUTO
	JMP	PT_exit
Get_Lowest_Frame:			; B
	CMP	[baseset], 0
	JNE	inv_parm
	LODSB
	CMP	AL, '='
	JNE	inv_parm
	int	12h			; Get conventional memory size QLEO
	shl	ax,6			; In paragraph                 QLEO
	mov	bx,ax			;			       QLEO
	CALL	Get_Hexadecimal
	JC	inv_parm
	OR	DX, DX
	JNZ	inv_parm
	CMP	AX, 1000h		; At least 1000h
	JB	inv_parm
	cmp	ax,bx			;Q: Check upper limit?         QLEO
	jbe	short GLFcont		; N: OK, continue              QLEO
	mov	ax,bx                   ; Y: Use INT 12h size	       QLEO
;QLEO	CMP	AX, 04000h		; At most 4000h                QLEO
;QLEO	JA	inv_parm                                               QLEO
GLFcont:				;                              QLEO

	ADD	AX, 3FFh		; In case it's between 16K frames
	AND	AX, NOT 3FFh		; Must be 16K increment
	XOR	EBX, EBX
	MOV	BX, R_CODE
	SHL	EBX, 4
	ADD	BX, OFFSET end_of_R_CODE
	SHR	EBX, 4
	ADD	EBX, 3FFh
	AND	EBX, NOT 3FFh
	CMP	BX, AX			; Q: Is the B= below R_CODE?
	JB	SHORT use_original	; N: Use B=
	MOV	AX, BX			; Y: Use window above R_CODE
use_original:
	dec	ax
	mov	cs:[Bparm],ax
	inc	ax
	SHR	AX, 8
	MOV	FS:[strtng_bs_wndw_PTE], AX
	MOV	[baseset], NOT 0
	JMP	PT_exit
Get_DMA_Buffer_Size:			; D
	cmp	dword ptr ds:[si],'TXAM'; DMAXT?
	jne	short GDBScont
	or	fs:[DMAFlags],fDMABuffXT; put DMA buffer in first meg
	add	si,4
	jmp	PT_exit

GDBScont:
	CMP	[bDMAset], 0
	JNE	inv_parm
	LODSB
	CMP	AL, '='
	JNE	inv_parm
	XOR	EAX, EAX
	CALL	Get_Decimal
	JC	inv_parm
	OR	DX, DX
	JNZ	inv_parm
	CMP	AX, 16
	JB	inv_parm
	CMP	AX, 256
	JA	inv_parm
	SHL	EAX, 10			; In bytes
	MOV	FS:[DMABufferSize], EAX
	MOV	[bDMAset], NOT 0
	JMP	PT_exit
Get_Page_Frame:				; F
	CMP	GS:[PF_Base], FREE	; Q: Has the page frame been set?
	JNE	inv_parm		; Y: Bad!
	DEC	SI			; Compare the whole "FRAME=" string
	MOV	DI, OFFSET frame$
	MOV	CX, framLen
	REPE	CMPSB
	JNZ	inv_parm
	cmp	dword ptr [si],'ENON'	;Q: "FRAME=NONE"
	jne	is_$P			; N: try hexadecimal number
	mov	gs:[NoPFset],TRUE	; Y: mark no page frame
	add	si,4
	jmp	PT_exit
Get_Left_Alone_Size:			; L
	CMP	[ext_mem], FREE
	JNE	inv_parm
	LODSB
	CMP	AL, '='
	JNE	inv_parm
	CALL	Get_Decimal
	JC	inv_parm
	test	dx,not 3Fh		;Q: Greater than 4GB (in KB increments)?
	JNZ	inv_parm		; Y: invalid!
	or	dx,dx			;Q: Greater than 64MB?
	jnz	short GLASok		; Y: OK
	OR	AX, AX			; At least 1 KB
	JZ	inv_parm
GLASok:
	MOV	word ptr [ext_mem], AX
	MOV	word ptr [ext_mem][2],dx
	JMP	PT_exit
NoMoveXBDA:				  ; N undocumented NOMOVEXTBIOS
	cmp	dword ptr ds:[si-1],'OMON';Q: correct switch?
	jne	short NoEMS		  ; N: check for "NOEMS"
	cmp	dword ptr ds:[si+3],'BXEV';Q: correct switch?
	jne	inv_parm		  ; N: Bad!
	cmp	word ptr ds:[si+7],'AD'	  ;Q: correct switch?
	jne	inv_parm		  ; N: Bad!
	or	gs:[GenFlags],fXBDAnotRel ; Y: don't move Extended BIOS data area
	add	si,9
	JMP	PT_exit
NoEMS:
	cmp	dword ptr ds:[si],'SMEO';Q: "NOEMS"?
	jne	short NoINTparm		; N: Check if "NoINT parameter is indicated
	mov	gs:[NoEMSset],TRUE	; Y: set flag
	add	si,4
	jmp	PT_Exit
NoINTparm:
	cmp	dword ptr ds:[si],'TNIO';Q: correct switch?
	jne	short NoLocUMBparm	; N: Check for "NoLocUMBs" parameter.
	or	gs:[GenFlags],fNoINT	; Y: set flag for no interrupts
	add	si,4
	jmp	PT_exit
NoLocUMBparm:
ifndef MSFLAG
	cmp	dword ptr ds:[si],'COLO'  ;Q: correct switch?
	jne	short NoPFCparm		  ; N: Check for "NoPFC" parameter.
	cmp	dword ptr ds:[si+4],'SBMU';Q: correct switch?
	jne	short NoPFCparm		  ; N: Check for "NoPFC" parameter.
	or	gs:[GenFlags],fNoLocUMB   ; Y: set flag for no interrupts
	add	si,8
	jmp	PT_exit
endif
NoPFCparm:
	cmp	dword ptr ds:[si],'CFPO';Q: correct switch?
	jne	NoA20TrapParm		; N: Bad!
	mov	cs:[NoPFCset],TRUE	; Y: Don't check page frame addresses
	add	si,4
	jmp	PT_exit

ifndef LC910611
NoA20TrapParm:
	cmp	dword ptr ds:[si],'02AO'  ;Q: correct switch?
	jne	short NoVCPIparm	  ; N:
	cmp	dword ptr ds:[si+4],'PART';Q: correct switch?
	jne	short NoVCPIparm	  ; N:
	or	gs:[GenFlags],fNoA20Trap  ; Y: don't trap A20 line
	add	si,8
	jmp	PT_exit
endif

NoVCPIparm:
	cmp	dword ptr ds:[si], 'PCVO'   ; Q: NOVCPI?
	jne	short NoHighparm
	cmp	byte ptr ds:[si+4],'I'
	jne	short NoHighparm
	mov	cs:[NoVCPI], TRUE
	add	si, 5
	jmp	PT_exit

NoHighparm:
	cmp	dword ptr ds:[si],'IHO';Q: "NOHI"?
	jne	NoTRparm
	mov	cs:[NoHigh],TRUE	; Y: set flag
	add	si,3
	jmp	PT_Exit

NoTRparm:
	cmp	dword ptr ds:[si],'RTO' ; Q: NOTR?
	jne	inv_parm		; N: Bad!
	mov	cs:[NoTR], TRUE
	add	si, 3
	jmp	PT_exit

Get_M:					; M
	cmp	word ptr ds:[si],'NI'	; Q: MIN=?
	jne	Get_Window_Frame
	add	si, 2
	lodsb
	cmp	al, '='
	jne	inv_parm
	cmp	cs:[min_pool_set], TRUE ; Q: already set min pool size?
	je	inv_parm		; Y: error
	call	Get_Decimal
	jc	inv_parm
	or	dx, dx
	jz	checkMinAX
	mov	ax, MAX_SIZE
	jmp	short min_size_err
checkMinAX:
	cmp	ax, MAX_SIZE
	ja	useMaxPool
	test	al, 0Fh
	jz	short min_size_ok
	and	al, NOT 0Fh
	jmp	short min_size_err
useMaxPool:
	mov	ax, MAX_SIZE
min_size_err:
	or	gs:[msg_flag], SIZE_ADJ_MSG
min_size_ok:
	mov	cs:[min_pool_size], ax
	mov	cs:[min_pool_set], TRUE
	jmp	PT_exit

Get_Window_Frame:			; Mx
	CMP	GS:[PF_Base], FREE	; Q: Has the page frame been set?
	JNE	inv_parm		; Y: Bad!
	CALL	Get_Decimal
	JC	inv_parm
	OR	DX, DX
	JNZ	inv_parm
	OR	AX, AX			; At least 1
	JZ	inv_parm
	CMP	AX, 14			; At most 14
	JA	inv_parm
	MOV	BX, LAST
	MOV	DS, BX
	ASSUME	DS: LAST

	DEC	AX
	ADD	AX, AX
	MOV	BX, AX
	ADD	BX, OFFSET frames
	MOV	AX, [BX]
	MOV	GS:[PF_Base], AX
	JMP	PT_exit
Get_On_Off_Mode:			; O
	CMP	GS:[Initial_Mode], FREE	; Q: Has the mode been set?
	JNE	inv_parm		; Y: Bad!
	DEC	SI			; Retrieve last character
	MOV	BX, SI			; Save SI in case it's not ON
	MOV	DI, OFFSET op_on
	MOV	CX, ONLEN
	REPE	CMPSB
	JNZ	SHORT check_off
	MOV	GS:[Initial_Mode], MODE_ON
	JMP	PT_exit
check_off:
	MOV	SI, BX			; Retrieve SI
	MOV	DI, OFFSET op_off
	MOV	CX, OFFLEN
	REPE	CMPSB
	JNZ	inv_parm
	MOV	GS:[Initial_Mode], MODE_OFF
	JMP	PT_exit
Get_I_Page_Frame:			; P
	CALL	Get_Decimal
	JNC	inv_parm
	CMP	AX, 3			; Q: Is this P0..P3?
	JNA	SHORT check_equal	; Y:
check_254:
	CMP	AX, 254			; Q: Is this in P254..P255?
	JB	inv_parm		; N:
	OR	AH, AH			; Q: Is this in P254..P255?
	JNZ	inv_parm		; N:
	SUB	AX, 250			; Get the proper index
check_equal:
ifdef PICtrap
	cmp	word ptr ds:[si-1],'CI'	;Q: Undocumented "PIC" switch?
	je	short PICVec		; Y: get values
endif
	MOV	BX, AX
	CMP	pn[BX], FREE		; Q: Is this P already set?
	JNE	inv_parm		; Y: error
	CMP	BYTE PTR DS:[SI-1], '='	; Q: Is '=' present?
	JNE	inv_parm		; N: error
	CALL	Get_Hexadecimal		; Retrieve the number following '='
	JC	inv_parm		; If number is invalid, error
	OR	DX, DX			; Q: Is number too big?
	JNZ	inv_parm		; Y: error
	cmp	cs:[NoPFCset],TRUE	;Q: Check for valid page frame?
	je	short PFok		; N: skip check
	MOV	CX, numPFrm		; Total number of valid windows
	CALL	Frame_Check		; Q: Is this P valid?
	JNZ	inv_parm		; N: error
PFok:
	XCHG	AH, AL
	MOV	CX, 6			; Compare this P with others
	MOV	DI, OFFSET pn
	REPNE	SCASB			; Q: Is this P a duplicate?
	JE	inv_parm		; Y: error
pn_okay:
	MOV	Pn[BX], AL		; N: store this P
	BTS	[PnSet], BX		; Set flag for this Pn
	JMP	PT_exit			; Done for this parameter
ifdef PICtrap
PICVec:
	cmp	word ptr ds:[si+1],':S'	;Q: Undocumented "PICS:" switch?
	je	short PICSVec		; Y: get values
	cmp	word ptr ds:[si+1],':M'	;Q: Undocumented "PICM:" switch?
	jne	inv_parm		; N: error
	add	si,3
	call	Get_Hexadecimal
	jc	inv_parm
	mov	gs:[MasterPICVec],ax
	jmp	PT_exit
PICSVec:
	add	si,3
	call	Get_Hexadecimal
	jc	inv_parm
	mov	gs:[SlavePICVec],ax
	jmp	PT_exit
endif

EMSInts:				  ; EMS=FROM-TO
	cmp	dword ptr ds:[si],'NISM'  ;Q: "EMSINTS" switch?
	jne	short EMSrange		  ; N: "EMS=" parameter?
	cmp	word ptr ds:[si+4],'ST'	  ;Q: "EMSINTS" switch?
	jne	short EMSrange		  ; N: "EMS=" parameter?
	add	si,6			  ; Y: get to "=" sign
	and	gs:[GenFlags],not fNoEMSInt ; clear flag for no EMS interrupts
	jmp	PT_exit
EMSrange:				  ; EMS=FROM-TO
	cmp	dword ptr ds:[si-1],'=SME';Q: "EMS=" switch?
	jne	inv_parm		  ; N: invalid parameter
	add	si,2			  ; Y: get to "=" sign
	lea	bx,[EMSSet]
	jmp	GetRange

GetRAM:
	cmp	dword ptr ds:[si-1],'=MOR';Q: "ROM=" switch?
	je	short ROMrange		  ; Y: get range
	cmp	dword ptr ds:[si-1],'=MAR';Q: "RAM=" switch?
	je	short RAMrange		  ; Y: get range
	cmp	word ptr ds:[si],'MO'	;Q: "ROM" switch?
	je	short ROMparam		  ; Y: get range
	cmp	word ptr ds:[si],'MA'	;Q: "RAM" switch?
	jne	inv_parm		; N: invalid parameter
	mov	cs:[UMBset],TRUE
	add	si,3
	jmp	PT_exit
RAMrange:           			; RAM=FROM-TO
	add	si,2			; get to "=" sign
	lea	bx,[RAMSet]
	jmp	GetRange
ROMrange:           			; ROM=FROM-TO
	add	si,2			; get to "=" sign
	lea	bx,[ROMSet]
	jmp	GetRange
ROMparam:           			; ROM(COMPRESS)
	dec	si			; start at "R" again
	mov	cs:[ROMparm],TRUE	; assume it compares
	push	es			; save current ES
	push	cs			; make ES:DI point to "ROMCOMPRESS"
	pop	es
	lea	di,ROMCompParm
	mov	cx,ROMCompLen		; length of string
	cld
      	rep cmpsb			;Q: Does it match
	pop	es
	jz	PT_exit			; Y: OK
	dec	si			; N: check why not
	cmp	byte ptr ds:[si],' '	;Q: Was it due to a space?
	je	PT_exit			; Y: OK
	cmp	byte ptr ds:[si],'0'	;Q: Was it another character
	jb	PT_exit			; N: OK
	cmp	byte ptr ds:[si],'Z'
	ja	PT_exit
	mov	cs:[ROMparm],FALSE	; bad parameter
	jmp	inv_parm
Get_Wetek_Mode: 			; W
	cmp	byte ptr ds:[si], '='
	jne	WinParm
	CMP	GS:[Initial_Weitek], FREE; Q: Has the weitek been set?
	JNE	inv_parm		; Y: Bad!
	inc	si
	MOV	BX, SI
	MOV	DI, OFFSET op_on
	MOV	CX, ONLEN
	REPE	CMPSB
	JNE	SHORT check_woff
	MOV	GS:[Initial_Weitek], 1
	JMP	PT_exit
check_woff:
	MOV	SI, BX
	MOV	DI, OFFSET op_off
	MOV	CX, OFFLEN
	REPE	CMPSB
	JNZ	inv_parm
	MOV	GS:[Initial_Weitek], 0
	JMP	PT_exit

WinParm:
	cmp	dword ptr ds:[si-1], '=NIW'	;Q: WIN=?
	jne	inv_parm
	add	si,2			; get to "=" sign
	lea	bx,[WINset]
	jmp	GetRange

GetFullPath:				; M010 - Start
	lodsb
	cmp	al, '='
	jne	inv_parm
	call	StorePath		; save user specified path in CEMMPath
	jmp	PT_exit			; M010 - End

Vir8042Parm:
	cmp	dword ptr ds:[si],'08RI'  ;Q: correct switch?
	jne	VerboseParm		  ; N:
	cmp	word ptr ds:[si+4],'24'	  ;Q: correct switch?
	jne	VerboseParm		  ; N:
	or	gs:[GenFlags],fVir8042	  ; Y: virtualize 8042 (keyboard)
	add	si,6
	jmp	PT_exit

VerboseParm:
	dec	si			; backup to 'V'
	mov	di, OFFSET verbose$	; 'VERBOSE'
	mov	cx, verboseLen
	repe cmpsb
	jz	got_verbose
	cmp	byte ptr ds:[si-1], 0	; allow V, VE, VER, ... VERBOSE
	jnz	inv_parm
got_verbose:
	mov	cs:[Verbose], TRUE
	jmp	PT_exit

GetExcludeRange:			; X=FROM-TO
	lea	bx,[eXcSet]
	jmp	short GetRange

GetIncludeRange:			; I=FROM-TO
	lea	bx,[IncSet]
	jmp	short GetRange


;==============================================================================
;==
;==  GetRange:  Routine which extracts the range from a parameter of the form:
;==
;==		[variable]=xxxx-yyyy (xxxx/yyyy: hexadecimal segment values)
;==
;==  Enter:
;==	DS:SI	pointer to parameter at "=" sign.
;==	DS:[BX]	pointer to a word array which will store the range information.
;==
;==  Exit:
;==	DS:SI	points to blank after the parameter
;==	DS:[BX] array is updated to include the range.
;==
;==============================================================================
	public	GetRange
GetRange:
	lodsb
	cmp	al,'='			;Q: Possible range?
	jne	short inv_parm		; N: invalid parameter
	int	12h			; Get conventional memory size
	shl	ax,6			; In paragraph
	mov	cx,ax
	call	Get_Hexadecimal		; Get FROM
	or	dx,dx
	jnz	short inv_parm
	cmp	ax,cx			; Valid range: INT 12h-FFFF
	jb	short inv_parm

	movzx	di,byte ptr [bx]
	mov	[bx][di],ax		; save from value

	cmp	byte ptr ds:[si-1], '-'	;Q: Is there a TO?
	jne	short GFskipTO		; N: TO = FROM
	mov	cx,ax			; Save FROM
	call	Get_Hexadecimal		; Y: Get TO
	jc	short GFuseFROM
	or	dx,dx
	jnz	short GFuseFROM
	cmp	ax,cx			;Q: TO > FROM?
	jb	short GFuseFROM		; N: TO cannot be less tham FROM

GFskipTO:
	add	di,2
	add	byte ptr [bx],4
	mov	[bx][di],ax
	jmp	short PT_exit

GFuseFROM:
	mov	ax,cx
	or	gs:[msg_flag],INV_PARM_MSG ; set invalid parameter flag
	jmp	short GFskipTO


Invalid_Parameter:
inv_parm:
	OR	GS:[msg_flag], INV_PARM_MSG ; set invalid parameter flag
PT_exit:
	POP	DS
	POPAD
	RET
Parse_Token	ENDP

;
; DX:AX = Get_Decimal
;
;******************************************************************************
;   Get_Decimal - Translates ASCII decimal numbers
;
;   ENTRY:	DS:SI points to a buffer where a string is to be translated
;   EXIT:	DX:AX stores the number
;		CY is set if error detected
;   USED:
;   STACK:	<amount of stack used by routine>
;------------------------------------------------------------------------------
	public	Get_Decimal
Get_Decimal	PROC
	push	ebx
	push	eax

	xor	eax,eax			; clear sum and current digit
	xor	ebx,ebx
	LODSB
	OR	AL, AL			;Q: Anything there?
	JZ	SHORT invalid_ddd	; N: error
	JMP	SHORT check_AL		; Y: check if it's a digit

next_ddd:
	LODSB				; get next digit
	OR	AL, AL			;Q: Anything there?
	jnz	short check_AL		; Y: check if it's a digit
	pop	eax			; N: return total in DX:AX
	mov	ax,bx			; low order word in AX
	shr	ebx,16			; high order word in DX
	mov	dx,bx
	clc
	jmp	short GDexit

check_AL:
	CMP	AL, '0'			;Q: Is it an ASCII digit?
	JB	SHORT invalid_ddd       ; N: error
	CMP	AL, '9'			;Q: Is it an ASCII digit?
	JA	SHORT invalid_ddd	; N: error
	SUB	AL, '0'			; make it an integer
	add	ebx,ebx			; multiply current total by two
	test	ebx,0E0000000h		;Q: Reaching overflow of 32-bits?
	jnz	short invalid_ddd	; Y: must be an error! (number too large)
	lea	ebx,[ebx*4+ebx]		; N: multiply total by ten
	add	ebx,eax			; add new digit to total
	JMP	next_ddd

invalid_ddd:
	pop	eax
	mov	ax,bx
	shr	ebx,16
	mov	dx,bx
	STC

GDexit:
	pop	ebx
	ret
Get_Decimal	ENDP

;
; DX:AX = Get_Hexadecimal
;
;******************************************************************************
;   Get_Hexadecimal - Translates ASCII hexadecimal numbers
;
;   ENTRY:	DS:SI points to a buffer where a string is to be translated
;   EXIT:	DX:AX stores the number
;		CY is set if error detected
;   USED:
;   STACK:	<amount of stack used by routine>
;------------------------------------------------------------------------------
	public	Get_Hexadecimal
Get_Hexadecimal	PROC
	PUSH	BX
	PUSH	CX
	XOR	AX, AX			; bottom part of result
	LODSB
	OR	AL, AL
	JZ	SHORT invalid_hhh
	XOR	BX, BX			; Sum
	XOR	DX, DX			; top part of result
	MOV	CX, 16			; Hexadecimal
	JMP	SHORT check_hh
next_hhh:
	LODSB
	OR	AL, AL			; also clears carry
	JZ	SHORT no_hhh
check_hh:
	CMP	AL, 'A'
	JB	SHORT check_dd
	CMP	AL, 'F'
	JA	SHORT invalid_hhh
	SUB	AL, 'A'
	ADD	AL, 10
	JMP	SHORT calculator
check_dd:
	CMP	AL, '0'
	JB	SHORT invalid_hhh
	CMP	AL, '9'
	JA	SHORT invalid_hhh
	SUB	AL, '0'
calculator:
	XCHG	AX, BX
	MUL	CX
	ADD	AX, BX
	ADC	DX, 0
	XCHG	AX, BX
	JMP	next_hhh
invalid_hhh:
	STC
no_hhh:
	XCHG	AX, BX
	POP	CX
	POP	BX
	RET
Get_Hexadecimal	ENDP

;
; ZF = Frame_Check(AX);
;
;******************************************************************************
;   Frame_Check - Checks if the page frame is valid
;
;   ENTRY:	ED:DI points to a buffer where valid frames are stored
;		AX stores the number to be checked
;   EXIT:	ZR is frame is invalid
;   USED:
;   STACK:	<amount of stack used by routine>
;------------------------------------------------------------------------------
	public	Frame_Check
Frame_Check	PROC
	PUSH	ES
	PUSH	DI
	LES	DI, framadr
	CLD
	REPNE	SCASW
	POP	DI
	POP	ES
	RET
Frame_Check	ENDP

;==============================================================================
;==
;==  NoEMScheck: If the [NoEMS] parameter is chosen.  Changes all the defaults
;==		 to a minimum configuration.
;==
;==
;==============================================================================
	public	NoEMScheck
	assume	cs:LAST,ds:_DATA,gs:R_CODE
NoEMScheck proc	near

	cmp	gs:[NoEMSset],TRUE	; Q: Was [NoEMS] set?
	je	NEcCont			; Y: continue
	cmp	cs:[max_pool_size],0	; Q: Did user specify size?
	jnz	short NEcDone		; Y: OK
	mov	cs:[max_pool_size], MAX_SIZE	; default to large EMS size
NEcDone:
	ret

NEcCont:

	; Since noems has been specifed we need to setup with frame=none
	; parms and also provide a 0 pool size if the user has not specifed
	; any.

	mov	gs:[NoEMSset], FALSE
	mov	gs:[NoPFset], TRUE

	;
	; set up VCPIset to imply the foll.
	;
	; 	VCPIset = -1 => NOEMS has not been specifed (default)
	;	VCPIset = TRUE	=> NOEMS has been specifed
	;	VCPIset = FALSE => NOEMS+NOVCPI has been specifed
	;
	mov	gs:[VCPIset], FALSE	; assume NOEMS + NOVCPI

	cmp	cs:[NoVCPI], TRUE	; Q: did user disable VCPI too?
	je	short NEcSetNoems	; Y:

	mov	gs:[VCPIset], TRUE	; N: VCPI = TRUE

	cmp	cs:[min_pool_set], TRUE ; default min pool size with NOEMS
	je	short NEcMinSet 	;   is 0
	mov	cs:[min_pool_size], 0
NEcMinSet:
	cmp	cs:[max_pool_set], TRUE ; default to large EMS/VCPI size
	je	short NEcMaxSet 	;     if VCPI support enabled
	mov	cs:[max_pool_size], MAX_SIZE
NEcMaxSet:

	; Note: we'll set up the device name to EMMQXXX. This will enable
	; Lotus 123 to work with noems option.

	mov	gs:[DevName+3],'Q'

	; Some programs can't detect EMM386 when it's installed with the
	; 'EMMQXXX0' name, so when this name is used add a second device
	; driver header with the name '$MMXXXX0' that these programs do
	; know to detect.

	mov	gs:[DEVHEAD], offset R_CODE:DEVHEAD2	;link in second header
	ret


	; User doesn't want EMS or VCPI, must be a UMB only type of guy.

NEcSetNoems:

ifdef QLEO
	or	ax,ax
	jz	short NEcAltRegSets
	or	gs:[msg_flag],NoEMS_MSG
NEcAltRegSets:
endif

	mov	ds:[total_register_sets],2

ifdef QLEO
	cmp	cs:[altRset],0
	je	short NEcHandle
	or	gs:[msg_flag],NoEMS_MSG
NEcHandle:
endif

	mov	[total_handles],2
	mov	gs:[ttl_hndls],2

ifdef QLEO
	cmp	cs:[$Hset],0
	je	short NEcBase
	or	gs:[msg_flag],NoEMS_MSG
NEcBase:
	cmp	cs:[baseset],0
	je	short NEcPn
	or	gs:[msg_flag],NoEMS_MSG
NEcPn:
	cmp	cs:[PnSet],0
	je	short NEcFrame
	or	gs:[msg_flag],NoEMS_MSG
NEcFrame:
	cmp	gs:[PF_Base],FREE
	je	short NEcExit
	or	gs:[msg_flag],NoEMS_MSG
endif
	mov	gs:[DevName],'$'
NEcExit:
	ret
NoEMScheck	endp

;==============================================================================
;==
;==  GetEMSPool: This subroutine preallocates the EMS pool for CEMM.
;==
;==  Entry: (Real Mode)
;==	CS:[min_pool_size] = minimum EMS pool size requested
;==
;==  Exit:
;==	CS:[min_pool_size]	= preallocated EMS pool size
;==	CS:[ext_size]  	        = extended memory pool size (size of pool 1)
;==	CS:[hi_size]   	        = BIM pool size             (size of pool 2)
;==	CS:[ext_memory_address] = extended pool address (address of pool 1)
;==	CS:[high_memory_address]= BIM pool address	(address of pool 2)
;==
;==============================================================================
	public	GetEMSPool
	assume	ds:_DATA,es:ABS0,gs:R_CODE
GetEMSPool proc	near
;
;  Initialize variables
;
	xor	eax,eax
	mov	cs:[ext_memory_address],eax
	mov	cs:[high_memory_address],eax
	mov	cs:[ext_size],ax
	mov	cs:[hi_size],ax
;
;  Leave atleast L=nnn extended memory after CEMM installs (default: L=0)
;
;+++
	CMP	CS:[ext_mem], FREE	; Q: Is L=ddd set?
	JE	SHORT skip_left_alone	; N: Skip these steps
	CALL	TotalFreeMem		; EAX = TotalFreeMem();
	JC	GEPerror
	SHR	EAX, 10			; In K bytes
	MOV	EBX, CS:[ext_mem]
	SUB	EAX, EBX
	JBE	GEPerror
	MOVZX	EBX, cs:[max_pool_size]
	CMP	EAX, EBX
	JAE	SHORT skip_left_alone
	CMP	AX, MIN_SIZE
	JB	GEPerror
	AND	AX, NOT 0Fh		; Round it down to multiple 16K	;@PIW
	OR	GS:[msg_flag], SIZE_ADJ_MSG; Memory size adjusted message;@PIW
	MOV	cs:[max_pool_size], AX
	cmp	ax, cs:[min_pool_size]	; Max EMS pool size adjusted, don't
	jae	skip_left_alone 	;   let Min size be larger than Max
	mov	cs:[min_pool_size], ax
skip_left_alone:
;+++
;
;  Try get all of preallocated EMS pool from BIM
;
	xor	ebx,ebx
	movzx	eax,cs:[min_pool_size]	; size of preallocated pool requested
	or	ax,ax			;Q: Any EMS pool needed?
	jz	short GEPcont		; N: no memory used
	shl	eax,10			; in bytes
	mov	bx,fBIMMem
	shl	ebx,16
	mov	bx,EMS_BOUNDARY		; EMS bound and BIM

	call	MemGet			;Q: Enough BIM for EMS pool?
	jc      short GEPnotEnoughBIM	; N: not enough
GEPcont:				; Y: save starting address
	mov	[high_memory_address],ebx
	shr	eax,10			; size in Kbytes
	mov	[hi_size],ax

	jmp	GEPexit

;
; Try get what is available from BIM
;
GEPnotEnoughBIM:
	and	eax,not (4000h-1)	; round down to 16 Kbytes
	or	eax,eax			;Q: Any BIM available?
	jnz	short GEPhi		; Y: allocate all of it?
	movzx	eax,cs:[min_pool_size]	; N: allocate any type of memory
	shl	eax,10			; size in bytes
	mov	ebx,EMS_BOUNDARY	; must be 4K aligned

GEPhi:
	and	eax,not (4000h-1)	; round down to 16 Kbytes
	or	eax,eax			;Q: Any memory to allocate?
	jz	short GEPerror		; N: no memory available

	call	MemGet			;Q: Allocate hi memory?
	jc	GEPhi			; N: try hi memory one more time?
	mov	[high_memory_address],ebx;Y: save starting address

	shr	eax,10			; size in 1 Kbytes
	mov	[hi_size],ax
	sub	ax,cs:[min_pool_size]	; check if any more is needed
	neg	ax
	shl	eax,10			; memory still needed

	or	eax,eax			;Q: Any more needed?
	jz	short GEPexit           ; N: exit, all memory allocated

GEPlo:
	and	eax,not (4000h-1)	; round down to 16 Kbytes
	or	eax,eax			;Q: Any memory to allocate?
	jz	short GEPless		; N: report less memory allocated

	mov	ebx,EMS_BOUNDARY	; must be 4K aligned

	call	MemGet			;Q: Get the rest needed?
	jc	short GEPlo		; N: get what ever you can
	mov	cs:[ext_memory_address],ebx; Y: save starting address
	shr	eax,10			; size in Kbytes
	mov	cs:[ext_size],ax
	add	ax,cs:[hi_size]		; total allocated

	cmp	ax,MIN_SIZE		;Q: Did we get atleast 64K?
	jb	short GEPerror		; N: exit, not enough memory
	cmp	ax,cs:[min_pool_size]	;Q: Did we get all we needed?
	jae	short GEPexit		; Y: exit, all memory allocated

;
;  All memory was not allocated
;
GEPless:
	mov	ax,cs:[hi_size]  		; amount allocated
	add	ax,cs:[ext_size]
	mov	cs:[min_pool_size],ax
	mov	cs:[max_pool_size],ax
	or	gs:[msg_flag],SIZE_ADJ_MSG ; memory size adjusted message
	jmp	short GEPexit

;
;  No memory was allocated
;
GEPerror:
	or	gs:[msg_flag],MEM_ERR_MSG ; no memory available

GEPexit:
	mov	ax, cs:[max_pool_size]	; save Min/Max EMS pool sizes for
	mov	gs:[MaxEMSpool], ax	;   IOCTL interface reporting
	mov	ax, cs:[min_pool_size]
	mov	gs:[MinEMSpool], ax

	call	UpdateHandleSpace
	ret
GetEMSPool 	endp

;==============================================================================
;==
;==  UpdateHandleSpace: This subroutine updates the handle space with the free
;==			EMS memory allocated in GetEMSPool.
;==
;==  Entry: (Real Mode)
;==	CS:[ext_size]  	        = extended memory pool size (size of pool 1)
;==	CS:[hi_size]   	        = BIM pool size             (size of pool 2)
;==	CS:[ext_memory_address] = extended pool address (address of pool 1)
;==	CS:[ext_memory_address] = BIM pool address      (address of pool 2)
;==
;==  Exit:
;==	handle space updated
;==
;==============================================================================
	public	UpdateHandleSpace
UpdateHandleSpace proc	near
;
;  Initialize the handle space management data structures
;
	mov	bx, cs:[handle0_PTEs]
	add	bx, FIRST_HANDLE_PTE

	mov	ax, cs:[max_pool_size]		; TopOfHandleSpace has to map
	shr	ax, 2				;   the max amount of EMS space
	add	ax, bx				;   that can be allocated
	mov	[TopOfHandleSpace], ax
	mov	[BotOfVCPIspace], ax		; No VCPI space initially

	mov	ax,cs:[min_pool_size]		; KB of preallocated EMS memory
	shr	ax,2                            ; 4 KB size
	add	ax, bx				; size of total EMS pool
	mov	[TopOfFreeEMSspace],ax		; existing but free EMS memory

	mov	ax,cs:[handle0_PTEs]		; EMS 4K pages already allocated
	mov	[TopOfUsedEMSspace],ax
	add	[TopOfUsedEMSspace],FIRST_HANDLE_PTE
	shr	ax,2
	mov	[UsedEMSPages],ax		; EMS memory allocated by handle 0
;
;  Get destination address
;
	movzx	edi,cs:[handle0_PTEs]
	add	di,FIRST_HANDLE_PTE
	shl	edi,2
	add	edi,[page_directory]
;
;  Update with hi pool first
;
	mov	esi,cs:[high_memory_address]
	shr	esi,10				; PTE offset into page tables
	add	esi,[page_tables]
	movzx	ecx,cs:[hi_size]		; size in KB, also number of byte to tx
	jcxz	short UHSgetExt
	call	DoMoveBlock
	jc	short UHSerror
	add	edi,ecx
;
;  Now, update with ext pool
;
UHSgetExt:
	mov	esi,cs:[ext_memory_address]
	shr	esi,10				; PTE offset into page tables
	add	esi,[page_tables]
	movzx	ecx,cs:[ext_size]			; size in KB, also number of byte to tx
	jcxz	short UHSexit
	call	DoMoveBlock
	jc	short UHSerror
UHSexit:
	ret
UHSerror:
	or	gs:[msg_flag],MEM_ERR_MSG
	jmp	short UHSexit
UpdateHandleSpace 	endp
;==============================================================================
;==
;==  EstWorkSpace: Estimates amount of work space.
;==
;==  Enter:
;==
;==  Exit:
;==	EBX = estimate size of memory required for work space: data/code (bytes)
;==
;==============================================================================
EstWorkSpace	proc	near
	push	eax
	push	edx

	mov	bx,gs:[total_handles]
	mov	eax,size HandleTable_struc+size HandleName_struc+size HandleSaveMap_struc+size save_flag+size EMM_Handle
;QEMS	mov	eax,size HandleTable_struc+size HandleName_struc+size EMS_window_struc * TOTAL_PF_WINDOWS+size save_flag+size EMM_Handle
	mul	bl			; Size of handle related data
	mov	edx,eax
	mov	bl,size RegisterSet_struc
	mov	al,[total_register_sets]
	mul	bl			; Size of total_register_sets
	add	eax,edx
	add	eax,offset _DATA:[end_of_DATA]
	add	eax,offset _TEXT:[end_of_TEXT]
	add	eax,offset STACK:[end_of_STACK]
;TSSQLEO add	eax,TSSLEN
	add	eax,IDTLEN
	add	eax,GDTLEN		; space for TSS,IDT, & GDT
	mov	ebx,eax

	pop	edx
	pop	eax
	ret
EstWorkSpace	endp

;==============================================================================
;==
;==  EISACheck: This routine checks if the machine is EISA.
;==
;==============================================================================
	public	EISACheck
EISACheck proc	near
	push	es

	cmp	[ROMID],ROMIDISA	;Q: ISA/EISA machine?
	jne	short ECexit		; N: exit

	mov	eax,'ASIE'		; "EISA"
	les	di,cs:[pEISAROM]	; possible "EISA" in ROM

	cmp	eax,es:[di]		;Q: EISA machine?
	jne	short ECexit		; N: not on EISA box
	or	gs:[GenFlags],fEISA	; Y: set flag

ECexit:
	pop	es
	ret
	assume	fs:nothing
EISACheck	endp

;==============================================================================
;==
;==  MCACheck: This routine checks if the machine is MCA.
;==
;==  Input: GS = R_CODE
;==
;==  Output: bit in GenFlags set
;==
;==  Uses: AX, ES, BX
;==
;==============================================================================
	public	MCACheck
MCACheck 	proc   near

	mov     ah,0c0h		; Get System Description Vector
	stc
	int	15h
	jc	MCNoMCA  	; Error?  Not an MCA
;
;  Do we have a "Micro Channel" computer?
;
	mov     al,byte ptr es:[bx+5] ; Get "Feature Information Byte 1"
	test    al,00000010b    ; Test the "Micro Channel Implemented" bit
				; Q: is this an MCA machine
	jz	MCNoMCA		; N: clear ax and ret
	or	gs:[GenFlags],fMCA
				; Y: MCA set flag
MCNoMCA:
	ret

MCACheck	endp

;==============================================================================
;==
;==  VirHMAinit: This routine checks if a virtual HMA is being provided.
;==		 And if so, adds 64K to the int 15h handler if no XMM is
;==		 on the system.
;==
;==============================================================================
	public	VirHMAinit
VirHMAinit proc	near

	cmp	gs:[HMAptr],100000h		;Q: Virtual HMA?
	je	short VHexit			; N: don't process
	test	gs:[GenFlags],fXMM		;Q: XMM on the system?
	jnz	short VHexit			; Y: virtual HMA
	mov	gs:[ext_rem],64			; N: add 64K to INT 15h memory
VHexit:
	ret
VirHMAinit	endp

;==============================================================================
;==
;==  MoveExtBIOS: This routine moves the extended BIOS area (XBDA) to below
;==		  the lowest EMS window.
;==
;==============================================================================
assume cs:LAST
ArenaStruc	struc
  ArenaID	db	?	; Arena ID (i.e., 'Z', 'M')
  ArenaOwn	dw	?	; Arena owner (segment of)
  ArenaSize	dw	?	; Arena size (size in paragraphs)
ArenaStruc	ends

	public	MoveExtBIOS

MoveExtBIOS	proc	near
	push	ax
	push	bx
	push	cx
	push	si
	push	di
	push	ds
	push	es
	push	fs
	pushf				; no interrupts while transferring data
	cli

;
;  Check to see if Extended BIOS should be moved
;
	test	gs:[GenFlags],fXBDAnotRel	;Q: Move extended BIOS?
	jne	short MEBexit		 	; N: exit
	mov	ax,romdata		 	; Y: get pointer to BIOS data area
	mov	fs,ax
	assume 	fs:romdata

	int	12h			; get amount of INT 12 memory (1K)
	mov	cx,ax
	and	ax,0Fh			; check if extended BIOS machine
	cmp	al,0Fh			;Q: 1K less than a 16K multiple?
	jne	short MEBexit		; N: exit

	shl	cx,6			; convert to paragraphs
	mov	ds,cx			; current Extended BIOS area
	cmp	fs:[pExtBIOS],cx	;Q: Extended BIOS machine?
	jne	short MEBexit		; N: exit
;
;  Change Extended BIOS pointers and add 1K to Int 12h memory
;
	mov	ax,cs:[EndDriver]	; get current end of resident CEMM
	mov	fs:[pExtBIOS],ax	; place pointer in BIOS data area
	mov	es,ax			; new Extended BIOS area
	add	fs:[Int12hMem],1	; increment INT 12h by 1K
	add	ax,40h			; add 1K in paragraphs
	mov	cs:[EndDriver],ax	; add 1K to base memory usage
;
;  Move data to new location
;
	xor	si,si			; set up for 1K transfer
	xor	di,di
	mov	cx,100h			; move entire extended BIOS area
	rep movsd			; from old to new position
;
;  Add 1K to last ARENA header so DOS will use it!
;
	mov	bx,-1			; sanity checker
	mov	ax,5200h		; add 1K to arena header
	int	21h                     ; get SYSINIT variables
	cmp	bx,-1			;Q: Did the call work?
	je	short MEBexit		; N: don't add 1K to arena
	mov	bx,es:[bx][-2]		; Y: UNDOCUMENTED start of arena chain
	xor	si,si
MEBloop:
	mov	ds,bx			; access current arena
	add	bx,[si].ArenaSize	; get segment to next arena
	jc	short MEBexit		; if overflow, incorrect chain - EXIT!
	inc	bx
	cmp	[si].ArenaID,'Z'	;Q: Last arena header?
	jne	short MEBloop		; N: check next arena
	add	[si].ArenaSize,40h	; Y: add 1K to last arena (in paragraphs)

MEBexit:
	popf				; restore flags
	pop	fs
	pop	es
	pop	ds
	pop	di
	pop	si
	pop	cx
	pop	bx
	pop	ax
	ret
	assume es:nothing
MoveExtBIOS	endp

;----------------------------------------------------------------------------
;
; Procedure Name : IsZenith
;
;	Input  : gs -> R_CODE
;	Output : set's fzenith bit in genflags if zenith machine
;
;  Check to see if we're on a ZENITH machine
;
;----------------------------------------------------------------------------
	public	IsZenith
IsZenith 	proc near

	push	es
	push	ds
	push	di
	push	si
	push	cx

	les	di,cs:[pZENITH]		; es:di points to possible COMPAQ signature
	push	cs
	pop	ds
	mov	si,offset szZENITH 	; "ZDS"
	mov	cx,3
	cld
	rep	cmpsb			;Q: ZENITH machine?

	jnz	iz_notzen		; N:
	or	gs:[GenFlags], fzenith	; Y: set flag

iz_notzen:

	pop	cx
	pop	si
	pop	di
	pop	ds
	pop	es

	ret

IsZenith	endp

;-------------------------------------------------------------------------
;
;	M008:
;
;	Procedure Name	: IsCPQ16
;
;	Input		: gs -> R_CODE
;	Output		: set fCPQ16 bit in genflags if
;
;   	Determine if this system is a COMPAQ Deskpro 386/16 or
;       a COMPAQ Portable 386.  If so, the SHADOW ROM unmapping should not
;       be allowed because of a bug in these machines.
;
;--------------------------------------------------------------------------

CMOS_CPQ_DP38616        equ     31h     ; CMOS ID byte for COMPAQ DP386/16
CMOS_CPQ_P386           equ     33h     ; CMOS ID byte for COMPAQ P386

	public	IsCPQ16

IsCPQ16 proc near
        push    ax                      ; save register

        ; Read CMOS configuration to see if
        ;  the machine ID matches the ID for
        ;  DP 386/16 or P386
        ;-----------------------------------
        mov     al, 24h                 ; point to CMOS index for ID byte
        out     70h, al                 ; byte to get
        jmp     $+2                     ; delay I/O for 286
        jmp     $+2                     ;  ...
        in      al, 71h                 ; get byte from CMOS
        cmp     al, CMOS_CPQ_DP38616    ; Is this a DP386/16 ?
        je      C386_YES                ; Y: return
        cmp     al, CMOS_CPQ_P386       ; Is this a P386 ?
        jne     C386_NO                 ; N:

C386_YES:
	or	gs:[GenFlags], fCPQ16	; Y: set flag

C386_NO:
        pop     ax                      ; restore registers
        ret                             ;   and return

IsCPQ16 endp

;-------------------------------------------------------------------------
;
;	Install_I13handler:
;
;	If smartdrv present install emm386's int13 hook in front of smartdrv
;
;	HKN 11/02/89
;
;--------------------------------------------------------------------------
	public	Install_I13handler

Install_I13handler	proc	near

	push	cx
	push	dx
	push	bx
	push	ax
	push	ds
	push	gs
	push	fs
	push	di

	push	ds
	pop	gs

	mov	dx, word ptr gs:[EMM_rFarEntry+2]
	mov	fs, dx		; fs = R1_CODE seg
	assume	fs:R1_CODE

; Open smartdrv
	push	cs
	pop	ds
	mov	dx, offset cs:smartdrv_name
	call	IOCTLOpen
	cmp	bx,-1
	je	no_smartdrv


; Read Smartdrv's OLD int 13 handler
				; handle in BX
	mov	cx, SIZE smartdrv_status
	mov	dx, offset cs:smartdrv_status
	call	IOCTLRead
	cmp	ax, -1
	je	no_smartdrv

; EMM386's OLD int 13 handler should be SMartdrv's OLD
	mov	di, dx
	mov	eax, ds:[di.old_int13]
	mov	gs:[PrevInt13],eax
	mov	fs:[OldInt13], eax

; Smartdrv's OLD int 13 handler should be EMM386's int 13 handler
	mov	di, offset set_int13
	mov	ax, seg R_CODE
	shl	eax, 16
	mov	ax, offset R_CODE:rINT13hHandler
	mov	dword ptr ds:[di+2], eax
	mov	dx, di
	mov	cx, 5
	call	IOCTLWrite
	cmp	ax, -1
	jne	Install_I13_done

no_smartdrv:
	call	IOCTLClose	; ignore error from close
;
;  Check for CACHE.EXE on the system
;
	lea	dx,[CACHEname]
	call	IOCTLOpen
	cmp	bx,-1
	je	short II13noCACHE

;
; Give CACHE CEMM's INT 13h handler & Get CACHE's OLD INT 13h handler: handle in BX
;
	mov	ax,seg R_CODE
	shl	eax,16
	lea	ax,R_CODE:rINT13hHandler
	mov	dword ptr [CACHEIOCTL][2],eax

	mov	cx,10
	lea	dx,[CACHEIOCTL]
	call	IOCTLRead
	cmp	ax,-1
	je	short II13noCACHE

;
; CEMM's OLD int 13 handler should be CACHE's OLD
;
	lea	ax,R_CODE:rINT13hHandler
	cmp	ax,[CACHEIOCTL][2]
	jne	short II13noCACHE

	cmp	dword ptr [CACHEIOCTL][6],0
	je	short II13noCACHE

	mov	eax,dword ptr [CACHEIOCTL][6]
	mov	gs:[PrevInt13],eax
	mov	fs:[OldInt13], eax

	jmp	short Install_I13_done

II13noCACHE:
	mov	ax, seg R_CODE
	shl	eax, 16
	mov	ax, offset R_CODE:rINT13hHandler
	xchg	es:[int13],eax
	mov	gs:[PrevInt13],eax
	mov	fs:[OldInt13], eax


Install_I13_done:
;				; handle in BX
;	mov	cx, SIZE smartdrv_status
;	mov	dx, offset cs:smartdrv_status
;	call	IOCTLRead
;
	call	IOCTLClose	; ignore error from close
	pop	di
	pop	fs
	pop	gs
	pop	ds
	pop	ax
	pop	bx
	pop	dx
	pop	cx

	assume	fs:nothing

	ret

Install_I13handler	endp

;--------------------------------------------------------------------------
;
;	IOCTLOpen - Open the indicated device and make sure it's a device
;
;  ENTRY:
;	DS:DX Pointer to name of device
;  EXIT:
;	BX = -1 if error, device not opened
;	else BX = handle of open device
;
;----------------------------------------------------------------------------
	public	IOCTLOpen

IOCTLOpen	proc	near

	MOV	AX,3D02H
	INT	21H		; Open the device
	JC	NO_DEV_ERR	; No device
	MOV	BX,AX
	MOV	AX,4400H
	INT	21H		; Make sure it IS a device
	JC	CLOSE_NO_DEV
	TEST	DX,4080H
	jnz	short PXDONE

CLOSE_NO_DEV:
	mov	ax,3e00H	; Close
	int	21H
NO_DEV_ERR:
	mov	bx,-1
PXDONE:
	ret

IOCTLOpen	endp

;--------------------------------------------------------------------------
;	IOCTLClose - Close the indicated handle
;
;  ENTRY:
;	Handle
;  EXIT:
;	None
;
;---------------------------------------------------------------------------
	public	IOCTLClose

IOCTLClose	proc	near

	cmp	bx,-1		;Q: Is there a handle?
	je	short @f	; N: exit
	MOV	AX,3E00H
	INT	21H		; close the device
@@:
	ret
IOCTLClose	endp

;--------------------------------------------------------------------------
;	IOCTLWrite - Perform IOCTLWrite to device handle
;
;  ENTRY:
;	BX = Handle to open device
;	DS:DX -> Pointer to data to write
;	CX = Count in bytes of data to write
;  EXIT:
;	AX = -1 error
;	else AX = input count
;
;--------------------------------------------------------------------------
	public	IOCTLWrite

IOCTLWrite	proc	near

	MOV	AX,4403H	; IOCTL Write
	INT	21H
	JC	Werr
	CMP	AX,CX
	jz	short WDONE

WERR:
	mov	ax,-1
WDONE:
	ret

IOCTLWrite 	endp


;----------------------------------------------------------------------------
;	IOCTLRead - Perform IOCTLRead to device handle
;
;  ENTRY:
;	BX = Handle to open device
;	DS:DX -> Pointer to data area to read into
;	CX = Count in bytes of size of data area
;  EXIT:
;	AX = -1 error
;	else AX = input count
;-----------------------------------------------------------------------------
	public	IOCTLRead

IOCTLRead	proc	near

	MOV	AX,4402H	; IOCTL Read
	INT	21H
	JC	Rerr
	CMP	AX,CX
	jz	short RDONE

RERR:
	mov	ax,-1
RDONE:
	ret

IOCTLRead	endp

;******************************************************************************
;	ErrorExit - clean up after init error encountered
;
;	When an initialization error occurs which prevents CEMM from loading,
;	this routine is responsible for "cleaning" up before CEMM's driver
;	init routine returns to DOS.  This involves
;	    (1) if EGAmove has been initialized, then
;		    unpatch int10h vector.
;	    (2) if Weitek installed, upatch int11h vector.
;
;	ENTER: REAL mode
;
;	EXIT: REAL mode
;
;	USED: high word of EAX
;
;******************************************************************************
	public	ErrorExit

ErrorExit	proc	near
	push	ax
	push	es
	push	ds
	push	fs

	mov	ax,R1_CODE
	mov	fs,ax
	ASSUME	fs:R1_CODE

	mov	ax,R_CODE
	mov	ds,ax
	ASSUME	ds:R_CODE


	; Fool the system that we are a block driver, so it will remove the
	; whole program, not leaving a trace.
	AND	[DDT], NOT CHAR_DEV


    	; Remove ourselves from the XMM control chain if XmmControlBase is
	; not -1.
	;
    	; We assume that no one has hooked us, since we're still in driver
    	; initialization.
    	;

	cmp	word ptr cs:[XmmControlBase],-1
	jne	UnhookXmm
	cmp	word ptr cs:[XmmControlBase+2],-1
	je	EE_chki10		; we didn't hook- don't unhook
UnhookXmm:
	mov	al,0EBh			; AL = opcode for short jump
	mov	ah,cs:[XmmControlJmpVal]; AH = displacement for short jump
	les	bx,cs:[XmmControlBase]	; ES:BX = ptr to previous XMM header
	mov	word ptr es:[bx],ax	; restore previous XMM's short jump
	mov	word ptr es:[bx+2],9090h;  followed by nop's
	mov	byte ptr es:[bx+4],90h


EE_chki10:

	xor	ax,ax
	mov	es,ax				; ES-> 0
	ASSUME	ES:ABS0


	;
	; check for Int10h patched
	;
	test	[Current_State],fState_CEGAmove ;Q: EGA ROM moved ?
	jz	short EE_chki11			;  N: chk for int11 patch
	mov	eax,[Int10_Save]	; EAX = initial int 10h vector
	mov	dword ptr es:[int10],eax		; set it back

	;
	; check for Int11h patched
	;
EE_chki11:
	test	[Weitek_State],fWeitek_Inst	;Q: Weitek installed ?
	jz	short EE_exit 			;  N: leave now
						;  Y: unpatch int11h vector
	mov	eax,fs:[PrevInt11] 		; EAX = initial int 11h vector
	or	eax,eax				; Q: eax=0 ?
	jz	short EE_exit 			;   Y: don't unpatch
	mov	dword ptr es:[int11],eax		; set it back

EE_exit:
	pop	fs
	pop	ds
	pop	es
	pop	ax
	ret
	assume	ds:_DATA
	assume	es:_DATA
	assume	fs:nothing

ErrorExit	endp

;;--------------------------------------------------------------------------
;;  $chkCPQxmm:  This routine checks for a COMPAQ HIMEM.SYS.  If an EISA machine
;;		 is detected, any XMM other than COMPAQ will be activated.
;;
;;  (The routine preserves all registers)
;;--------------------------------------------------------------------------
$ROMSEG		equ	0F000h
$EISAOFF	equ	0FFD9h
$ROMEISA label	dword
		dw	$EISAOFF
		dw	$ROMSEG
$CPQOFFS	equ	1Ch
$CPQEPOFFS	equ	104h
$XMMcontrol	dd	0

	public	$chkCPQxmm

$chkCPQxmm proc	near
    push      es                            ;;
    push      bx                            ;;
    push      ax                            ;;
;
;  Check if this is an EISA machine
;
    les	      bx,[$ROMEISA]		    ;; address for "EISA" string
    cmp	      es:[bx],'IE'		    ;;Q: "EI" part of EISA?
    jne	      $no_XMM			    ;; N: not an EISA machine
    cmp	      es:[bx+2],'AS'		    ;;Q: "SA" part of EISA?
    jne	      $no_XMM			    ;; N: not an EISA machine
;
;  Check if XMM is on system
;
    mov       ax,4300h                      ;; function for XMS detect
    int       2fh                           ;; check for XMS driver
    cmp       al,80h                        ;; Q: Is XMS installed?
    jne       $no_XMM                       ;;  N: XMS not in use, not enough
;
;  Get entry point of XMM
;
    mov       ax,4310h                      ;; Get XMS routine address
    int       2fh                           ;;
    mov       word ptr cs:[$XMMcontrol],bx  ;; Save XMS routine address
    mov       word ptr cs:[$XMMcontrol+2],es;; (offset), (segment)
;
;  Is it a COMPAQ XMM?
;
    cmp	      word ptr es:[$CPQOFFS],'oC'   ;;Q: "Co" of COMPAQ?
    jne	      $chk2_50			    ;; N: check CPQ ver 2.50
    cmp	      word ptr es:[$CPQOFFS+2],'pm' ;;Q: "Comp" of COMPAQ?
    jne	      $chk2_50			    ;; N: check CPQ ver 2.50
    cmp	      word ptr es:[$CPQOFFS+4],'qa' ;;Q: "Compaq" of COMPAQ?
    je	      $no_XMM			    ;; Y: don't activate CPQ XMM
;
;  Check if it is a COMPAQ HIMEM version 2.50
;
$chk2_50:
    mov       ah,0                          ;; Get XMS version number
    call      cs:[$XMMcontrol]              ;; Call XMM
    cmp       ax,200h                       ;;Q: Version = 2.00?
    jne       $ActivateXMM                  ;; N: foreign XMM, activate it!
    cmp       bx,250h                       ;;Q: Revision 2.50?
    jne       $ActivateXMM                  ;; N: foreign XMM, activate it!

    cmp       word ptr cs:[$XMMcontrol],$CPQEPOFFS;;Q: CPQ ver 2.50?
    je	      short $no_XMM		 	  ;; Y: don't activate XMM
;
;  Not a COMPAQ XMM driver, activate it!
;
$ActivateXMM:
    mov       ah,8                          ;; How much extended memory is
    call      cs:[$XMMcontrol]              ;; ..... available in Kbytes?
$no_XMM:                                     ;;
    pop       ax                            ;;
    pop       bx                            ;; restore bx
    pop       es                            ;; restore es
    ret                                     ;;
$chkCPQxmm    endp 	                    ;;

;*****************************************************************************
;
; Synopsis    : Check_Install_Abort()
;
; Description : Check if the "installation abort hotkey" is being pressed
;                   Hotkey for abort is: L-SHIFT + R-SHIFT + "-" (minus key)
;
; Returns     : CY = 0 if program should continue to install
;                  = 1 if program should abort installation
;
; Alters      : None
;
; Calls       : BIOS
;
; History     : Kelan Silvester 02/27/91
;                       original entry
;
;*****************************************************************************/
	public	Check_Install_Abort

Check_Install_Abort 	proc	near

        mov     ah,1                ; check keyboard request
        int     16H                 ; call BIOS
        jz      install_OK          ; if keyboard buffer empty -->

        cmp     ah, 0Ch             ; Is the hotkey (minus "-") being pressed ?
        jne     install_OK          ;  N:
        mov     ah, 2               ; get shift status
        int     16H                 ; call BIOS

	and	al,011b		    ; concerned with shift status only
        cmp     al,011b		    ; Are both shift keys pressed ?
        jne     install_OK          ;  N:
        stc			    ;  Y: set return flag
        ret

install_OK:
        clc			    ; return
        ret

Check_Install_Abort endp

;*************************************************************************** 
;     Chk_DOS - check DOS version to be >= 3.1                               
;		- must be in this seg since the rest is still packed	     
;
;	EXIT: carry set if DOS < 3.1, otherwise carry clear		     
;									     
;	USED: ax							     
;									     
;*************************************************************************** 
	public	Chk_DOS

Chk_DOS proc near				;			     

	mov	ah,GET_VERSION		;get DOS version #	     
	int	MS_DOS			;			     
	mov	cs:[DOS_version],ax
	cmp	al,10			; Q: OS/s DOS?
	jae	short C_bad_DOS		;  Y
	cmp	al,3			;Q: >= DOS 3.10 ?	     
	jb	short C_bad_DOS		;  N: won't work             
	ja	short Chk_ok		;  higher is ok
	cmp	ah,10			;Q: DOS >= 3.10 ?	     
	jb	short C_bad_DOS		;  N: won't work            
Chk_ok:
	clc				;clear carry for no error   
	ret				;			     
C_bad_DOS:				;handle invalid DOS version 
	stc
	ret
Chk_DOS endp				;	       


;===========================================================================
;
;
; 	Procedure Name 	: MovUmbSeg
;
;	Entry		: GS = R_CODE
;			: V86 MODE
;
;	Exit		:
;
;	Uses		: ax, bx, cx, dx, es, ds, di, si
;
;===========================================================================

	public	MovUmbSeg

MovUmbSeg	proc	near

	cmp	cs:[NoHigh], TRUE	; Q: NOHI specified
	je	short MUSquit		; Y: do not mov r1_code into UMB
	cmp	cs:[UMBset], TRUE	; Q: RAM option specified
	je	short MUScont		; Y: UMBs available
	cmp	gs:[VCPIset], -1	; Q: NOEMS option specfied
	jne	short MUScont		; Y: UMBs available

MUSquit:
	ret

MUScont:
	mov	dx, OFFSET R1_CODE:end_of_R1_CODE
	mov	cx, dx			; cx = length of R1_CODE in bytes
	add	dx, 0fh			; round up to para boundary
	shr	dx, 4
	mov	ah, 10h			; ah = request UMB
	call	rXMMentry		; Make our own XMS call
	or	ax, ax			; Q: was the request successful
	jz	MUSquit			; N: quit
					; Y: dx is the size of block in paras

	sub	cs:[EndDriver], dx	; Update EndDriver size
	mov	es, bx
	assume	es:R1_CODE
	xor	di, di			; es:di is the UMB seg
	mov	ax, seg R1_CODE
	mov	ds, ax
	mov	si, di			; ds:si is start of R1_CODE
					; cx already set up
rep	movsb				; move it

	;
	; Update INT 2Fh, 4Bh, int 10 and int 11 hooks.
	;

	xor	ax,ax
	mov	fs,ax
	ASSUME	fs:ABS0

	mov	word ptr fs:[int2f+2], bx
	mov	word ptr fs:[int4B+2],bx

	cmp	word ptr fs:[int10+2], seg R1_CODE
	jne	short MUSchki11
	mov	word ptr fs:[int10+2],bx
MUSchki11:
	cmp	word ptr fs:[int11+2], seg R1_CODE
	jne	short MUSfixup
	mov	word ptr fs:[int11+2],bx
MUSfixup:	

	;
	; We now need to patch in the segment address in the UMBFARTABLE
	; in umbseg.asm
	;

	xor	eax, eax
	mov	ax, gs
	mov	es, ax 			; es = R_CODE
	mov	cx, UMBADDRLEN		; cx = number of segments to patch
	mov	ax, bx			; ax = the UMB seg where R1_CODE is.
	mov	di, OFFSET R_CODE:UMBFARTABLE
	add	di, 2
MUSpatch:
	stosw
	add	di, 2
	loop	MUSpatch

	; Update word containing segment address of R1_CODE

	assume	es:R_CODE
	mov	es:[segR1_CODE], ax
	assume	es:NOTHING

	;
	; We now need to update our XMS entry point segment in the XMS 
	; chain. Note that we have saved the address at which we patched
	; rXMMentry's seg address in XMMHookAddr (xms.asm)
	;
	les	di, cs:[XMMHookAddr]
	stosw
	; 
	; We now need to set up the RDSdata in Win386VxdRefDat. Since this 
	; is in the _DATA segment we have to swicth to protct mode. This has 
	; to be initialized to InstanceData. The latter is defined in the 
	; R1_CODE seg that has just been moved to UMB. The segment of this 
	; UMB is passed in ax to the int protTrap handler defined in 
	; pictrap.asm. Also this call will update the GDT entries for R1_CODE
	; to the new base address.
	;

ifdef DEBUG
	push	ax			; save UMB segment on stack
endif

	or	gs:[TrapFlags],fSetInstPtr
					; dispatch to set inst ptr
	int	ProtTrap		; enter protected mode


	; The R1_CODE segment has been moved to the UMB.  Tell the debugger
	; where it now resides.

ifdef DEBUG
	mov	al, 40h 		; code segment & selector
	mov	bx, seg R1_CODE 	; bx = segment to define
	pop	cx			; cx = UMB segment addr
	mov	dx, R1CODE_GSEL 	; dx = selector
	call	DebDefineSeg
endif

	ret

MovUmbSeg	endp	


;******************************************************************************
;
; Code from Toshiba to set upper memory exclusion areas on some Toshiba
; systems.

RESUME_SEG	equ	0F000H					
RESUME_ID_OFF	equ	0E024H					

BACKUP_RAM_SIZE	equ	0E02AH
BACKUP_RAM_STRT	equ	0E02BH
HARD_RAM_SIZE	equ	0E02DH
HARD_RAM_STRT	equ	0E02EH

RESUME_ID_1st	equ	'R'					

HARD_RAM_TYPE	equ	70H					

SetToshibaOptions proc near

	call	IsToshiba		; Is this a Toshiba machine?
	jnz	sto_ret 		;   no, exit this routine

;	Check which type of machine we are running EMM386 on.
;	This is important, since certain segments are used by resume
;	& hardRAM machines, and therefore cannot be used
;	as EMM pages.

	push	es						
	mov	ax,RESUME_SEG					
	mov	es,ax		     
				
	push	bx

;	Now second byte of Resume ID may contain flag bits, so check for
;	each byte separate (Rx).  "R" indicates resume machine, second byte,
;	if not 0 contains NMI, HardRAM, Backup RAM mapping info.

	cmp	byte ptr es:[RESUME_ID_OFF], RESUME_ID_1st ; check for 'R'		

	jne	IE_resume_e				   

;	Pick up Backup RAM mapping info from Resume ID area (if new
;	machine type -- T3300SL); otherwise set exclude areas to defaults.

	bt	word ptr es:[RESUME_ID_OFF + 1],2 ; test if Bkup RAM @ dflt loc: e800-efff
	jc	short IE_BRAM_Mapped		; brif Bckup RAM mapping defined (NOT e000-e7ff)
	mov	word ptr cs:[BRAMbeg],0E800h	; Backup RAM is @ default loc: E800-EFFF
	mov	word ptr cs:[BRAMend],0Efffh
	jmp	short IE_resume_e
IE_BRAM_Mapped:
	mov	bx,word ptr es:[BACKUP_RAM_STRT]	; get Bkup RAM start seg
	mov	ax,word ptr es:[BACKUP_RAM_SIZE]	; get Bkup RAM size in KB
	shl	ax,6				; Bkup RAM size in bytes
	add	ax,bx				; size + beg = Bkup RAM end seg
	dec	ax				; back off to xfff
	mov	cs:[BRAMbeg],bx			; save beg to exclude Bkup RAM
	mov	cs:[BRAMend],ax			; save end to exclude Bkup RAM

IE_resume_e:							

	push	cx						
	push	dx						

	mov	dl,80H						
	mov	ah,21						
	int	13H						
	jc	IE_hard_ram_1					
	cmp	ah,HARD_RAM_TYPE				
	je	IE_hard_ram_2					
IE_hard_ram_1:							
	mov	dl,81H						
	mov	ah,21						
	int	13H						
	jc	IE_hard_ram_e					
	cmp	ah,HARD_RAM_TYPE				
	jne	IE_hard_ram_e					
IE_hard_ram_2:							

;	Pick up HardRAM mapping info from Resume ID area (if new
;	machine type -- T3300SL); otherwise set exclude areas to defaults.

	bt	word ptr es:[RESUME_ID_OFF+1],3 ; test if Hard RAM @ dflt loc: e000-e7ff
	jc	short IE_HRAM_Mapped		; brif Hard RAM mapping defined (NOT e000-e7ff)
	mov	word ptr cs:[HRAMbeg],0E000h	; Hard RAM is @ default loc: E000-E7FF
	mov	word ptr cs:[HRAMend],0E7ffh
	jmp	short IE_hard_ram_e
IE_HRAM_Mapped:
	mov	bx,word ptr es:[HARD_RAM_STRT]	; get Hard RAM start seg
	mov	ax,word ptr es:[HARD_RAM_SIZE]	; get Hard RAM size in KB
	shl	ax,6				; Hard RAM size in bytes
	add	ax,bx				; size + beg = Hard RAM end seg
	dec	ax				; back off to xfff
	mov	cs:[HRAMbeg],bx			; save beg to exclude Hard RAM
	mov	cs:[HRAMend],ax			; save end to exclude Hard RAM

IE_hard_ram_e:							
	pop	dx						
	pop	cx						
	pop	bx

	pop	es

sto_ret:
	ret

SetToshibaOptions endp

;******************************************************************************
;
; IsToshiba - check to see if we're on a Toshiba 386 machine
;
;	in:   none
;	out:  toshiba_machine = 1 and Z flag set if Toshiba
;	used: flags
;

IsToshiba proc	near

	push	es
	push	ds
	push	di
	push	si
	push	cx

	les	di, pToshiba		;es:di -> possible Toshiba signature
	push	cs
	pop	ds
	mov	si, offset szToshiba	; "TOSHIBA "
	mov	cx, 8
	cld
	rep cmpsb

	pop	cx
	pop	si
	pop	di
	pop	ds
	pop	es

	jnz	it_ret
	mov	toshiba_machine, 1

it_ret:
	ret

IsToshiba endp

LAST	ends				; End of segment
	end				; End of module
