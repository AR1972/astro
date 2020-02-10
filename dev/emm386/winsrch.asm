.386p
	page 58,132
;=============================================================================
	title	W I N S R C H - searches for EMS and UMB windows
;=============================================================================
;==
;== (C) Copyright MICROSOFT Corp. 1990-1991
;== (C) Copyright COMPAQ Computer Corp. 1990-1991
;==
;==	Title:	EMM386.EXE - MICROSOFT Expanded Memory Manager 386 Driver
;==
;==	Module: WinSrch  - searches for EMS and UMB windows
;==
;==	Version: 1.00
;==
;==	Date:	July 9,1990
;==
;==	Author: Leo Cohen
;==	(some routines extracted from ROM_SRCH.ASM: Daniel J. Mazina,
;==						    Richard D. Vail,
;==	  					    & Patrick Wu)
;==
;=============================================================================
;==
;==	Change Log:
;==
;==	DATE	 REVISION	Description
;==	-------- --------	--------------------------------------------
;==     07/09/90 0.00	        Original (* routines from ROM_SRCH.ASM)
;==
;==	01/18/91 M002		use xchg instead of move to write into 
;==				adapter space for ram determination.
;==
;==	01/30/91 M004		When trying to reclaim shadow ROM in routine
;==				CheckForCompaqROM, check to make sure that
;==				the offset of the int 10 vector lies within
;==				ROM length as specifed in the ROM header.
;==
;=============================================================================
;==
;==   Functional Description:
;==
;==   This module determines the areas which will be used by CEMM.  These
;==   areas include: page frame, other EMS windows above base memory, UMB
;==   windows, and EMS base memory windows.  It analyzes the user parameters
;==   and detects ROM and RAM areas to best determine these EMS/UMB windows.
;==
;==
;=============================================================================
;==	P U B L I C   D E C L A R A T I O N S
;=============================================================================
	public	Page4K
	public	UMBtable
	public	NumOfUMBwindows
	public	FindWindowLocations
	public	ROMcheck
	public	RAMcheck
ifdef	ROMCOMP
	public	ROMstart
endif
	public	CPQvideoROM
	public	DefaultROM
	public	DefaultInclude
	public	ProcessRange
	public	ExcludeRange
	public	RangeOverlap
	public	SetFlag
	public	GetPageFrame
	public	SetPageFrame
	public	EMSwindows
	public	UMBwindows
	public	BaseEMSwindows
	public	CheckEMSwindow
	public	CheckPageFrame
	public	CheckForROMHeader
	public	CheckForCompaqROM
	public	UpdatePageArray
	public	PFB_warning
	public	IsCompaq
;=============================================================================
;==	E X T E R N A L  D E C L A R A T I O N S
;=============================================================================
LAST	segment

ifdef	ROMCOMP
	extrn	UnProtectROM:near
	extrn	ProtectROM:near
	extrn	FixROMptrs:near
endif

	extrn	Pn:byte
	extrn	PnSet:word
	extrn	eXcSet:word
	extrn	IncSet:word
	extrn	DfltSet:word
	extrn	RAMSet:word
	extrn	ROMSet:word
	extrn	EMSSet:word
	extrn	WINset:word
	extrn	HighSet:word
	extrn	VGAROM:word
	extrn	E000ROM:word
	extrn	ROMparm:byte
	extrn	pCOMPAQ:dword
	extrn	szCOMPAQ:byte
	extrn	UMBset:byte
	extrn	PS2DATA:byte
	extrn	Highscan:byte
	extrn	NoTR:byte
	extrn	RangeSets:byte
	extrn	RangeSave:byte
	extrn	RANGESIZE:abs

	extrn	HRAMSet:word					
	extrn	BRAMSet:word					
	extrn	ToshSet:word					
	extrn	TOSHbeg:word					
	extrn	TOSHend:word
	extrn	toshiba_machine:byte

LAST	ends

_DATA	segment
	extrn	ROMID:byte
_DATA	ends
;=============================================================================
;==	L O C A L   C O N S T A N T S
;=============================================================================
	include vdmseg.inc
	include emm386.inc
	include romstruc.equ
	include emmfunct.inc
	include emmdata.inc
	include ps2ex.inc
	include ps2equ.inc
	include	eisaex.inc

;=============================================================================
;==  For ROM, RAM, and CPQ Video ROM search
;=============================================================================
FIRST_ROM_SEGMENT	= 01000H	; Segment address of first option ROM.
LAST_ROM_SEGMENT	= 0DF80H	; Segment address of last option ROM.*A
FIRST_RAM_SEGMENT	= 01000H	; Seg address of 1st possible RAM addr
;910520 LAST_RAM_SEGMENT= 0DF80H	; Seg addr of last possible RAM addr
LAST_RAM_SEGMENT	= 0EF80H	; Seg addr of last possible RAM addr 910520
REMAP_VIDEO_ROM_SEGMENT	= 0E000H	; Seg address of optional sys rom
ROM_SIGNATURE		= 0AA55h
NEXT_ROM_SEG		= 080H		; 2K seg increment to next ROM location
ROUND_BY_2K_SEG		= 0FF80h	; rounding factor for 2k seg addresses
SIZE_OF_16K_PAGE	= 0400h		; length of a EMS page in paragraphs
;=============================================================================
;==  For XMA2EMS mode
;=============================================================================
P0	EQU	1
P1	EQU	1 SHL 1
P2	EQU	1 SHL 2
P3	EQU	1 SHL 3
P254	EQU	1 SHL 4
P255	EQU	1 SHL 5
;=============================================================================
;==	C O D E  S E G M E N T
;=============================================================================
LAST	segment
	assume	cs:LAST,ds:_DATA,gs:R_CODE
;=============================================================================
;==	L O C A	L   D A T A
;=============================================================================

; Data items between ZeroInitData and ZeroInitSize are set to zero (suprise)
; when FindWindowsLocations is called since it may be called more than once.

ZeroInitData		label	byte
PFB_warning		dw	0
Page4K			db	256 dup (0)
NumOfUMBwindows		db	0
UMBtable		db	128 dup (0)
ZEROINITSIZE		equ	$-ZeroInitData

LastChanceBuf		db	4*1024 dup(0)

SetTable label	word
	dw	offset LAST:DfltSet
	dw	offset LAST:HRAMSet
	dw	offset LAST:BRAMSet
	dw	offset LAST:ToshSet
	dw	offset LAST:eXcSet
	dw	offset LAST:RAMSet
	dw	offset LAST:ROMSet
	dw	offset LAST:EMSSet
	dw	offset LAST:WINset
NumberOfSets	equ	($-SetTable)/2

Pass		db	-1

EISAdata	db	(size GSI_data) dup (?)

ifdef	ROMCOMP

ROM64k		db	'DEP'
Num64kROMs	equ	$-ROM64k
ROM40k		db	'BHL'
Num40kROMs	equ	$-ROM40k
ROM32k          db	'FG'
Num32kROMs	equ	$-ROM32k

ROMstart	dw	0

endif

ROMChangeCount	equ	20

;===============================================================================
;==
;== FindWindowLocations: This routine creates an array of 256 entries.  Each
;==			 entry represents a 4K page in the first megabyte.
;==			 After searching the option ROM area, it marks any
;==			 entry which included ROM or RAM appropriately.  Then
;==			 it takes the user defined ranges from the command line
;==			 and merges this information into this array.  This
;==			 array is then analyzed to create the UMB and EMS
;==			 windows necessary for operation.
;==
;== Entry
;==	cs:PF_windows[] = table of possible page frame addresses.
;==	cs:DefltSet[]	= array of default unused areas.
;==	cs:eXcSet[]	= array of user specified areas to exclude.
;==	cs:IncSet[]	= array of user specified areas to include.
;==	cs:RAMSet[]	= array of user specified areas to include RAM.
;==	cs:EMSet[]	= array of user specified areas to include EMS.
;==	CS		= LAST segment
;==	DS 		= _DATA segment
;==	GS 		= R_CODE segment
;==
;== Exit
;==	ds:EMS_window[]			= initialized
;==	ds:EMS_window_locations[]	= initialized
;==	ds:EMSsegLoc[]			= initialized
;==	ds:[number_EMS_windows]		= initialized
;==	ds:UMBtable[]			= initialized
;==	ds:[XMA2EMS]			= initialized
;==	gs:[PF_Base]			= initialized
;==
;===============================================================================
FindWindowLocations	proc	near

	pushad
	cld

;  This routine may be called more than once, so after the first pass
;  some data must be reinitialized.

	push	cs
	pop	es

	inc	Pass				; Q: 1st time?
	jnz	short FWLagain			; N:

	mov	cx, RANGESIZE			; 1st time, save inital state
	mov	si, offset LAST:RangeSets	;   of the ranges so they can
	mov	di, offset LAST:RangeSave	;   be reused next time.
	rep movs byte ptr es:[di], es:[si]
	jmp	short FWLcont

FWLagain:
	xor	ax, ax				; 2nd time, zero initialize
	mov	cx, ZEROINITSIZE		;   some data
	mov	di, offset LAST:ZeroInitData
	rep stosb

	mov	cx, RANGESIZE			; and restore the saved ranges
	mov	si, offset LAST:RangeSave
	mov	di, offset LAST:RangeSets
	rep movs byte ptr es:[di], es:[si]

FWLcont:

;
;  Get first 16K multiple PTE entry after base memory. Used for base EMS windows.
;
	int	12h
	shr	ax,2				; 4K page
	add	ax,3				; round up
	and	ax,0FFFCh			; multiple of 4K
	mov	[end_of_base_memory_PTE],ax
;
;  Determine default memory areas not to use for EMS/UMB windows.
;
	call	DefaultROM

;  Check for Token Ring card

	call	CheckToken

;  Check Toshiba special exclude areas

	call	Toshiba_exclude

ifdef E000
;  Determine if can 'deshadow' video ROM at E000h

	test	gs:[GenFlags], fMCA	; Q: MCA
	jnz	short FWLskipE000	; Y: E000 is other ROM

	call	CheckE000

FWLskipE000:
endif
;
;  Check if Include ranges are for RAM or EMS: if no RAM switches, then EMS
;
	call	DefaultInclude
;
; If this is an MCA machine we'll do the detection of adapters using the 
; POS bytes. 
;

ifdef	ROMIDMCA
	xor	ax, ax			; Assume that POS did not work 
	cmp	[ROMID],ROMIDPS2	; Q: Is it a PS2
	jne	short FWLromscan	; N: do rom scan
endif
	test	gs:[GenFlags], fMCA	; Q: MCA
	jz	short FWLchkEISA	; N: check if EISA
					; Y: do POS detection
	call	ExcludePS2Options
	jmp	short FWLromscan

FWLchkEISA:
	test	gs:[GenFlags], fEISA	; Q: EISA
	jz	FWLromscan		; N: do rom scan
	call	ExcludeEISAOptions
;
;  Detect Option ROMs
;
FWLromscan:
	call	ROMcheck
;
;  Detect Compaq VIDEO ROM
;
	call	CPQVideoROM
;
;  Check if ROM can be mapped over by RAM
;
ifdef	ROMCOMP
	call	CPQROMcheck
endif
	call	ROMcompress

;ifndef IBMCOPYRIGHT

	; IBM version doesn't do this because their QBASIC uses ROM Basic

	call	ROMbasic
;endif
;
;  Process Default, Exclude, Include, RAM, and EMS range arrays (sets)
;
	xor	bx,bx
	mov	cx,NumberOfSets		; number of range arrays

FWLsetLoop:
	mov	si,cs:[SetTable][bx]	; get array address
	call	ProcessRange
	add	bx,2
	loop	FWLsetLoop
;
;  Detect Option RAM
;
	call	RAMcheck
;
;  Mark HighScan regions as available
;
	lea	si, cs:[HighSet]
	call	ProcessRange
;
; We now try to scan again using Ralph's algo
;
	push	edx	
	mov	edx, 0c0h		; start c0h
FWLscanagn:
	call	LastChanceScan
	inc	edx
	cmp	edx, 0f0h		; until efh
	jb	short FWLscanagn
	pop	edx
;
;  Make sure there are no overlaps in the ranges: Exclude takes precedence.
;
	call	RangeOverlap

;  On a Toshiba system, don't allow the user to put the EMS page frame over
;  reserved upper memory.

	call	ToshInvPFbase					
;
;  Now the EMS/UMB windows are selected: First get a page frame address
;
	call	GetPageFrame
;
;  Set up the page frame
;
	call	SetPageFrame
;
;  Set up EMS windows in the 640K to 1MB region
;
	call	EMSwindows
;
;  Set up UMB windows in the 640K to 1MB region
;
	call	UMBwindows
;
;  Set up base EMS windows
;
	call	BaseEMSwindows

	popad
	ret
FindWindowLocations endp

;===============================================================================
;==
;==  ROMcheck: Detect Option ROMs.
;==	       This section of code searches the auxiliary rom area
;==	       (from C0000 up to E0000) in 2K increments. A ROM checksum is
;==	       calculated to insure that the ROMs are valid.  Valid ROMs must
;==	       have the 1st byte = 55H and the next byte = 0AAH.  The next
;==	       byte indicates the size of the ROM in 512-byte blocks.  The
;==	       sum of all bytes in the ROM, modulo 256,	must be zero.
;==
;==	       If a ROM is not found at a location, the next location 2K-bytes
;==	       down is examined.  However, if it is found, the next location
;==	       after this ROM is tried.  The next ROM location is determine
;==	       according to the	size of the previous ROM.
;==
;==  Enter:
;==	Page4K[BX] = uninitialzed with detected ROM locations.
;==
;==  Exit:
;==	Page4K[BX] = initialzed with detected ROM locations.
;==
;===============================================================================
ROMcheck proc	near
	push	ax

;
; Memory above 640k is searched for any VIDEO or option ROMs located there.
;
;QLEO	mov	ax,FIRST_ROM_SEGMENT
	mov	ax,[end_of_base_memory_PTE]
	shl	ax,8

RcNextROMlocation:
	call	CheckForROMHeader

ifndef	MSFLAG
	jc	short RcROMDetected
	sub	ax,NEXT_ROM_SEG
	call	CheckPrechargeBus
endif

	jnc	short RcLast
RcROMDetected:
	call	UpdatePageArray

RcLast:
	cmp	ax,LAST_ROM_SEGMENT
	jbe	short RcNextROMlocation

	pop	ax
	ret
ROMcheck	endp

;===============================================================================
;==
;==  RAMcheck: Detect RAM mapped in the Option ROM space.
;==
;==  Enter:
;==	Page4K[BX] = uninitialzed with detected RAM locations.
;==
;==  Exit:
;==	Page4K[BX] = initialzed with detected RAM locations. (Unusable)
;==
;===============================================================================
RAMcheck proc	near

	push	ax

;
; ROM area is checked to insure no common RAM is mapped in the option space.
;
;QLEO	mov	ax,FIRST_RAM_SEGMENT
	mov	ax,[end_of_base_memory_PTE]
	shl	ax,8

RAcNextRAMlocation:
	call	CheckForMappedRAM
	jnc	short RAcLast
	call	UpdatePageArray

RAcLast:
	cmp	ax,LAST_RAM_SEGMENT
	jbe	short RAcNextRAMlocation

	pop	ax
	ret
RAMcheck	endp

;===============================================================================
;==
;==  CPQVideoROM: Detect Compaq Video ROM.
;==
;==  Enter:
;==	Page4K[BX] = uninitialized for E000h segment.
;==
;==  Exit:
;==	Page4K[BX] = E000h segment is either usable or INUSE.
;==
;===============================================================================
CPQVideoROM	proc	near
	push	ax

;
; The option ROM area that sometimes Compaq's VIDEO ROM is remapped to is
; checked.  If nothing is there then fine.
;
	mov	ax,REMAP_VIDEO_ROM_SEGMENT
	call	CheckForROMHeader
	jnc	short CVRexit
;
; If there is something there then check to see if it is Compaq's VIDEO ROM.
; If it is (CY = clear) then this area can still be used as a EMS window.
; Otherwise it can't.
;
	call	CheckForCompaqROM
	jc	short CVR_InUse

; Identified Video ROM @ E000, add it to our high scan set as being available

	movzx	di, byte ptr cs:[HighSet]
	add	byte ptr cs:[HighSet], 4
	mov	word ptr cs:[HighSet][di], 0E000h	; start of video rom
	dec	ax
	mov	word ptr cs:[HighSet][di+2], ax 	;   end of video rom
	jmp	short CVRexit

CVR_InUse:
	call	UpdatePageArray

CVRexit:
	pop	ax
	ret
CPQVideoROM	endp

ifdef	ROMCOMP

;===============================================================================
;==
;==  CPQROMcheck: Detect CPQ ROMs which have unused space in the F000 segment.
;==
;== 		  32K ROMs F000:0    - F000:7FFF = mirror image of
;==			   F000:8000 - F000:FFFF
;==
;==   		    	386G  Deskpr 386/16, 386/20, 386/25
;==   			386F  Deskpro 386s
;==   			386H  Deskpro 386/20e (Horizon only)
;==
;== 		  40K ROMs F000:0    - F000:5FFF   junk
;==			   F000:6000 - F000:FFFF = system ROM
;==
;==   			386H  Deskpro 386/20e (Gambler only)
;==   			386H  Deskpro 386/25e
;==   			386L  Deskpro 386/33
;==
;==		  64K ROMs F000:0    - F000:FFFF
;==
;==	   		386P  Portable 386
;==			386E  Systempro/Deskpro 486/25
;==			386D  Deskpro 386n
;==
;==  		  The pointers which need to be changed in order to
;==		  reclaim the lower 32K space include:
;==
;==			1BD0	dummy end of interrupt handler
;==			1C1F	IRQ 9 entry point
;==			1C28	287 error entry point
;==			20FB	diskette parameter table entry point
;==			2E12	fixed disk BIOS entry point
;==			2FA4	old fixed disk hardware entry point
;==			3343	fixed disk hardware entry point
;==			4A97	real-time clock entry point
;==
;==  Enter:
;==
;==  Exit:
;==
;===============================================================================
pCutTable	equ	801Eh		; pointer to cut-off table
CTsig		equ	1234h		; signature

tSegBase	equ	1		; segment base
tPOST		equ	2		; POST
tRunTime	equ	3		; Run-Time
tSetup		equ	4		; Setup
tVideo		equ	5		; System video (int 42h)
tProt16		equ	6		; 16 bit protected mode capable code
tProt32		equ	7		; 32 bit protected mode capable code

sCutTable 	struc
  sCTsig	dw	CTsig		; signature
  sCTnum	db	?		; number of entries
  sCTtype	db	?		; type of entry
  sCTaddr	dw	?		; address
sCutTable 	ends

sCutTableEntry 	struc
  sCTEtype	db	?		; type of entry
  sCTEaddr	dw	?		; address
sCutTableEntry	ends

CPQROMcheck proc	near
	pusha
	push	ds
	push	es
	push	fs

	call	IsCompaq		;Q: Is it a Compaq 386
	jnz	CRcExit			; N: can't use ROM space

	cmp	cs:[ROMparm],TRUE	;Q: Was ROM(Compress) parameter set?
	jne	CRcExit			; N: don't use ROM space

	cmp	cs:[UMBset],TRUE	;Q: Was RAM specified?
	je	short CRcRAM		; Y: RAM was implied

	cmp	gs:[NoEMSset],TRUE	;Q: Was NoEMS specified?
	je	short CRcRAM		; Y: RAM was implied

	cmp	gs:[NoPFset],TRUE	;Q: Was FRAME=NONE specified?
	jne	CRcExit			; N: RAM not implied

;
;  Need to detect ROMs, include RAM, and fix the vectors (end of init)
;
CRcRAM:

	mov	ax,0F000h
	mov	fs,ax
	mov	ax,cs
	mov	es,ax
	assume	es:LAST
;
;  Access cut-off table
;
	mov	bx,fs:[pCutTable]	; get pointer to cut-off table
	cmp	fs:[bx].sCTsig,CTsig	;Q: Is this a valid table?
	jne	CRcExit			; N: no ROM compression
	movzx	cx,fs:[bx].sCTnum	; Y: get number of entries
	jcxz	CRcExit			; no ROM compression if no entries
;
;  Get segment base (assume F0000h)
;
	push	bx
CRcBase:
	mov	ax,fs:[bx].sCTaddr	; get possible base
	cmp	fs:[bx].sCTtype,tSegBase;Q: Is this the segment base entry?
	je	short CRcBaseDone	; Y: get base
	add	bx,size sCutTableEntry	; N: next entry
	loop	CRcBase
	mov	ax,0F000h		; default to F000h
CRcBaseDone:
	pop	bx
;
;  Get size of POST code
;
	movzx	cx,fs:[bx].sCTnum	; get number of entries again
CRcPOST:
	cmp	fs:[bx].sCTtype,tPOST	;Q: Is this the POST entry?
	je	short CRcPOSTdone	; Y: get base
	add	bx,size sCutTableEntry	; N: next entry
	loop	CRcPOST
	jmp	short CRcExit		; no ROM compression if no entries
CRcPOSTdone:
	mov	bx,fs:[bx].sCTaddr	; get size of POST
	inc	bx
	jmp	short CRcAddRAM		; map RAM

ifdef	ROMCOMP
	mov	al,fs:[0FFE4h]		; get ROM type

;
;  Check for 64K ROMs
;
	lea	di,ROM64k
	mov	cx,Num64kROMs
	repne scas ROM64k		;Q: 64K ROM?
	je	CRcExit			; Y: no ROM compression

;
;  Check for 40K ROMs
;
	mov	bx,6000h		; start of 40K ROMs
	lea	di,ROM40k
	mov	cx,Num40kROMs
	repne scas ROM40k		;Q: 40K ROM?
	je	short CRcCompress	; Y: ROM compression

;
;  Check for 32K ROMs
;
	mov	bx,8000h		; start of 32K ROMs
	lea	di,ROM32k
	mov	cx,Num32kROMs
	repne scas ROM32k		;Q: 32K ROM?
	jne	CRcExit			; N: no ROM compression
;
;  Compress the System ROM
;
CRcCompress:

	or	gs:[GenFlags],fROMComp  ; set ROM compression flag
	mov	cs:[ROMstart],bx	; save starting of ROM after compression

;
;  Map lower image of ROM with RAM
;
	mov	ax,0F000h		; start of RAM
endif

CRcAddRAM:
	shr	bx,4
	add	bx,ax			; end of RAM
	dec	bx
	movzx	di,byte ptr cs:[RAMset]	; add to RAM list
	add	byte ptr cs:[RAMset],4
	mov	cs:[RAMset][di],ax	; from F000h ...
	mov	cs:[RAMset][di][2],bx	; ... to either F800h or F600h

CRcExit:
	pop	fs
	pop	es
	pop	ds
	popa
	ret
	assume	es:nothing
CPQROMcheck	endp

endif	; ROMCOMP


;===============================================================================
;==
;==  ROMcompress: Check to see if lower 32k of system ROM can be mapped over by
;==		  RAM.	This code was adapted from Helix.
;==
;==  Enter:
;==
;==  Exit:
;==
;===============================================================================
	public	ROMcompress

ROMcompress proc near

	cmp	cs:[Highscan], TRUE
	jne	RcQuickExit

	pusha
	push	es
	push	ds

	call	chkt2mrg			;check for type 2 merge
	assume	ds:nothing, es:nothing
	jc	short domerge			;if found skip type 1 check

	mov	ax, 0f000h			;first 3000h words (12k)
	mov	ds, ax				; at f000:0000 & f000:8000
	mov	es, ax				; must be the same
	xor	si, si
	mov	di, 8000h
	mov	cx, 3000h
	cld
	repe cmpsw
	jne	short RcExit

	xor	si,si
	mov	ds,si
	mov	cx,128
chkvecs:lodsw
	mov	dx,ax
	lodsw
	cmp	ax,0f000h
	jne	short @f
	cmp	dx,8000h
	jae	short @f
	cmp	si,32h*4			;cpm jump?
	je	short @f			;skip it
	cmp	dx,6000h
	jae	short RcExit
@@:	loop	chkvecs

domerge:xor	si,si
	mov	ds,si
	mov	cx,128
merge:	lodsw
	mov	dx,ax
	lodsw
	cmp	ax,0f000h
	jne	short @f
	cmp	dx,8000h
	jae	short @f
	add	word ptr [si-4],8000h
@@:	loop	merge

;  Make sure BIOS points to INT 13h entry point on upper 32K image by making an
;  INT 2Fh AH=13h (DOS 3.20 and later): changes the INT 13h calls from IBMBIO.
;  Note: Does not affect DOS 3.00-3.10, but valid only on DOS 3.20 and later.

	mov	ah,13h
	int	2fh
	mov	ax,ds
	cmp	ax,0f000h
	jne	short @f
	or	dx,8000h
@@:	mov	ax,es
	cmp	ax,0f000h
	jne	short @f
	or	bx,8000h
@@:	mov	ah,13h
	int	2fh

; Add 32k ROM area to RAM list

	movzx	di, byte ptr cs:[HighSet]
	add	byte ptr cs:[HighSet], 4
	mov	word ptr cs:[HighSet][di], 0f000h	; from F000h
	mov	word ptr cs:[HighSet][di+2], 0f7ffh	;   to F7FFh

RcExit:
	pop	ds
	pop	es
	popa

RcQuickExit:
	ret

ROMcompress endp


chkt2mrg proc near
	xor	si,si
	mov	ds,si
	mov	cx,128
	cld
chkt2a: lodsw
	mov	bx,ax
	lodsw
	cmp	ax,0f000h
	jne	short @f
	cmp	bx,8000h
	jae	short @f
	mov	es,ax
	cmp	si,32h*4			;cpm jump?
	je	short @f
	cmp	byte ptr es:[bx],0e9h		;check for jmp $+8000
	jne	short chkt2ng
	cmp	word ptr es:[bx+1],7ffdh	;
	jne	short chkt2ng
@@:	loop	chkt2a
	mov	ah,8
	mov	dl,0
	int	13h
	mov	ax,es
	mov	ds,ax
	mov	si,di
	or	si,8000h
	mov	cx,11
	repe cmpsb
	jne	short chkt2ng
	mov	ah,8
	mov	dl,1
	int	13h
	mov	ax,es
	mov	ds,ax
	mov	si,di
	or	si,8000h
	mov	cx,11
	repe cmpsb
	jne	short chkt2ng
	mov	ah,8
	mov	dl,80h
	int	13h
	mov	ax,es
	mov	ds,ax
	mov	si,di
	or	si,8000h
	mov	cx,11
	repe cmpsb
	jne	short chkt2ng
	stc
	ret
chkt2ng:clc
	ret
chkt2mrg endp

	assume	ds:_DATA

;ifndef IBMCOPYRIGHT

	; IBM version doesn't do this because their QBASIC uses ROM Basic

;===============================================================================
;==
;==  ROMbasic:	Check to see if machine has ROM basic that can be mapped
;==		over by RAM.  This code was adapted from Helix.
;==
;==  Enter:
;==
;==  Exit:
;==
;===============================================================================
	public	ROMbasic

basic_id db	'The IBM Personal Computer Basic'
basic_id_length equ $-basic_id

ROMbasic proc	near

	push	es

	cmp	cs:[Highscan], TRUE
	jne	RbNoBasic

	test	gs:[GenFlags], fMCA	; Q: MCA
	jnz	short RbChk4Basic	; Y: look for ROM basic

	mov	ax,3518h		; code from Helix to look for ROM basic
	int	21h			; on non-MCA machines
	mov	ax,es
	cmp	ax,0f600h
	jne	short RbNoBasic
	cmp	bx,0
	jne	short RbNoBasic
	mov	di,8000h
	mov	cx,32
	mov	al,'I'
	cld
	repne	scasb
	jne	short RbNoBasic
	cmp	word ptr es:[di],'MB'
	jne	short RbNoBasic

;  We either have an MCA machine, or a non MCA which looks like it might have
;  ROM basic (non MCA PS/2).  Do a more detailed ROM Basic search.

RbChk4Basic:

	mov	ax,0f600h		; more code from Helix
	mov	es,ax
	xor	di,di
	mov	cx,8000h-basic_id_length
	mov	al, cs:[basic_id]
basic_loop:
	repne	scasb
	jne	short RbNoBasic
	push	cx
	push	di
	mov	si,offset LAST:basic_id+1
	mov	cx,basic_id_length-1
	push	ds
	push	cs
	pop	ds
	rep	cmpsb
	pop	ds
	pop	di
	pop	cx
	jne	short basic_loop

;  Looks like we found ROM basic, add 32k to our HighScan set

	movzx	di, byte ptr cs:[HighSet]
	add	byte ptr cs:[HighSet], 4
	mov	word ptr cs:[HighSet][di], 0f600h	; from F600h
	mov	word ptr cs:[HighSet][di+2], 0fdffh	;   to FDFFh

RbNoBasic:
	pop	es

	ret

ROMbasic endp

;endif	IBMCOPYRIGHT

;===============================================================================
;==
;==  DefaultROM: Check to see if C600-C7FF should be excluded due to a bad CPQ
;==		 Video ROM.
;==
;==  Enter:
;==
;==  Exit:
;==
;===============================================================================
DefaultROM proc	near
	push	ds

	call	IsCompaq		;Q: Is it a Compaq 386
	jnz	short DRcheckMCA	; N: check if E000 should be excluded
;
;  Check if CPQ video ROMs are dated between 01/16/90 and 07/30/90
;
	push	0C000h
	pop	ds
	cmp	word ptr ds:[5FFBh],'09';Q: VGA ROMs from '90?
	jne	short DRcheckE000	; N: no problem
	cmp	byte ptr ds:[5FFAh],'/'	;Q: VGA ROMs have format mm/dd/yy?
	jne	short DRcheckE000	; N: no problem
	cmp	byte ptr ds:[5FF7h],'/'	;Q: VGA ROMs have format mm/dd/yy?
	jne	short DRcheckE000	; N: no problem
	cmp	byte ptr ds:[5FF5h],'0'	;Q: VGA ROMs have format mm/dd/yy?
	jne	short DRcheckE000	; N: no problem

	cmp	byte ptr ds:[5FF6h],'7'	;Q: VGA ROMs after July?
	ja	short DRcheckE000	; Y: no problem

	cmp	byte ptr ds:[5FF6h],'1'	;Q: VGA ROMs after January?
	ja	short DRexcC7FF		; Y: problem, exclude to C7FF

	cmp	byte ptr ds:[5FF8h],'1'	;Q: After Jan 20, 1990?
	ja	short DRexcC7FF		; Y: exclude
	jb	short DRcheckE000	; N: no problem

	cmp	byte ptr ds:[5FF9h],'6'	;Q: Before Jan 16, 1990?
	jb	short DRcheckE000	; Y: no problem

DRexcC7FF:
	mov	cs:[VGAROM],0C7FFh	; exclude to C7FFh area
	jmp	short DRcheckE000

DRcheckMCA:

	test	gs:[GenFlags], fMCA	; Q: MCA system
	jz	short DRexit		; N: finished

	mov	cs:[VGAROM],0BFFFh	; Y: include C000 ROM area

	mov	cs:[E000ROM],0E000h	;    and exclude E000 ROM area

DRcheckE000:
	;
	; DO NOT ADD ANY CODE BETWEEN DRcheckE000 & DRexit
	;

DRexit:
	pop	ds
	ret
DefaultROM	endp

;===============================================================================
;==
;==  CheckToken: Checks for Token Ring card.  Adapted from code by Helix.
;==
;==  Enter:
;==
;==  Exit:
;==
;===============================================================================
	public	CheckToken

CheckToken proc near

	push	si
	push	di
	push	es

	cmp	cs:[NoTR], TRUE 	;Don't do it if disabled
	je	CT_exit

	mov	dx,0a20h			;load primary base i/o address
	in	al,dx				;get BIOS/MMIO address
	cmp	al,0ffh 			;token ring adapter present ?
	jne	short gottr			;yes
	mov	dx,0a24h			;load alternate base i/o address
	in	al,dx				;get BIOS/MMIO address
	cmp	al,0ffh 			;token ring adapter present ?
	je	short CT_exit			;no
gottr:	and	al,0fch 			;mask off interrupt bits
	shr	al,1				;shift to correct position
	or	al,80h				;mask on high bit
	mov	ah,al				;load ah with result
	mov	al,0				;zero al
	mov	es,ax				;load es with rom address
	mov	si,1f00h			;point to adapter id area
	mov	cx,12				;load id word length
chkid:	lods	word ptr es:[si]		;get id byte
	and	al,0fh				;mask off high nybble
	xor	al,0fh				;complement value
	mov	ah,es:[si+16h]			;get complement id byte
	and	ah,0fh				;mask off high nybble
	cmp	al,ah				;does complement id byte match ?
	jne	short CT_exit			;no
	loop	chkid				;iterate loop

	push	dx				;save port address
	mov	bx, es				;mark the 8k ROM space as INUSE
	mov	dl, INUSE
	call	SetFlag
	inc	bh				;2nd 4k page of ROM
	call	SetFlag
	pop	dx

	test	gs:[GenFlags], fMCA		; Q: MCA system
	jnz	CT_mca				; Y:

	mov	ah, es:[1E00h]			; N: get shared RAM addr on ISA
	jmp	short CT_len

CT_mca: 					;get shared RAM address on MCA
	inc	dx				;add 2 to I/O address
	inc	dx
	in	al, dx				;get shifted RAM address
	and	al, 0FEh			;mask off low bit
	mov	ah, al

CT_len:
	xor	al, al				;ax = paragraph RAM address
	xor	ch, ch
	mov	cl, es:[1E01h]			;get shared RAM size
	and	cl, 0Ch 			;(bits 2&3: 00=8k,  01=16k,
	shr	cl, 1				;	    10=32k, 11=64k)
	shr	cl, 1
	inc	cl
	shl	cl, 1				;cx = RAM size in 4k pages

	mov	bx, ax				;mark the shared RAM space
	mov	dl, INUSE			;  as INUSE
CT_set_ram:
	call	SetFlag
	inc	bh
	loop	CT_set_ram

CT_exit:
	pop	es
	pop	di
	pop	si
	ret

CheckToken endp


ifdef E000

;===============================================================================
;==
;==  CheckE000: Determines if E000h area is available by default.  Adapted
;==		from code by Helix.
;==
;==  Enter:
;==
;==  Exit:
;==
;===============================================================================
	public	CheckE000

CheckE000 proc	near

;  DefaultROM will have set E000ROM to 0E000h if this is not a Compaq
;  system.  This causes E000h-EFFFh to be excluded as ROM by default,
;  but we want to be a little smarter than that.  If we can identify
;  and 'deshadow' a video bios in the E000h area, E000h is made available
;  by default.

	push	ds
	push	es
	assume	ds:nothing, es:nothing

	cmp	cs:[Highscan], TRUE
	jne	CE_exit

	mov	ax,0e000h		; from Helix...
	mov	es,ax
	cmp	word ptr es:[0],0aa55h
	jne	short CE_exit
	mov	ch,es:[2]
	mov	cl,0
	push	ds
	mov	ax,0c000h
	mov	ds,ax
	xor	si,si
	xor	di,di
	rep	cmpsw
	pop	ds
	jne	short CE_exit

	call	deshadow_command
	jc	short CE_exit

;  'Deshadowed' the video rom, add first 32k of E000 as RAM.  Why 32k?
;  That's what Helix does!

	movzx	di, byte ptr cs:[HighSet]
	add	byte ptr cs:[HighSet], 4
	mov	word ptr cs:[HighSet][di], 0E000h	; from E000h
	mov	word ptr cs:[HighSet][di+2], 0E7FFh	;   to E7FFh

CE_exit:
	pop	es
	pop	ds

	ret

CheckE000 endp

	public	deshadow_command

deshadow_command proc near
	push	ax				;save work regs
	push	bx				;...
	push	cx				;...
	push	si				;...
	push	di				;...
	push	ds				;...
	push	es				;...

; scan interrupt vectors for those pointing to E000 area
	xor	ax,ax				;assume IDT at zero
	mov	es,ax				;...
	xor	di,di				;zero offset
deshadow_command_2:
	mov	ax,word ptr es:[di]		;get offset
	shr	ax,4				;...
	add	ax,word ptr es:[di+2]		;add in segment to get abs. seg
	jc	short deshadow_command_4	;overflow= bigger then ff
	cmp	ah,0e0h 			;if not between e0
	jb	short deshadow_command_4
	cmp	ah,0e7h 			;and e7
	ja	short deshadow_command_4	; skip
; vector found pointing to E000 area
	mov	bx,di				;save offset of vector found
	lds	si,es:[di]			;point DS:SI at target area
	mov	ax,ds				;convert to C000 address
	sub	ax,2000h			;...
	mov	es,ax				;equivalent area of C000
	mov	di,si				;same offset
	mov	cx,100h 			;check for 256 bytes the same
	repe cmpsb				;compare two areas
	jne	short deshadow_command_error	;problems

; code in C000 seems to be the same as in the E000 area
	xor	ax,ax				;restore vector pointer
	mov	es,ax				;...
	mov	di,bx				;...
	sub	byte ptr es:[di+3],20h		;change E000 vector to C000

deshadow_command_4:
	add	di,4				;step to next vector
	cmp	di,320h 			;if more to check,
	jb	short deshadow_command_2	;look at the next vector

; complete IDT scanned with no problem cases

	pop	es				;restore entry regs
	pop	ds				;...
	pop	di				;...
	pop	si				;...
	pop	cx				;...
	pop	bx				;...
	pop	ax				;...
	clc					;clear carry for ok
	ret					;& return

deshadow_command_error:
	pop	es				;restore entry regs
	pop	ds				;...
	pop	di				;...
	pop	si				;...
	pop	dx				;...
	pop	cx				;...
	pop	bx				;...
	pop	ax				;...
	xor	dx,dx
	stc					;set carry for error
	ret					;& return
deshadow_command endp

	assume	ds:_DATA
endif

;===============================================================================
;==
;==  DefaultInclude: Determines if Include parameter is for RAM or EMS.
;==
;==  Enter:
;==	cs:IncSet[1] = RAM
;==
;==  Exit:
;==	cs:IncSet[1] = RAM or EMS
;==
;===============================================================================
DefaultInclude	proc	near
	push	ax
	push	bx
	push	si
	push	di

	lea	bx,cs:[RAMSet]		; assume RAM
	xor	si,si

	cmp	cs:[UMBset],TRUE	;Q: Was RAM specified?
	je	short DItran		; Y: RAM was implied

	cmp	gs:[NoEMSset],TRUE	;Q: Was NoEMS specified?
	je	short DItran		; Y: RAM was implied

	cmp	gs:[NoPFset],TRUE	;Q: Was FRAME=NONE specified?
	je	short DItran		; Y: RAM was implied
;
;  No RAM parameters were specified, thus Include is taken as EMS windows.
;
	lea	bx,cs:[EMSSet]		; default is EMS
;
;  Move Include ranges to either RAM or EMS ranges
;
DItran:
	movzx	di,byte ptr cs:[bx]	; get pointer into RAM/EMS set
	mov	ax,cs:[IncSet][2][si]	; get address from INCLUDE set
	mov	cs:[bx][di],ax		; save in RAM/EMS set
	or	ax,ax			;Q: Any more addresses?
	jz	short DIexit		; N: exit
	add	si,2			; Y: increment to next address
	add	byte ptr cs:[bx],2
	jmp	short DItran		; get next address

DIexit:
	pop	di
	pop	si
	pop	bx
	pop	ax
	ret
DefaultInclude	endp

;===============================================================================
;==
;==  ProcessRange: Process range array constructed by the parser and transfer
;==	           information to the [Page4K] array.  The parser array has
;==		   the following structure:
;==
;==		   0  [n*4+2|array flag]
;==		   2  [ entry # 1 FROM ]
;==		   4  [ entry # 1 TO   ]
;==		   6  [ entry # 2 FROM ]
;==		   8  [ entry # 2 TO   ]
;==		   :  :	    :   : :  : :
;==	       n*4-2  [ entry # n FROM ]
;==	       n*4+2  [ entry # n TO   ]
;==
;==  Enter:
;==	SI = address of parser range array to process.
;==	Page4K[0..255] = Uninitialized with flag in parser array.
;==
;==  Exit:
;==	Page4K[0..255] = Initialized with this flag.
;==
;===============================================================================
ProcessRange	proc	near
	push	ax
	push	bx
	push	dx
	push	ds

	mov	ax,seg LAST
	mov	ds,ax
	assume	ds:LAST

	mov	dl,byte ptr [si+1]	; array flag (include,exclude,RAM,EMS,etc...)
	add	si,2			; point ro first range

PRnext:
	lodsw				; Get FROM
	or	ax,ax			;Q: Any more ranges?
	jz	short PRexit		; N: finished processing
	mov	bx,ax			; Y: BX has lower limit

	call	SetFlag			; set flag for FROM value

	lodsw				; AX has upper limit

PRnextPage:
	add	bx,100h			; next 4K page
	jc	PRnext			; if overflow, goto next range
	cmp	bx,ax			;Q: Has upper limit been reached?
	ja	PRnext			; Y: Try next range (X=FROM-TO)
	call	SetFlag			; N: set flag for next 4K page
	jmp	short PRnextPage

PRexit:
	pop	ds
	pop	dx
	pop	bx
	pop	ax
	ret
	assume	ds:_DATA
ProcessRange	endp

;===============================================================================
;==
;==  ExcludeRange: Exclude range in Page4K[] array.
;==
;==  Enter:
;==	AX = starting paragraph address
;==	BX = ending paragraph address
;==
;==  Exit:
;==	Page4K[AX-BX] = EXCLUDE
;==
;===============================================================================
ExcludeRange	proc	near
	push	bx
	push	dx

	mov	dl,EXCLUDE
ERloop:
	call	SetFlag
	sub	bx,100h
	cmp	ax,bx		;Q: End of range?
	jbe	short ERloop	; N: continue excluding

	pop	dx
	pop	bx
	ret
ExcludeRange	endp

;===============================================================================
;==
;==  SetFlag: Set proper flag in Page4K[] array.
;==
;==  Enter:
;==	DL = proper flag.
;==	BX = index to Page4K[] array.
;==
;==  Exit:
;==	Page4K[BX] = DL
;==
;===============================================================================
SetFlag	proc	near
	push	bx

	shr	bx,8			; paragraph to 4K page
	and	cs:[Page4K][bx],not INUSE ; no longer INUSE
	or	cs:[Page4K][bx],dl	  ; mark with current flag

	pop	bx
	ret
SetFlag	endp

;===============================================================================
;==
;==  RangeOverlap: Checks Include/eXclude/RAM/EMS ranges for overlap.  Exclude
;==		   has precedence over WIN, RAM or EMS.  WIN has precedence
;==		   over RAM or EMS.  RAM has precedence over EMS.
;==
;==  Enter:
;==	Page4K[] array completely filled out.
;==
;==  Exit:
;==	[msg_flag] = Overlap_MSG flag set if an overlap is detected.
;==
;===============================================================================
RangeOverlap	proc	near
	push	ecx

	mov	ecx,100h			; loop through 256 entries
ROPage4K:
	test	cs:[Page4K][ecx-1],EMS+RAM+ROM+WIN  ;Q: EMS,RAM,ROM or WIN set?
	jz	short ROnextPage4K		    ; N: next 4K page

	test	cs:[Page4K][ecx-1],EXCLUDE	;Q: Is this page also excluded?
	jz	short RO4KWINpage		; N: try WIN
	and	cs:[Page4K][ecx-1],EXCLUDE	; Y: mark 4K page as excluded
	jmp	short ROmsg			; warn user with msg

RO4KWINpage:
	test	cs:[Page4K][ecx-1],EMS+RAM+ROM	;Q: EMS,RAM,or ROM set?
	jz	short ROnextPage4K		; N: next 4K page

	test	cs:[Page4K][ecx-1],WIN		;Q: Is this page also WIN?
	jz	short RO4KRAMpage		; N: try RAM
	and	cs:[Page4K][ecx-1],WIN		; Y: mark 4K page as WIN
;;;	jmp	short ROmsg			; warn user with msg
	jmp	short RONextPage4K		; no warning for WIN= overlap
						; (MEMMAKER makes WIN= overlaps)
RO4KRAMpage:
	test	cs:[Page4K][ecx-1],RAM+ROM	;Q: Is RAM or ROM set?
	jz	short ROnextPage4K		; N: EMS only, next 4K page

	btr	cs:[Page4K][ecx-1],EMSbit	;Q: Is RAM/ROM and EMS set?
	jnc	short ROnextPage4K		; N: RAM only, next 4K page
ROmsg:
	or	gs:[msg_flag],Overlap_MSG	; range overlap message

ROnextPage4K:
	loop	ROPage4K

	pop	ecx
	ret
RangeOverlap	endp

;===============================================================================
;==
;==  GetPageFrame:  Given the user has not selected a page frame base address,
;==		    this routine will find an available 64K area.
;==
;==  Enter:
;==	Page4K[] array completely filled out.
;==
;==  Exit:
;==	[PF_Base] = page frame base address
;==
;===============================================================================
GetPageFrame	proc	near

;
;  Check to see if the base address of the page frame needs to be selected
;
	cmp	gs:[NoEMSset],TRUE	;Q: Has NoEMS switch been used?
	je	short GPFexit		; Y: don't select one.

	cmp	gs:[NoPFset],TRUE	;Q: Has FRAME=NONE switch been used?
	je	short GPFexit		; Y: don't select one.

	cmp	gs:[PF_Base],FREE	;Q: Has page frame been selected?
	jne	short GPFexit		; Y: don't reselect one.

	test	cs:[PnSet],P0+P1+P2+P3	;Q: XMA2EMS mode selected?
	jnz	short GPFexit		; Y: don't select page frame
;
;  Check default page frame address of E000h for a conflict
;
	mov	ax,0E000h		; default page frame address
	call	CheckPageFrame		; check for a page frame
	or	bx,bx			;Q: Conflict?
	jz	short GPFfound		; N: found page frame
;
;  Check area from 8000h to F000h for page frame with no conflict.
;
	mov	dx,[end_of_base_memory_PTE] ; get lowest PTE candidate
	shl	dx,8			; paragraph address
	mov	ax,0F000h		; highest page frame possible

	cmp	dx,8000h		;Q: Above 512K?
	jae	short GPFfind		; Y: continue
	mov	dx,8000h		; N: lowest possible page frame address
GPFfind:
	call	CheckPageFrame		; check [AX] for a page frame
	or	bx,bx			;Q: Conflict?
	jz	short GPFfound		; N: found page frame
	sub	ax,400h			; Y: try 16K lower
	cmp	ax,dx			;Q: Valid page frame address?
	jae	short GPFfind		; Y: try this address range
	mov	ax,FREE			; N: no page frame address found

GPFfound:
	mov	gs:[PF_Base],ax		; save page frame address

GPFexit:
	ret
GetPageFrame	endp

;===============================================================================
;==
;==  SetPageFrame: Given a page frame address, it sets up the EMS_window[],
;==		   EMS_window_location[], and EMSsegLoc[].
;==
;==
;==  Enter:
;==	[PF_Base] page frame base address assigned.
;==	Page4K[]  array completely filled out.
;==
;==  Exit:
;==	EMS_window_location[]
;==	EMSsegLoc[]
;==	Page4K[] array completely filled out.
;==
;===============================================================================
SetPageFrame	proc	near

	mov	[number_EMS_windows],0	; save number of EMS windows

;
;  Check to see if the base address of the page frame needs to be set
;
	cmp	gs:[NoEMSset],TRUE	;Q: Has NoEMS switch been used?
        je      SPFexit			; Y: don't select one.

	cmp	gs:[NoPFset],TRUE	;Q: Has FRAME=NONE switch been used?
	je	SPFexit			; Y: don't select one.

	cmp	gs:[PF_Base],FREE	;Q: Has page frame been selected?
	je	SPFexit			; N: don't set one.

	test	cs:[PnSet],P0+P1+P2+P3	;Q: XMA2EMS mode selected?
	jnz	short SPFinvParm	; Y: don't select page frame

	mov	ax,gs:[PF_Base]		; get page frame address
;
;  Make sure page frame was not set in base memory
;
	mov	bx,[end_of_base_memory_PTE] ; get lowest PTE candidate
	shl	bx,8			; paragraph address
	cmp	ax,bx			;Q: Valid page frame address?
	jae	short SPFcont		; Y: continue
	mov	gs:[PF_Base],FREE	; N: can not load
	jmp	short SPFexit

SPFcont:
;
;  Exclude range from Page4K[] array and check for conflicts
;
	mov	bx,ax
        shr     bx,8                            ; paragraph to 4K page
        mov     cx,16
SPFConflictLoop:
        test    cs:[Page4K][bx],not (EMS or RAM);Q: Marked for usage other than EMS?
        jz      short SPFnoConflict             ; N: no conflict
        or      gs:[msg_flag],PF_WARN_MSG       ; Y: warn user of conflict
SPFnoConflict:
        or      cs:[Page4K][bx],EXCLUDE         ; exclude from usage
        inc     bx
        loop    SPFConflictLoop	                ; continue excluding
;
;  Initialize EMS_window_location[], EMSsegLoc[], and EMS_window[] with information
;
	mov	bx,ax
        add     bx,0C00h                ; EMS window number 3
	shr	bx,8
	mov	ecx,4   		; number of windows in page frame
SPFloop:
	mov	[EMS_window_location][ecx*2-2],bx     ; address of window
;QEMS	mov	[EMS_window][ecx*2+ecx-3].handle,FREE ; window is free
	mov	[EMSsegLoc][bx],cl		      ; window index
	dec	[EMSsegLoc][bx]			      ; window index
	sub	bx,4				      ; next window
	loop	SPFloop

	mov	[number_EMS_windows],4	; save number of EMS windows
SPFexit:
	ret

SPFinvParm:
	or	gs:[msg_flag],INV_PARM_MSG
	jmp	short SPFexit
SetPageFrame	endp

;===============================================================================
;==
;==  EMSwindows:
;==
;==
;==
;==  Enter:
;==	Page4K[] array completely filled out.
;==
;==  Exit:
;==	EMS_window_location[]
;==	Page4K[] array completely filled out.
;==
;===============================================================================
EMSwindows proc	near
;
;  Check to see if EMS windows are needed
;
	cmp	gs:[NoEMSset],TRUE	;Q: Has NoEMS switch been used?
	je	EwExit			; Y: don't create EMS windows

	cmp	gs:[NoPFset],TRUE	;Q: Has FRAME=NONE switch been used?
	je	EwExit			; Y: don't create EMS windows

	test	cs:[PnSet],P0+P1+P2+P3+P254+P255 ;Q: XMA2EMS mode selected?
	jnz	short EwXMA2EMS			  ; Y: set XMA2EMS mode

;
;  Create as many EMS windows as possible in the 640K to 1MB region.
;
	mov	ax,[end_of_base_memory_PTE] ; get lowest PTE candidate
	add	ax,3
	shr	ax,2			; round to next 16K boundary
	mov	cx,100h shr 2		; 1MB index in 16K blocks
	sub	cx,ax			; number of PTE entries to 1MB
	shl	ax,10			; paragraph address
	movzx	esi,[number_EMS_windows]; get number of EMS windows so far
EwLoop:
	mov	bx,EMS			; erase EMS flag
	call	CheckEMSWindow
	test	bx,not EMS		;Q: Conflict?
	jnz	short EwNext		; Y: try next 16K window

	test	bx,EMS			;Q: Is this page specified as EMS?
	jnz	short Ewfound		; Y: use for EMS

	cmp	cs:[UMBset],TRUE	;Q: Has RAM switch been used?
	je	EwNext			; Y: use RAM as default
					; N: use EMS as default
EwFound:
	mov	bx,ax			; N: exclude range
	add	bx,3FFh
	call	ExcludeRange
;
;  Set up EMS_window_location[] and EMS_Window[]
;
	mov	bx,ax
	shr	bx,8
	mov	[EMS_window_location][esi*2],bx
;QEMS	mov	[EMS_window][esi*2+esi].handle,FREE
	xchg	ax,si
	mov	[EMSsegLoc][bx],al		      ; window index
	xchg	ax,si
	inc	si
EwNext:
	add	ax,400h			; next window
	loop	EwLoop

	mov	[number_EMS_windows],si	; save number of EMS windows
	jmp	EwExit

;
;  Need to place CEMM in XMA2EMS mode.
;
EwXMA2EMS:
	mov	[XMA2EMS],TRUE		; configure in XMA2EMS mode

	cmp	gs:[PF_Base],FREE	;Q: Has a page frame been set?
	je	short EwXPn		; N: just look at Pn set
	mov	ax,gs:[PF_Base]		; Y: get base address
	shr	ax,8			; get PTE index
	mov	cx,4			;
	xor	bx,bx
EwXloop:
	mov	Pn[bx],al		; save starting address
	bts	[PnSet],bx		; set Pn value
	inc	bx			; next Pn value
	add	al,4			; next window base address
	loop	EwXloop
;
;  Check conflicts with P0-P3 & P254-P255
;
EwXPn:
	xor	esi,esi			; index into Pn
	xor	dx,dx			; number of EMS windows
	mov	cx,6			; check P0-3 & P254-255
EwXPnLoop:
	bt	[PnSet],si		;Q: Is this P set?
	jnc	short EwXnextPn		; N: skip it
	mov	al,Pn[si]		; Y: get address

	xor	bx,bx
	shl	ax,8			; change to paragraph
	cmp	gs:[PF_Base],FREE	;Q: Has a page frame been set?
	je	short EwXcont1		; N: check for conflicts
	cmp	si,3			;Q: PF has already been tested?
	jbe	short EwXcont		; Y: don't check
EwXcont1:				; N: check if valid EMS page
	call	CheckEMSWindow		; check for conflict
;
;QLEO: Need a message if this conflicts with a page marked as RAM
;
	test	bx,EXCLUDE		;Q: Is this window excluded?
	jnz	short EwNoWay		; Y: can't do it
	test	bx,INUSE		;Q: Is there a ROM in the window?
	jz	short EwXcont		; N: continue
	mov	cs:[PFB_warning],TRUE	; Y: warn user
EwXcont:
;
;  Set up EMS_window_location[] and EMS_Window[]
;
	mov	bx,ax
	shr	bx,8
	mov	[EMS_window_location][esi*2],bx
;QEMS	mov	[EMS_window][esi*2+esi].handle,FREE
	xchg	ax,si
	mov	[EMSsegLoc][bx],al		      ; window index
	xchg	ax,si
;
;  Exclude window range from available area
;
	mov	bx,ax
	add	bx,3FFh
	call	ExcludeRange
	inc	dx							;@PIW
EwXnextPn:
	inc	si
	loop	EwXPnLoop
	mov	[number_EMS_windows],dx	; number of EMS windows
;
; Now that all Pn's are set, can a page frame be formed?
;
EwXPF:
	mov	cx,3
	mov	esi,3
	mov	ax,[EMS_window_location][esi*2]
EwXPFset:
	dec	si
	mov	bx,[EMS_window_location][esi*2]
	sub	ax,bx
	cmp	ax,4			;Q: Are they next to each other?
	jne	short EwXnoPF		; N: page frame is not available
	mov	ax,bx			; Y: get last address
	loop	EwXPFset

	shl	ax,8
	mov	gs:[PF_Base],ax		; save page frame address
	jmp	short EwExit
EwExit:
	ret

EwXnoPF:
	or	gs:[msg_flag],NO_LIM_PF_MSG ; no page frame warning
	jmp	short EwExit

EwNoWay:
	mov	gs:[PF_Base],FREE
	jmp	short EwExit

EMSwindows	endp

;===============================================================================
;==
;==  UMBwindows:
;==
;==
;==  Enter:
;==	Page4K[] array completely filled out.
;==
;==  Exit:
;==	EMS_window_location[]
;==	Page4K[] array completely filled out.
;==
;===============================================================================
UMBwindows proc	near

	mov	bx,[end_of_base_memory_PTE] ; get lowest PTE candidate
	mov	cx,100h			    ; 1MB PTE entry
	sub	cx,bx			    ; number of PTE entries to 1MB
	xor	si,si
UMBloop:
	test	cs:[Page4K][bx],not (RAM+ROM);Q: Is this page used?
	jnz	short UMBnotFound	     ; Y: no UMB here

	test	cs:[Page4K][bx],RAM+ROM	;Q: Is this page specified as RAM or ROM?
	jnz	short UMBfound		; Y: use as a UMB

	cmp	gs:[NoEMSset],TRUE	;Q: Has NoEMS switch been used?
	je	UMBfound		; Y: UMBs implied

	cmp	gs:[NoPFset],TRUE	;Q: Has FRAME=NONE switch been used?
	je	UMBfound		; Y: UMBs implied

	cmp	cs:[UMBset],TRUE	;Q: Has RAM switch been used?
	jne	UMBnotFound		; N: no RAM by default

UMBfound:
	test	cs:[Page4K][bx],ROM	;Q: Is this page specified as ROM?
	jnz	short UMBcont		; Y: don't add to base memory

	cmp	[end_of_base_memory_PTE],bx;Q: Is this adjacent to base memory?
	jne	short UMBcont		   ; N: don't add to base memory
	inc	[end_of_base_memory_PTE]   ; Y: Add to base memory?

UMBcont:
;QLEO	or	cs:[Page4K][bx],EXCLUDE
	mov	cs:[UMBtable][si],bl
	inc	si
UMBnotFound:
	inc	bx
	loop	UMBloop
UMBend:
	mov	bx,si
	mov	cs:[NumOfUMBwindows],bl
	ret
UMBwindows	endp

;===============================================================================
;==
;==  BaseEMSwindows:
;==
;==
;==
;==  Enter:
;==	Page4K[] array completely filled out.
;==
;==  Exit:
;==	EMS_window_location[]
;==	Page4K[] array completely filled out.
;==
;===============================================================================
BaseEMSwindows	proc	near
;
;  Check to see if base EMS windows are needed
;
ifdef NoEMS
	cmp	gs:[NoEMSset],TRUE	;Q: [NoEMS] mode?
	je	short BEwExit		; Y: no base EMS windows
endif
	cmp	[XMA2EMS],TRUE		;Q: XMA2EMS mode?
	je	short BEwExit		; Y: no base EMS windows

	movzx	esi,[number_EMS_windows]; get number of EMS windows so far
	mov	cx,[end_of_base_memory_PTE] ; beyond base window
	add	cx,3
	shr	cx,2
	xor	ax,ax
BEwLoop:
	xor	bx,bx
	call	CheckEMSWindow
	test	bx,not (EMS+RAM)
	jnz	short BEwNext
	mov	bx,ax
	shr	bx,8
	mov	[EMS_window_location][esi*2],bx
;QEMS	mov	[EMS_window][esi*2+esi].handle,FREE
	xchg	ax,si
	mov	[EMSsegLoc][bx],al		      ; window index
	xchg	ax,si
	mov	bx,ax
	add	bx,3FFh
	call	ExcludeRange
	inc	si
BEwNext:
	add	ax,400h
	loop	BEwLoop
	mov	[number_EMS_windows],si

BEwExit:
	ret
BaseEMSwindows	endp

;===============================================================================
;==
;==  CheckEMSWindow: Given a starting paragraph address, this routine will
;==		     return a composite of all the flags encountered in the
;==		     16K range.
;==
;==  Enter:
;==	AX = Starting paragraph
;==	BX = flag to clear from range: EMS, RAM, EXCLUDE, INUSE
;==
;==  Exit:
;==	AX = same
;==	BX = 0 		No conflict. No carry.
;==	     EXCLUDE    Conflict with an excluded 4K page
;==	     RAM	Conflict with a RAM 4K page
;==	     INUSE	Conflict with a default excluded area
;==
;===============================================================================
CheckEMSWindow	proc	near
	push	ax
	push	cx
	push	dx

	not	bx		; complement flag
	mov	dx,bx		; flag to clear
	mov	bx,ax		; starting paragraph address
	xor	ax,ax		; clear flags
	shr	bx,8
	mov	cx,4
CEWloop:
	or	al,cs:[Page4K][bx]
	and	cs:[Page4K][bx],dl
	inc	bx
	loop	CEWloop
	mov	bx,ax
	and	bx,not EMS

	pop	dx
	pop	cx
	pop	ax
	ret
CheckEMSWindow	endp

;===============================================================================
;==
;==  CheckPageFrame: Given a starting paragraph address, this routine will
;==		     return a composite of all the flags encountered in the
;==		     64K range.
;==
;==  Enter:
;==	AX = Starting paragraph
;==
;==  Exit:
;==	AX = same
;==	BX = 0 		No conflict. No carry.
;==	     EXCLUDE    Conflict with an excluded 4K page
;==	     RAM	Conflict with a RAM 4K page
;==	     INUSE	Conflict with a default excluded area
;==
;===============================================================================
CheckPageFrame	proc	near
	push	ax
	push	cx
	push	dx

	xor	dx,dx
	mov	cx,4
CPFloop:
	xor	bx,bx
	call	CheckEMSWindow
	or	dx,bx
	add	ax,400h
	loop	CPFloop
	mov	bx,dx

	pop	dx
	pop	cx
	pop	ax
	ret
CheckPageFrame	endp

;===============================================================================
;==
;==  UpdatePageArray: Updates page array (Page4K[]) with detected ROM/RAM.
;==
;==  Enter:
;==	AX = Paragraph address of next ROM/RAM position.
;==     DX = Length of ROM/RAM in paragraphs
;==
;==  Exit:
;==	[Page4K] updated
;==
;==
;===============================================================================
UpdatePageArray	proc	near
	push	bx

	mov	bx,ax
	sub	bx,dx			; start of ROM/RAM
UPAloop:
	shr	bx,8			; index into 4K page
	or	cs:[Page4K][bx],INUSE	; mark it used
	inc	bx			; next page
	shl	bx,8			; paragraph address
	cmp	bx,ax			;Q: Still in the ROM/RAM area?
	jb	short UPAloop		; Y: mark this page as used

	pop	bx
	ret
UpdatePageArray	endp

ifndef	MSFLAG
;===============================================================================
;==
;==  CheckPrechargeBus: Detects a ROM which did not include a ROM header.
;==			The routine expects a floating bus to be precharged
;==			to either 00 or FF.  Thus, if any other pattern is read,
;==			a non-standard ROM will be reported.
;==
;==  Enter:
;==	AX = Paragraph address of next ROM/RAM position.
;==
;==  Exit:
;==	CY = A non-standard ROM was found at this location.
;==	NC = Did not find a valid ROM at this location.
;==	AX = Paragraph address of next ROM location.
;==     DX = Length of this ROM in paragraphs
;==
;==
;===============================================================================
CheckPrechargeBus proc	near
	push	ds
;
;  Assume no ROM will be found: Floating Bus
;
	mov	ds,ax
	mov	dx,NEXT_ROM_SEG
	add	ax,dx
;
;  Check for precharge of low
;
	cmp	dword ptr ds:[4],0
	jne	short CPBcheckFF
	cmp	dword ptr ds:[8],0
	jne	short CPBROMdetected
	cmp	dword ptr ds:[12],0
	jne	short CPBROMdetected
	jmp	short CPBFloatingBus
;
;  Check for precharge of high
;
CPBcheckFF:
	cmp	dword ptr ds:[4],-1
	jne	short CPBROMdetected
	cmp	dword ptr ds:[8],-1
	jne	short CPBROMdetected
	cmp	dword ptr ds:[12],-1
	je	short CPBFloatingBus
;
;  ROM detected
;
CPBROMdetected:
	stc
	jmp	short CPBexit

;
;  Floating Bus was detected (no ROM)
;
CPBFloatingBus:
	clc

CPBexit:
	pop	ds
	ret
CheckPrechargeBus	endp

endif

;******************************************************************************
; CheckForROMHeader
;
; ENTRY
;	AX = Segment address of ROM.
; EXIT
;	CY = Found a VDU ROM at this location.
;	NC = Did not find a valid ROM at this location.
;	AX = Segment address of NEXT ROM location.
;	DX = Length of this ROM in paragraphs
;
; DESCRIPTION
;	This routine looks at the ROM located at the segment address
;	specified in AX to see if 0TH and 1ST Bytes = 0AA55H.
;	If so, it calculates the checksum over the length of
;	ROM.  If the checksum is valid it updates AX to point
;	to the location of the next ROM.
;
;	For option ROMs, the layout of each valid ROM is as follows:
;
;	 OFFSET +-----------------------+
;	    0	|	   55h		|
;		+-----------------------+
;	    1	|	   AAh		|
;		+-----------------------+
;	    2	|    ROM size / 512	|
;		+-----------------------+
;	    3	|  Start of init code	|
;			    :
;	   n-1	|			|
;		+-----------------------+
;		(Sum of all bytes MOD 100h is 00h.)
;
;******************************************************************************
CheckForROMHeader proc	near

	push	ds

		; The ROM segment address is loaded into DS and the memory
		; is tested to see if the ROM signature is there.
	mov	ds,ax
	xor	bx,bx
	cmp	[bx].ROM_RECOGNITION,ROM_SIGNATURE
	jne	SHORT CFR_no_ROM_found

		; If a ROM signature is there than compute the ROM's checksum.
		; The size of the ROM is stored as the number of 512 bytes
		; blocks. It is loaded into CX as the number of bytes.
	xor	esi,esi
	xor	ecx,ecx
	mov	ch,[bx].ROM_LEN

	shl	ecx,1

	or	ecx,ecx			;Q: 128K ROM?
	jnz	SHORT CFRcont		; N: continue
	mov	ecx,20000h		; Y: 128K ROM
CFRcont:
	mov	edx,ecx

		; Each byte is loaded from the ROM area and added to the
		; checksum in BL.  At the end of the loop the value in BL
		; should be zero.  If not than this isn't a ROM.
CFR_next_byte:
	lodsb
	add	bl,al
	dec	ecx
	jnz	short CFR_next_byte

	or	bl,bl				;Q: Is this a ROM?
	jnz	short CFR_no_ROM_found		; N: ROM not found

		; If this is reached then the address reflects a ROM.  The
		; size of the ROM in DX is in bytes and is converted into
		; the number of paragraphs.
		; The original ROM address in DS is loaded into AX and
		; incremented to point to the next possible ROM segment.
	shr	edx,4
	mov	ax,ds
	add	dx,NEXT_ROM_SEG - 1	;increment to next
	and	dx,ROUND_BY_2K_SEG	;truncate to 2K boundary
	add	ax,dx
	stc
	jmp	SHORT CFR_return_code

		; If this is not a ROM then the next possible address
		; is changed into a paragraph count.
		; The original ROM address in DS is loaded into AX and
		; incremented to point to the next possible ROM segment.
CFR_no_ROM_found:
	mov	dx,NEXT_ROM_SEG
	mov	ax,ds
	add	ax,dx
	clc

CFR_return_code:

	pop	ds
	ret
CheckForROMHeader endp

;******************************************************************************
; CheckForCompaqROM
;
; ENTRY
;	none
; EXIT
;	CY - If set then the area is not Compaq's VIDEO ROM and can not be used.
;		But, it must be removed from the window map
;			- AX = next ROM segment
;			- DX = ROM length in paragraphs
; DESCRIPTION
;
;******************************************************************************
CheckForCompaqROM	proc	near

		; The entrance registers are saved.
	push	ds
	push	es

ifdef E000
		; Can't be Compaq's ROM if not a Compaq system
	call	IsCompaq
	jnz	short CFCR_not_Compaqs_ROM
endif

		; The data in the ROM area is compared to the data at
		; the default VIDEO rom area.
		; DS:SI is the default location for COMPAQ's VIDEO ROM.
	push	0C000h
	pop	ds
	xor	esi,esi

		; ES:DI is the ROM location to verfify
	push	REMAP_VIDEO_ROM_SEGMENT
	pop	es
	xor	di,di

		; The data in the two ROM areas are now checked to see if they
		; are the same. DX is the ROM length in paragraphs.
	mov	cx,dx
	shl	cx,2
	repe	cmpsd
	jne	short CFCR_not_Compaqs_ROM

		; If the ROM data matches then now the VIDEO ROM location
		; interrupt vector is checked to see if it was remapped.
	xor	esi,esi
	mov	ds,si
	ASSUME	DS:ABS0
	cmp	[int10+2],REMAP_VIDEO_ROM_SEGMENT
	jne	short CFCR_not_Compaqs_ROM

		; No one has hooked the int 10 vector. Now check to make 
		; sure the offset of the int 10 vector lies within the ROM 
		; length specifed in the header. This is an attempt to
		; prevent us from reclaiming the shadow on ROMs that do not
		; specify their length correctly.
		; 
	mov	cx, dx	   	; dx has length in paras
	shl	cx, 4		; cx has length in bytes
	cmp	cx, [int10]	; Q: is the ROM lentgh <= the offset of int10
	jbe	short CFCR_not_Compaqs_ROM
				; Y: do not recalim shadow

				
		; If this is reached then the ROM appears to be Compaq's
		; VIDEO ROM remapped so set the appropriate state.
	push	R_CODE
	pop	ds
	assume	ds:R_CODE
	or	[Current_State],fState_CEGAinst
	mov	[CROM_Length],dx
	clc
	jmp	SHORT CFCR_return_code

CFCR_not_Compaqs_ROM:
	stc

CFCR_return_code:
	pop	es
	pop	ds
	ASSUME	ds:_DATA

	ret
CheckForCompaqROM	endp

;******************************************************************************
; CheckForMappedRAM
;
; ENTRY
;	AX = Segment address for RAM search.
; EXIT
;	CY = Found RAM at this location.
;	NC = Did not find RAM at this location.
;	AX = Segment address of next RAM location.
;	DX = Length of this RAM
;
; DESCRIPTION
;	This routine looks at the address range potentially used
;	by the Page Frame to determine if any RAM is in the way.
;	It updates the map accordingly.
;******************************************************************************
CheckForMappedRAM	proc	near
	push	cx
	push	ds
	pushf
	cli				;;; clear ints during this test

;  search for RAM
;
	xor	dx,dx					; length = 0
ram_loop:
	mov	ds,ax
	add	ax,NEXT_ROM_SEG		;     prepare for next chunk

;
;  Do not search if the area has been eXcluded/Included/EMS/RAM/ROM by the user
;
	mov	bx,ds
	shr	bx,8			; index into 4K page
	cmp	cs:[Page4K][bx],0	;Q: Did user specify this page excluded/EMS/RAM?
	jnz	short no_more_ram	; Y: don't feel for RAM!

					; M002 - Start
;;;	mov	bx,ds:0A0H		; get current word
;;;	push	bx			; save it
;;;	mov	cx,ds:0A2H		; get following word
;;;	push	cx			; save it
;;;	not	bx

	mov	bx, ds:0A2H		; save word at ds:A2H
	mov	cx, ds:0A0H

	; 
	; Let us ensure that the value we're going to write out to the bus 
	; is not going to contain FCH as we're going to preset the bus to 
	; 0FCH before reading it back.
	;
	cmp	cx, 0303h		; Q: is neg cx = FCFCH
	jne	@f			; N: fine
	inc	cx			; Y: change cx to some other value
@@:

	not	cx			; neg word at ds:A0H
	push	cx			; save it for future comparison
	xchg	ds:0A0H, cx		; write this negated word to ds:A0
					; and save ds:A0h in cx

;;;	mov	word ptr ds:0A0H,bx	; try to clear word

					; M002 - End

	mov	word ptr ds:0A2H,0FFFFh ; charge up data bus
;
;  the instructions following and preceeding the xchg... below
;  are carefully chosen to not contain the value in cx as part of the 
;  instruction.
;
	cld				;;; preset the bus to 0FCh
	cld				;;; preset the bus to 0FCh
	cld				;;; preset the bus to 0FCh
	cld				;;; preset the bus to 0FCh

;;;	mov	cx,DS:0A0H		; M002
	xchg	ds:0A0H, cx		; M002: restore word at ds:A0H and get
					; M002: current value in cx

	cld				;;; preset the bus to 0FCh
	cld				;;; preset the bus to 0FCh
	cld				;;; preset the bus to 0FCh
	cld				;;; preset the bus to 0FCh
	cld				;;; preset the bus to 0FCh
	cld				;;; preset the bus to 0FCh
	cld				;;; preset the bus to 0FCh
	cld				;;; preset the bus to 0FCh
	cld				;;; preset the bus to 0FCh
	cld				;;; preset the bus to 0FCh
	cld				;;; preset the bus to 0FCh
	cld				;;; preset the bus to 0FCh

					; M002 - Start
	mov	ds:0A2H,bx		; restore to it's original value

	pop	bx			; get value that was written to 
					; ds:A0h in bx
	cmp	bx, cx			; Q: was it really written out
	jne	short no_more_ram	;  N: leave
	
;;;	cmp	bx,cx			;Q: RAM here ?
;;;	pop	bx			; restore following word
;;;	mov	ds:0A2H,bx		; to it's original value
;;;	pop	bx			; restore test word
;;;	mov	ds:0A0H,bx		; to it's original value
;;;	jne	short no_more_ram	;  N: leave
					; M002 - End

					;  Y: RAM - keep looking
	add	dx,NEXT_ROM_SEG		;     increment length count
	cmp	ax,LAST_RAM_SEGMENT	;Q: last RAM location ?
	jbe	short ram_loop		;  N: continue searching
	mov	ds,ax			;  Y: no more searching
no_more_ram:
;
	mov	ax,ds			;  get current segment
	or	dx,dx			;Q: any RAM found ?
	jnz	short ram_found		;  Y: set RAM found & chk DS seg again
					;  N:
	add	ax,NEXT_ROM_SEG		; AX -> next one to check
	popf
	clc				; set no RAM
	jmp	short ram_exit		;     and leave
;
ram_found:
	popf
	stc
ram_exit:
	pop	ds
	pop	cx
	ASSUME	ds:_DATA
	ret
CheckForMappedRAM	endp

;---------------------------------------------------------------------------
;
; ExcludePS2Options - Find PS/2 Option addresses
;
;   ExcludePS2Options(EXPS2PDevTab,EXPS2PIFile)
;
; ENTRY:
;	Know we are on a PS/2 system
;
; EXIT:
;	AX == 0
;	    ERROR (usually INVALID POSCardTable (bad TabelRev))
;
;	AX != 0
;
; USES:	EAX,ECX,EDX
;
; NOTE: Ported from win386 3.0 sources.
;
;----------------------------------------------------------------------------

ExcludePS2Options	proc	near

	enter	8,0

EXPS2TotOptions 	equ  dword ptr [ebp-4]
EXPS2Flags		equ  dword ptr [ebp-8]

EXPS2F_FndOpt		equ  00000000000000000000000000000001b
EXPS2F_FndOptBit	equ  0

	push	ds
	push	esi
	push	edi
	push	ebx

	mov	ax, cs
	mov	ds, ax
	lea	esi, PS2DATA		; ds:esi -> PS2DATA

	xor	eax,eax
	mov	EXPS2Flags,eax
	cld
	lodsw				; Get TotalOptions
.erre	TotalOptions EQ 0
	mov	EXPS2TotOptions, eax
	lodsw				; Get table revision
.erre	TabelRev EQ 2
	cmp	ax,CUR_PS2_TABLE_REV
	jne	Bad_Rev

	xor	eax, eax		; Card number
Card_Loop:
	push	eax

	or	al, ADAPTER_ENB
	cli
	out	ADAPTER_POS, al		; Enable Card

	jmp	$+2
	jmp	$+2

	mov	edx, ID_ADDR_LO 	; Get Card ID
	in	al, dx

	jmp	$+2
	jmp	$+2

	mov	ah, al
	inc	edx
	in	al, dx
	xchg	ah, al			; ID now in ax

	btr	EXPS2Flags,EXPS2F_FndOptBit
	mov	ecx, EXPS2TotOptions
	push	esi			; Save ptr to Option Tables

Option_Loop:
	cmp	ax, [esi.OptID] 	; This card?
	je	short Found_Option
Continue_Scan:
	movzx	ebx, [esi.LookupCnt]	; # Entries in table to skip
	lea	esi, [esi.LookupTab]	; Point to table
	shl	ebx, 1			; Two bytes per entry
.erre (SIZE MemAddr) EQ 2
	add	esi, ebx		; Skip the table
	loop	Option_Loop

IFDEF DEBUG
	cmp	ax,0FFFFh
	je	short No_Adap
	or	ax,ax
	jz	short No_Adap
	bt	EXPS2Flags,EXPS2F_FndOptBit
	jc	short No_Adap

;;	trace_out "No entry in WIN386.PS2 for adapter #AX found in slot # ",NO_EOL

	pop	esi
	pop	eax
	push	eax
	push	esi
	inc	eax

;;	trace_out "#AX"

No_Adap:
ENDIF


End_Option_Loop:
	pop	esi
	mov	al, ADAPTER_DSB
	out	ADAPTER_POS, al		; Disable setup
	sti
	pop	eax			; Card number
	inc	eax
	cmp	eax, MAX_CARDS		; Done all cards?
	jb	short Card_Loop 	; On to the next one
	jmp	OK_exit

Found_Option:
	bts	EXPS2Flags,EXPS2F_FndOptBit
	push	eax
	push	ecx
	push	esi
	add	esi, 2			; Toss ID word
.erre OptID    EQ 0
.erre POS2Mask EQ (OptID + 2)
	mov	edi, 4			; Number of POS bytes
	mov	edx, ID_ADDR+2		; First POS byte
	xor	ebx, ebx		; Table index
POS_Loop:
	xor	eax,eax
	cld
	lodsw				; Get mask and count
.erre POS2Shft EQ (POS2Mask + 1)
.erre POS3Shft EQ (POS3Mask + 1)
.erre POS4Shft EQ (POS4Mask + 1)
.erre POS5Shft EQ (POS5Mask + 1)
	mov	ecx, eax
	xchg	ch, cl			; cl gets shift count
	or	ah, al
	jz	short Ignore_Byte	; Stuff this one
	in	al, dx			; Get POS byte
	and	al, ch			; Mask it
	ror	al, cl			; Shift it
	or	bl, al			; Or into index
Ignore_Byte:
	inc	edx			; Next POS byte
	dec	edi
	jnz	short POS_Loop

	cld
	xor	eax,eax
	lodsw				; # entries in table
.erre LookUpCnt EQ (POS5Shft + 1)
.erre LookUpTab EQ (LookUpCnt + 2)
	cmp	ebx, eax		; Index in range?
	jae	short ContinueJ 	; No, ignore

	shl	ebx, 1			; Offset in table
.erre (SIZE MemAddr) EQ 2
	movzx	eax, word ptr [esi+ebx] ; Get entry
.erre PGLen EQ (StartPg + 1)
	xor	ecx, ecx
	mov	cl, ah			; Number of 4k pages here
	xor	ah, ah
    ;
    ; EAX is starting 4k page number
    ; ECX is a count of 4K pages to be marked as excluded starting at EAX
    ;
    ; Note that we know start page # is <= 0FFh since its value is stored
    ; in a byte
    ;
	jecxz	ContinueJ		; Nothing to do.......
	mov	edx,eax
	add	edx,ecx
	dec	edx			; Last page of range
if 0
	cmp	eax,[End_VM_Page]
	jae	short EPS2O10
	debug_out "Exclude start page out of range #eax ExcludePS2Options"
	mov	eax,[End_VM_Page]	; Move start up
EPS2O10:
endif

	cmp	edx,100h
	jb	short EPS2O20
					; Exclude count out of range. 
	mov	edx,0FFh		; Move end down
EPS2O20:

	sub	edx,eax
	jc	short ContinueJ
	inc	edx			; Corrected count
	mov	ecx,edx

	lea	edi, page4k

SetExLoop:
	or	byte ptr [edi][eax], INUSE
	inc	eax
	loop	SetExLoop

ContinueJ:
	pop	esi
	pop	ecx
	pop	eax
	jmp	Continue_Scan

Bad_Rev:
	xor	eax, eax		; Bad PS/2 table rev
	jmp	short Bye

OK_Exit:
	mov	eax, 1
Bye:
	pop	ebx
	pop	edi
	pop	esi
	pop	ds
	leave
	ret

ExcludePS2Options	endp

;**
;
; ExcludeEISAOptions - Find EISA Option addresses
;
;    ExcludeEISAOptions
;
; ENTRY:
;	Know we are on an EISA system
;
; EXIT:
;	page4k is initialized with the memory locations of the EISA expansion
;	boards
;
;	AX == 0
;	    ERROR
;
;	AX != 0
;
; USES:	EAX, ECX
;	
;
ExcludeEISAOptions	proc	near

	push	ds
	push	esi
	push	edi
	push	ebx

	mov	ax, cs
	mov	ds, ax			; DS -> LAST

	mov	cl, 1			; start slot #
EISASlotLp:
	push	cx
	mov	ax, 0D800h
	int	15h			; read slot
	pop	cx

	jc	ChkErr

	test	dl, GSI_HasMemory	; Q: does this slot have a mem entry
	jz	SkipSlot		; N: skip this slot and try next.

	;
	; This slot has one or more memory entries. We need to execute the
	; 'Read Fucntion Configuration Information' to retrieve the data 
	; block for each function. Note the number of expansion board 
	; functions was returned in DH by the previous int 15 AX = D800h.
	;

	mov	ch, dh			; ch = funtion number
NextFunc:
	xor	esi, esi
	mov	ax, 0D801h		; Read Fucntion Config. Information
	mov	si, OFFSET LAST:EISAdata
	int	15h

	jc	SkipFunc		; unexpected error. Try next func.

	test	[esi.GSI_FuncInfo],GSI_FI_IsMem
					; Q: memory info follows?
	jz	SkipFunc		; N: Try next func.

	test	[esi.GSI_FuncInfo],GSI_FI_IsCFGFreeFrm
					; Q: is slot info Free Form 
	jnz	SkipFunc		; Y: try next func

	lea	esi,[esi.GSI_Mem_Info]
	push	ecx
	mov	ecx, MAX_MEM_ENTS
EISAMemLp:
	mov	eax,dword ptr [esi.GSI_Mem_Addr256]
	and	eax,00FFFFFFh
	movzx	ebx,[esi.GSI_Mem_SizeK]
	shl	eax,8			; Byte addr of start
	shl	ebx,10			; Size in bytes
	add	ebx,eax
	dec	ebx			; Last byte of range
	shr	eax,12			; Page # of start
	shr	ebx,12			; Page # of last page of range
	cmp	eax,00000100h		; Start >= page 100?
	jae	short NextMem		; Yes, not interesting
	cmp	ebx,0A0h		; End Below lowest page?

	jb	short NextMem		; Yes, not interesting
	
	;
	; Range is at least partly in the page [End_VM_Page]-FF  region
	;

	cmp	eax,0A0h
	jae	short NoStartMov
	mov	eax,0A0h		; Move start up to A0

NoStartMov:
	cmp	ebx,000000FFh
	jbe	short NoEndMov
	mov	ebx,000000FFh		; Move end down
NoEndMov:
	sub	ebx,eax
	jc	short NextMem		; Range underflow
	inc	ebx

	;
	; Exclude EBX 4K pages starting at page EAX
	;

	push	ecx
	lea	edi, page4k
	mov	ecx,ebx

SetExLoopEISA:
	or	byte ptr [edi][eax], INUSE
	inc	eax
	loop	SetExLoopEISA
	pop	ecx

NextMem:
	test	[esi.GSI_Mem_Conf], GSI_Mem_Cnf_More
	jz	short SkipFuncP
	add	esi,SIZE GSI_Mem_Ent
	dec	ecx
	jnz	EISAMemLp
	pop	ecx			; Hit MAX_MEM_ENTS without seeing 
					; Last mem ent slot #CL 
	jmp	short SkipFunc

ChkErr:
	mov	al, ah
	cmp	al,EISA_Err_EmptySlot
	je	short SkipSlot
	cmp	al,EISA_Err_InvalSlot
	je	short EISAEX_Exit
;;	debug_out "Unexpected get SHORT slot info error #AL on slot #CL ExcludeEISAOptions"
	jmp	short EISAEX_Fail

SkipFuncP:
	pop	ecx   		; restore slot and function.

SkipFunc:
	dec	ch		; Next lower function
	jnl	NextFunc	; valid functions are 0..n

SkipSlot:
	inc	cl
	jmp	EISASlotLp

EISAEX_Exit:
	mov	eax, 1
EISAEX_Done:
	pop	ebx
	pop	edi
	pop	esi
	pop	ds
	ret

EISAEX_Fail:
	xor	eax,eax
	jmp	short EISAEX_Done

ExcludeEISAOptions	endp

;========================================================================
;
; 	Procedure	: LastChanceScan
;
;	Inputs		: edx = 4k page to scan
;	Output		: update page4k array appropriately
;
;	Notes:
;		This is called with edx ranging from c0 to ef from the
;	routine findwindowlocations.
;
;=========================================================================
	public	LastChanceScan

LastChanceScan	proc	near

	push	ds
	push	es
	push	bp
	sub	sp, 256/8
	mov	bp, sp

	call	IsHighSetPage		; Included by HighScan code?
	jc	LCS_Exit		; Yes, don't check it again

	cmp	cs:[page4K][edx], INUSE ; Already think it's a ROM?
	je	LCS_Think_Its_A_ROM	; Yes, check for special ROMS

	cmp	cs:page4k[edx],0      	; Anybody know about this page?
	jne	LCS_Exit		; Yes, skip

	pushad
	cld

	mov	di, ss
	mov	es, di			; es -> stack
	or	eax, NOT 0
	mov	cx, 256/32
	mov	di, bp
	rep	stosd			; Zero all bit values

	mov	di, cs
	mov	es, di			; es -> LAST
	lea	di, cs:LastChanceBuf	; Caller knows this is valid

;;	mov	esi, edx
;;	shl	esi, 12 		; ESI -> Lin mem of page
	shl	edx, 8
	mov	ds, dx			; ds = 4K page seg addr
	xor	si, si
	mov	cx, 1000h/4
	rep movsd			; Copy from suspect to buffer

	sub	si, 2			; Back up source 1 word
	sub	di, 4			; Back up dest 1 DWORD
	std				; BACKWARDS!
	mov	cx, 1000h/4		; Number of dwords to compare
	xor	edx, edx		; 0 different bytes detected
	mov	eax, DWORD PTR [si-2]	; Initialize the xor value

LCS_Scan_Backwards:
;
;   Prime the bus with the inverse of the previous value to try to detect
;   bus noise.
;
	xor	eax, -1 		; Try to make bus noise
	push	eax			; Put this value on the bus
	pop	eax			; Read it back for fun

	lodsw				; Get next word in table
	shl	eax, 16
	lodsw				; Get 2 words in 2 fetches
	cmp	eax, DWORD PTR es:[di]	; Q: Did it match buffer val?
	jne	LCS_Is_A_Hole		;    N: BUS NOISE!  It's a hole
					;    Y: It's consistant
	movzx	ebx, al
	btr	[bp], ebx
	adc	edx, 0
	movzx	ebx, ah
	btr	[bp], ebx
	adc	edx, 0
	ror	eax, 16
	movzx	ebx, al
	btr	[bp], ebx
	adc	edx, 0
	movzx	ebx, ah
	btr	[bp], ebx
	adc	edx, 0
	ror	eax, 16

LCS_Try_Next_Dword:
	sub	di, 4				; Subtract word from buf ptr
	loop	LCS_Scan_Backwards		; Scan the entire page

	cld					; Get things fwd again!

	cmp	edx, ROMChangeCount
	jb	SHORT LCS_Think_Its_A_Hole

;------------------------------------------------------------------------------
;
;   We think this is a ROM -- UGLY SPECIAL CASE CODE TIME!
;
;------------------------------------------------------------------------------

	popad					; Get all registers back

LCS_Think_Its_A_ROM:

	pushad					; And save them again

;
;   On Northgate machines, pages E0h-EFh are a duplication of the ROM at
;   F000h.  If the page at Exxx exactly matches the page at Fxxx then we WON'T
;   mark this as a ROM page.
;
	cmp	edx, 0E0h
	jb	SHORT LCS_Not_Special_1
	cmp	edx, 0EFh
	ja	SHORT LCS_Not_Special_1

;;	TestMem <[ebx+(10h*SIZE VM_Page_Struc).V86_Flags]>, V86_ROMPage
;;	jz	SHORT LCS_Not_Special_1

;;	cmp	cs:page4k[edx+10h], INUSE	; Is this ROM
;;	jne	SHORT LCS_Not_Special_1 	; No


	; suspect page may or may not have copied above--do it now to make sure

	mov	di, cs
	mov	es, di			; es -> LAST
	lea	di, cs:LastChanceBuf	; Caller knows this is valid

	mov	si, dx
	shl	si, 8
	mov	ds, si			; ds = 4K page seg addr
	xor	si, si
	mov	cx, 1000h/4
	rep movsd			; Copy from suspect to buffer

	lea	di, cs:LastChanceBuf		; Caller knows this is valid
	lea	si, [edx+10h]			; Page # + 10h for F000 range
;;	shl	esi, 12 			; Convert to lin address
	shl	si, 8
	mov	ds, si
	xor	si, si
	mov	cx, 1000h/4
	cld
	repe cmpsd				; See if they are the same

IFDEF DEBUG
	jne	SHORT LCS_Not_Special_1
	Trace_Out "LCS Special case # 1 on page #DL -- NOT excluded"
	jmp	SHORT LCS_Is_A_Hole
ELSE
	je	SHORT LCS_Is_A_Hole
ENDIF

LCS_Not_Special_1:

LCS_Not_A_Hole:
	popad
;;	Trace_Out "Last-chance detection found ROM at page #DL -- TELL RALPHL"
;;	SetFlag [ebx.V86_Flags], (V86_ROMPage+V86_LastChance)
	or	cs:[Page4K][edx],INUSE	; mark it used
LCS_Exit:
	add	sp, 256/8
	pop	bp
	cld
	pop	es
	pop	ds
	ret


;------------------------------------------------------------------------------
;
;   At this point, we got no bus noise and think it probably is a hole
;   because the change count was so low.  If the count is actually 0 then
;   we are POSITIVE that it is a hole or at least a *VERY* odd ROM so
;   we'll skip all special cases
;
;   DX at this point is the change count.  If it isn't 0 then we'll nuke
;   it since we don't really care -- The rest of this code handles special
;   cases.
;
;------------------------------------------------------------------------------

LCS_Think_Its_A_Hole:
	test	dx, dx
	jz	SHORT LCS_Is_A_Hole

	popad
	pushad

;
;   Special case for ROMs without signatures in the E000-EFFF range that have
;   only a TINY amount of useful stuff in the last few pages.  Starting with
;   ED00-EF00h we will exclude this page if page EC00h was excluded too.
;
;   This is a special case for the IBM LX 40 portable
;
	cmp	edx, 0EDh
	jb	SHORT LCS_Is_A_Hole
	cmp	edx, 0EFh
	ja	SHORT LCS_Is_A_Hole

;;	TestMem <VMPagesArr[0ECh*SIZE VM_Page_Struc].V86_Flags>, V86_ROMPage
	test	cs:[page4K][0ECH], INUSE
IFDEF DEBUG
	jz	SHORT LCS_Is_A_Hole
	Trace_Out "LCS special case include # 1 for page #DL"
	jmp	LCS_Not_A_Hole
ELSE
	jnz	SHORT LCS_Not_A_Hole
ENDIF

;
;   At this point we are as sure as we are ever going to be that this page is
;   really a hole.
;
LCS_Is_A_Hole:
	popad
	and	byte ptr cs:[Page4K][edx], NOT INUSE
	jmp	LCS_Exit

LastChanceScan	endp

;============================================================================
;==
;== IsHighSetPage:  Determines if page is covered by HighScan set range.
;==
;== Enter:
;==   edx = page number
;==
;== Exit:
;==   CY if page covered by HighSet range
;==
;============================================================================
	public	IsHighSetPage

IsHighSetPage proc near

	push	ds
	push	si

	push	cs
	pop	ds
	lea	si, cs:[HighSet+2]

IHSP_next:
	lodsw					; range start
	or	ax, ax				; Q: end of set?
	jz	short IHSP_nope

	shr	ax, 8				; paragraph to 4k page
	cmp	ax, dx				; Q: below the target page?
	ja	short IHSP_not_this_one 	; N: can't be in this range

	lodsw					; end of range
	shr	ax, 8
	cmp	ax, dx				; Q: => target page?
	jb	short IHSP_next 		; N: try next range

	stc					; Y: set CY flag & exit
	jmp	short IHSP_exit

IHSP_not_this_one:
	lodsw					; skip end of range
	jmp	short IHSP_next 		; and try the next one

IHSP_nope:
	clc					; page not in range, clr CY

IHSP_exit:
	pop	si
	pop	ds
	ret

IsHighSetPage endp


;----------------------------------------------------------------------------
;
; Procedure Name : IsCompaq
;
;	Output : Z  if COMPAQ
;		 NZ if not
;
;  Check to see if we're on a Compaq 386 machine
;
;----------------------------------------------------------------------------
IsCompaq 	proc near

	push	es
	push	ds
	push	di
	push	si
	push	cx

	les	di,cs:[pCOMPAQ]		; es:di points to possible COMPAQ signature
	push	cs
	pop	ds
	mov	si,offset szCOMPAQ 	; "03COMPAQ"
	mov	cx,8
	cld
	rep	cmpsb			;Q: COMPAQ 386 machine?

	pop	cx
	pop	si
	pop	di
	pop	ds
	pop	es

	ret

IsCompaq	endp

;  Added following procedures to check for EMM page & UMB conflicts
;  with Toshiba reserved areas (resume and HardRAM).
;  If conflicts found because of user-specified page frame base, set
;  invalid page frame base error.

TOSH_MID_SEG	equ	0F000H	; Toshiba machine ID byte segment
TOSH_MID_OFF	equ	0FFFAH	; Toshiba machine ID byte offset

TOSH_T5100	equ	027H	; Toshiba T5100 machine ID byte

;----------------------------------------------------------------------------
;
; Procedure Name : Toshiba_exclude
;
;	Output : DfltSet updated to reflect excluded areas for
;                Toshiba resume and HardRAM machines.
;
;  Check for resume & HardRAM machines.
;
;  Note: this routine should be incorporated into SetToshibaOptions in
;  init.asm (or that code should be moved here).  It wasn't done initially
;  since this code was cut and pasted while building release canidates for
;  MS-DOS 6 and it works in it's current form.
;
;----------------------------------------------------------------------------
Toshiba_exclude	proc near					

	cmp	toshiba_machine, 0	; On a Toshiba system?
	je	te_exit 		;   no, skip this code

;	Check for T5100 machine w/FAST ROM enabled.  If found, exclude
;	addresses E000-F000 from being used as EMS pages or UMBs.

	push	es
	push	ax
	push	dx
	mov	ax,TOSH_MID_SEG
	mov	es,ax			;   get segment containing Toshiba machine ID
	cmp	byte ptr es:[TOSH_MID_OFF],TOSH_T5100	;Q: Is this a T5100?
	jne	short Tosh_exc_xit	;N: exit

	mov	dx,8080h		;   I/O Port '8080' = Memory Map Reg
	in	al,dx			;   Read Memory Map Register Fast ROM bits
	test	al,04h			;Q: Is Fast ROM enabled for E000-F000
	jz      short Tosh_exc_xit	;Y: Exclude E000-F000, w/remapped AGS BIOS

excl_E000:

;	Now specify T5100 machine special exclude areas in
;	Toshiba misc. exclude section.	Support for T3300SL.

	mov	cs:[TOSHbeg],0E000H	;Y: E000-F000 must be excluded from EMM use
	mov	cs:[TOSHend],0EFFFH	;Y: E000-F000 must be excluded from EMM use

Tosh_exc_xit:							
	pop	dx
	pop	ax
	pop	es							
te_exit:
	ret							

Toshiba_exclude	endp						

;----------------------------------------------------------------------------
;
; Procedure Name : ToshInvPFbase
;
;	Output : PF_Base set to FREE, and invalid parameter msg
;                set if invalid page frame base error.
;                
;
;  Check for User-specifed page frame base conflict with
;  resume & HardRAM machines.
;
;----------------------------------------------------------------------------

ToshInvPFbase	proc near					

	cmp	toshiba_machine, 0	; On a Toshiba system?
	je	ToshInv_xit		;   no, skip this code

	cmp	gs:[PF_Base],FREE	;Q: Has the page frame base address been set?
	je	short ToshInv_xit	;N: No need to check for bad page frame base
	cmp	gs:[PF_Base],0D400h	;Y: Does user specified base cross E000?
	jl	short ToshInv_xit	;exit if no conflict with E000-F000 area
	mov	ax,gs:[PF_base]					
	mov	cx,4	       					
PF_Base_loop:  				;check each page for potential conflict
	xor	bx,bx						
	call	CheckEMSWindow	   				
	and	bx,INUSE	   				
	jnz	short Inv_PF_base	;if page reserved, flag as invalid parameter
	add	ax,400h			;bump to next page
	loop	PF_Base_loop	   				
	jmp	short ToshInv_xit   				

Inv_PF_base:			   				
	mov	gs:[PF_Base],FREE   				
	or	gs:[msg_flag],INV_PARM_MSG			
ToshInv_xit:							
	ret							

ToshInvPFbase	endp						

LAST	ENDS
	END
