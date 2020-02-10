page 58,132
;******************************************************************************
	title	MEMM386 - main module for EMM386
;******************************************************************************
;
;   (C) Copyright MICROSOFT Corp. 1986-1991
;   (C) Copyright COMPAQ Computer Corp. 1986-1991
;
;	Title:	EMM386.EXE - MICROSOFT Expanded Memory Manager 386 Driver
;
;	Module: MEMM386 - main module
;
;	Version: 2.02
;
;	Date:	May 24,1986
;
;	Author: Steve Preston
;
;******************************************************************************
;
;	Change Log:
;
;	DATE	 REVISION	Description
;	-------- --------	--------------------------------------------
;	04/24/86 Original	From EMML LIM driver.
;	06/26/86 0.02		Put CLD in Inst_Chk (SBP).
;	06/28/86 0.02		Name change from CEMM386 to CEMM.
;	07/05/86 0.04		Changed segment to R_CODE (SBP).
;	07/27/87 2.02		Updated comments to include Weitek support
;				and page frames M6 - M14 (SBP).
;	03/12/88 3.30 (*B)	Convert to packed version w/unpack exec (RDV).
;				and no unpack when old DOS < 3.10
;	07/14/88 3.31 (*C)	Change DOS version check for >3.1 and <4.99
;				Change Inst_chk to use EMS device open (RDV).
;				Add banner and copyright message display (RDV).
;				Add call to chka20 & disabla20 (EXEPACK) (RDV).
;				Add chk for DOS 3.00 to not use waitkey (RDV).
;	10/12/88 3.32 (*D)	VCPI added (DJM).
;	01/09/88 4.00 (*E)	Version changed to 4.00 (RDV)
;				Abort install if CTRL-ALT-SHFT held down
;	02/12/89 4.00      	Remove VDISK header
;
;	02/13/91 M008		Do not make int 16s on a Compaq Deskpro
;				386/16 or a Compaq portable 386.
;
;******************************************************************************
;   Functional Description:
;	CEMM is an Expanded Memory Manager which implements expanded memory
;   on the COMPAQ Lightning machine.  CEMM uses Virtual mode and paging on
;   the 386 to make Extended memory useable as expanded memory.  The are two
;   basic functional parts of CEMM;  the Virtual DOS Monitor (VDM) and the
;   Expanded Memory Manager (EMM).  VDM simulates the 386 Real mode under the
;   386 Virtual mode.  EMM provides the software functionality for a EMM as
;   described in the Lotus-Intel-Microsoft (LIM) specification for expanded
;   memory.  EMM also emulates the necessary expanded memory hardware.
;   CEMM also provides support for the Weitek coprocessor when the Weitek
;   is installed.
;	This module contains the Device Driver header, stategy, and interrupt
;   routines required by a LIM standard EMM.
;	This device driver is a .EXE file and may be invoked as a DOS utility
;   program as well as loaded as a device driver.  When it is loaded as a
;   DOS utility, CEMM has three command line options: ON,OFF and AUTO.
;   The OFF options disables CEMM and exits to MS-DOS in real mode.
;   The ON option enables CEMM and exits to MS-DOS in virtual mode (only
;   if the CEMM.EXE driver has been loaded).  The AUTO option puts
;   CEMM in "auto mode".  In this mode, CEMM will enable and disable
;   itself automatically, depending on accesses to the EMM functions.
;	The general device driver CONFIG.SYS options are described below.
;
;    Syntax:
;
;		device=[d]:[<path>]CEMM.EXE [SIZE] [Mx] [ON | OFF | AUTO]
;						[W=ON | W=OFF]
;						[Iz] [Iz] [Iz] [Iz]
;
;    The following sections describe the optional arguments which the
;    user may specify for CEMM.EXE at load time (in the CONFIG.SYS
;    file).  These arguments are placed after the device driver name
;    in the CONFIG.SYS file.
;
;    CEMM arguments in the CONFIG.SYS file must be separated by spaces
;    or tabs.  Arguments may appear in any order; however, any redundant
;    or excessive instances are ignored and only the first valid instance
;    of an argument is used.  Invalid or extraneous arguments produce an
;    error message.
;
;    [SIZE]
;
;    The argument SIZE is the amount of expanded memory desired in
;    K bytes.  The default amount of expanded memory, 256K, is available
;    without using any extended memory.  To use more than 256K of
;    expanded memory, the 386 system must have extended memory.  When
;    CEMM uses extended memory it may reserve 4K for itself.  Thus, if
;    512K of expanded memory is requested, CEMM may actually use 260k
;    (256K + 4K) of extended memory.  If there is not enough memory available
;    to provide SIZE kbytes of expanded memory, CEMM will adjust SIZE to
;    provide as much expanded memory as possible.
;
;	- The valid range for SIZE is 16K - 8192K.  Value outside this range
;	  are converted to the default of 256K.
;
;	- If SIZE is not a multiple of 16K (size of an EMM page), then SIZE
;	  is rounded down to the nearest multiple of 16K.
;
;    [Mx]
;
;    The argument [Mx] specifies the address of the 64k EMM page frame.
;    This argument is optional since CEMM can choose an appropriate
;    location for the page frame when it is loaded. To choose a location
;    the 386 EMM driver scans memory addresses above video memory for
;    an appropriate 64K address range for the EMM page frame.  For a
;    default page frame base address CEMM looks for option ROMs and
;    RAM in the EMM addressing range and chooses a 64K slot of memory
;    for the page frame which apparently does not confict with existing
;    memory.  The user may override the 386 EMM driver's choice by
;    specifying the beginning address with the Mx argument.  If the
;    user specifies a page frame base address which conflicts with an
;    option ROM or RAM, CEMM displays a warning message and uses the
;    specified page frame base address.
;
;    The following options are possible:
;			Page Frame Base Address
;		M1 => 0C0000 Hex
;		M2 => 0C4000 Hex
;		M3 => 0C8000 Hex
;		M4 => 0CC000 Hex
;		M5 => 0D0000 Hex
;		M6 => 0D4000 Hex
;		M7 => 0D8000 Hex
;		M8 => 0DC000 Hex
;		M9 => 0E0000 Hex
;		M10 => 080000 Hex
;		M11 => 084000 Hex
;		M12 => 088000 Hex
;		M13 => 08C000 Hex
;		M14 => 090000 Hex
;
;    [ ON | OFF | AUTO ]
;
;    The argument [ON | OFF | AUTO] specifies the state of the 386 when
;    CEMM returns to DOS after the driver INIT routine finishes.  If this
;    argument is ON, then CEMM returns to DOS in virtual mode and
;    expanded memory is available.  If this argument is OFF, then CEMM
;    returns to DOS in real mode and expanded memory is not available
;    until CEMM is turned ON.  The default for this argument is AUTO
;    mode.  In AUTO mode, the CEMM.EXE device driver will exit to
;    DOS in the OFF state; afterwards, CEMM will enable and disable
;    itself automatically.   In the AUTO mode, CEMM will be enabled
;    only while the expanded memory manager is in use.
;
;    [W=ON | W=OFF]
;
;    This argument enables or disables the Weitek coprocessor support.
;    The COMPAQ Weitek 1167 Programming Information document describes
;    the MS-DOS real mode interface provided by CEMM.  If no Weitek is
;    installed in the system, these arguments are invalid.
;
;    [Iz]
;
;    The argument [Iz] specifies the I/O addresses for the expanded
;    memory board mapping registers emulated by CEMM.  Since up
;    to four expanded memory boards can be placed in one system,
;    CEMM will emulate up to four sets of mapping registers.
;    The user may specify the mapping register I/O addresses emulated
;    by CEMM by including up to four instances of the argument [Iz].
;    The default I/O addresses are I0, I1, I5, and I6; for the first,
;    second, third, and fourth boards respectively. The following
;    table lists the I/O addresses represented by each possible value
;    for 'z'.
;
;	z | I/O Addresses for Mapping registers (in Hex)
;	------------------------------------------------
;	0 | 0208h   4208h   8208h   C208h
;	1 | 0218h   4218h   8218h   C218h
;	5 | 0258h   4258h   8258h   C258h
;	6 | 0268h   4268h   8268h   C268h
;	A | 02A8h   42A8h   82A8h   C2A8h
;	B | 02B8h   42B8h   82B8h   C2B8h
;	E | 02E8h   42E8h   82E8h   C2E8h
;
;
;******************************************************************************
.lfcond
	page
;******************************************************************************
;			P U B L I C   D E C L A R A T I O N S
;******************************************************************************

;
;   R_CODE publics
;
	public	DEVHEAD 		; Device driver header
	public	DEVHEAD2		; Second device header
	PUBLIC	DDT			; Device driver type
	public	strategy		; normal strategy routine	     *B
	public	interrupt
	public	CEMM_Entry
	public	Devname			;EMS header device name
	public	Cache_Off		; Added for others
	public	wait_key
	public	SIG_LENGTH
;;	public	Dos_version

;
;   LAST publics
;
	public	ELIM_link
	public	ELIM_EXE		; .EXE execution entry point
	public	Inst_chk		; Check to see if CEMM already installed
	public	FarLink
	public	Linkbuf_length


	page
;******************************************************************************
;			L O C A L   C O N S T A N T S
;******************************************************************************
;
.386p
	include vdmseg.inc
	include oemdep.inc
	include driver.equ
	include ascii_sm.equ
	include emmfunct.inc
	include	emmdata.inc
	include emm386.inc
	include	winemm.inc
.8086

MS_DOS		equ	21h			; DOS interrupt
PRINT_STRING	equ	09h			;			     *B
GET_VERSION	equ	30h			;			     *B
GET_PSP 	equ	62h			; get program segment prefix
NULL		EQU		0FFFFH			;Null address pointer

dospsp_str	struc
		db	80h dup (?)
cmd_len 	db	?		; length of command line
cmd_line	db	?		; commande line
dospsp_str	ends

;******************************************************************************
;			E X T E R N A L    R E F E R E N C E S
;******************************************************************************
;
LAST	segment
	extrn	onf_func:near		; perform on/off/auto checking
	extrn	Any_key:byte
	extrn	DOS_version:word
LAST	ends

R_CODE	segment
	extrn	ELIM_Entry:far		; general entry for CEMM functions

	extrn	StrategyEntry:dword
	extrn	InterruptEntry:dword
	extrn	ReqPtr:word


R_CODE	ends

R_CODE	segment
	assume	cs:R_CODE, ds:R_CODE, es:R_CODE, ss:R_CODE

Start:
;******************************************************************************
;  Device driver header
;******************************************************************************
;
DEVHEAD DD		NULL
DDT	DW		CHAR_DEV+IOCTL_SUP	;Attribute - Char
Strato	DW		OFFSET STRATEGY 	;Strategy routine entry      *B
Intero	DW		OFFSET INTERRUPT	;Interrupt routine entry
Devname DB		'EMMXXXX0'              ;Character device name
;
;******************************************************************************
;		GENERAL FUNCTIONS ENTRY POINT
;	R_CODE:ELIM_Entry is a entry point for executing general CEMM
;			functions. (e.g. ON, OFF function).
;******************************************************************************
;
CEMM_Entry	dw	offset	ELIM_Entry		; general entry point

;******************************************************************************
;	       MEMM signature
;******************************************************************************
cemmsig db	'MICROSOFT EXPANDED MEMORY MANAGER 386'
SIG_LENGTH	equ	(this byte - cemmsig)

;;DOS_version	dw	0

;******************************************************************************
;  Second device driver header
;
;  Some programs cannot detect EMM386 when it uses the 'EMMQXXX0' device
;  name (No EMS, but VCPI supported).  To allow these programs to detect
;  EMM386, a second device header is added with the name '$MMXXXX0'.  This
;  header is only used when the primary name is 'EMMQXXX0'.
;
;******************************************************************************

	align	2

DEVHEAD2 DD		NULL
	 DW		CHAR_DEV+IOCTL_SUP	;Attribute - Char
	 DW		OFFSET STRATEGY 	;Strategy routine entry
	 DW		OFFSET INTERRUPT	;Interrupt routine entry
	 DB		'$MMXXXX0'		;Character device name

	page
;******************************************************************************
;			L O C A L   D A T A   A R E A
;******************************************************************************

;*************************************************************************** *B
;	STRATINI/STRATEGY - Initial strategy routine & normal strategy	     *B
;									     *B
;	ENTRY: ES:BX = pointer to Request Header			     *B
;									     *B
;	EXIT: CS:ReqOff, CS:ReqSeg - saved pointer to Request Header	     *B
;	      UNPACK executed if found					     *B
;									     *B
;	USED: 11 stack words						     *B
;									     *B
;	This is the initial STRATEGY routine for first time execution as a   *B
;	device driver. It is replaced with the normal STRATEGY routine below.*B
;	It searches the code segment for the unpack routine appended by the  *B
;	EXEPACK link option or EXEPACK program. If it finds the code it      *B
;	executes it as an EXE program. The original first instruction	     *B
;	pointer is patched to return to this routine before continuing full  *B
;	execution of the program after unpack. The seg regs must be loaded   *B
;	with a fake value. They normally point to the PSP, but there is none.*B
;	Nothing is used from or done to the PSP so it is okay.		     *B
;	Cacheing must be turned off on a 386/20 while unpacking, and this    *B
;	is done by calling the ROM BIOS Int 15h Move Block function.	     *B
;	The UNPACKER expands the image to it's original unpacked state and   *B
;	everything is back to normal. STRATEGY then returns to DOS which     *B
;	calls the INTERRUPT routine initialization code.		     *B
;									     *B
;	This code must be at the beginning of the program so that no packing *B
;	takes place until after it.					     *B
;									     *B
;*************************************************************************** *B

STRATEGY proc	far			 ;find and execute UNPACKER  *B

	jmp	cs:[StrategyEntry]


STRATEGY endp


;***************************************************************************
;	Cache_off - check presense and turn off cache if on
;		  - check A20 status and disable if neeeded
;
;	ENTRY: none
;
;	EXIT: BX = entry status of cache controller
;		   0E201h = on, anything else is off or not present
;	USED: ES, BX, AX
;
;***************************************************************************
Cache_Off proc near				 ;
	push	di			 ;int 16h chgs es:di
	push	es			 ;
	mov	bx,0F000h		 ;look at ROM mem struct
	mov	es,bx			 ;ROM seg

	cmp	word ptr es:[0FFE9h],'C3';Q: ROM ID Compaq 386?
	jne	short Nocache 		 ; N: can't have cache then

	;
	; M008
	;
	; We now test bit 17 of the dword GenFlags under .8086. Hence the
	; .errnz.
	;
	test	word ptr cs:[GenFlags+2], 02h
	.errnz	fCPQ16Bit-17		 ; Q: is this a Compaq deskpro 386/16
	jnz	Nocache			 ; Y: no cache

	mov	ax,0F400h		 ;cache controller status
	int	16h			 ;
	mov	bx,ax			 ;save status
	cmp	ax,0E201h		 ;Q: cache cntrller enabled?
	jne	short Nocache 		 ; N: nothing to do

	mov	ax,0F402h		 ;turn cache controller off
	int	16h			 ;no coherency problem
Nocache:pop	es			 ;
	pop	di			 ;
	ret				 ;returns bx=cache status
Cache_Off endp				 ;

	page
;******************************************************************************
;	Interrupt - device driver interrupt routine for CEMM
;
;	ENTRY: CS:ReqPtr = pointer to request header.
;
;	EXIT: Request completed.
;
;	USED: none
;
;******************************************************************************
Interrupt	proc		far

	jmp	cs:[InterruptEntry]

Interrupt	endp


R_CODE	ends

	page
;******************************************************************************
;
;	LAST Code Segment
;
;******************************************************************************
LAST	segment
	assume	cs:LAST, ds:LAST, es:LAST, ss:LAST

CEMM:
;******************************************************************************
;
;	ELIM_EXE - .EXE entry point - when CEMM.EXE is invoked as a DOS
;		   utility.
;
;******************************************************************************
ELIM_EXE	proc	near
	push	cs
	pop	ds
	assume	ds:LAST
	mov	ah,GET_PSP		; get segment of PSP
	int	MS_DOS
	mov	es,bx			; DOS call returned seg in bx
	mov	di,cmd_line		; es:di = command line               *E
	call	onf_func		; look for on, off, or auto

	mov	ax,4c00h		; exit to DOS
	int	MS_DOS

ELIM_EXE	endp

;******************************************************************************
;	ELIM_link - Link to Installed CEMM's ELIM_Entry
;
;	ENTRY: see ELIM_Entry
;		and
;		LAST:[FarLink] = far address of installed CEMM ELIM_Entry
;
;	EXIT: see ELIM_Entry
;
;	USED: none
;
;******************************************************************************
FarLink dd	0		; far pointer to installed cemm386 entry point
				; OK as writeable because it is only used
				; during .EXE execution.
ELIM_link	proc	near
	call	CS:[FarLink]
	ret
ELIM_link	endp

Linkbuf		dw	0,0,0
Linkbuf_length	equ	6
emmname		db	'EMMXXXX0',0	; std name for EMS device

; OLD INSTALL CHECK SUPPLIMENTED WITH ONE THAT USES EMS DEVICE OPEN AND
; IOCTL COMMANDS TO COMMUNICATE WITH THE RESIDENT COPY OF CEMM.EXE
; **THIS ISN'T INTERFERED WITH BY PROGRAMS THAT STEAL INT 67H**
;****************************************************************************
;	Inst_chk - Check to see if CEMM is already installed
;
;	ENTRY: int 67 vector
;
;	EXIT: al = 0 if not already installed
;		ah = 0 if no int 67h vector is detected (no EMM)
;		ah = 1 if int 67h vector is detected (another EMM)
;	      al = 1 if CEMM is already installed
;		ah = 0 always
;	      	LAST:[FarLink] = far address for installed CEMM entry point
;
;	USED: none
;
;****************************************************************************
Inst_chk	proc	near

	push	di			; save di
	push	si			; and si
	push	ds			; and ds
	push	es			; and es
	push	bx
	push	cx			; and cx
	push	dx

	xor	ax,ax			; put segment 0000h in ds
	mov	ds,ax
	ASSUME	DS:abs0 		; assume ds is abs0
	mov	ax,[int67+2]		; get segment pointed to by int 67
	or	ax,ax
	jz	short Inst_exit		; not install if int 67h is not used
	mov	es,ax
	ASSUME	ES:R_CODE

	mov	ax,seg R_CODE
	mov	ds,ax			; set DS = R_CODE
	assume	DS:R_CODE		; update assume

	mov	di,offset cemmsig	; cemm386 signature
	mov	si,di			; save for source string
	mov	ax,100h			; initialize to not found
	mov	cx,SIG_LENGTH		; length to compare
	cld				;  strings foward
	repe	cmpsb			;q: is the cemm386 signature out there?
	jne	short Inst_chk_again	; n: return zero

	mov	cx,ES:[CEMM_Entry]	; get offset for far call
Inst_okay:
	inc	ax			; y: return one
	mov	word ptr cs:[FarLink+2],es	; set segment of far call
	mov	word ptr cs:[FarLink],cx	; set offset of far call
Inst_exit:
	pop	dx
	pop	cx
	pop	bx
	pop	es
	pop	ds
	pop	si
	pop	di
	ret

Inst_chk_again:			       ; try other method when old fails
	push	cs		       ; ensure addressability
	pop	ds
	assume	ds:LAST
	mov	ax,3D00h	       ; open device - any EMS device
	mov	dx,offset emmname      ; check for device of std name
	int	MS_DOS		       ;
	jc	short Inst_notfound    ; open failed

	mov	bx,ax		       ; open ok, save handle

	mov	ax,4400h	       ; get device info - all EMS drivers
	int	MS_DOS
	jc	short Inst_notfound    ; function call error
	test	dx,80h		       ; Q: character device ? (ISDEV bit)
	jz	short Inst_notfound    ;  N: can't be ours

	mov	ax,4407h	       ; get output stat - all EMS drivers
	mov	cx,0
	int	MS_DOS
	jc	short Inst_notfound    ; function call error
	cmp	al,0ffh 	       ; Q: ready?
	jne	short Inst_notfound    ;  N: can't be ours

	mov	ax,4402h	       ; get input - only CEMM has this
	mov	dx,offset LAST:Linkbuf
	mov	cx,Linkbuf_length      ; ask for 6 bytes
	int	MS_DOS
	jc	short Inst_notfound    ; function call error
	cmp	ax,Linkbuf_length      ; Q: input what we asked for?
	jne	short Inst_notfound    ;  N: can't be ours
	cmp	Linkbuf[0],SIG_LENGTH  ; Q: signature len = ours?
	jne	short Inst_notfound    ;       N: no
	mov	cx,Linkbuf[4]
	mov	es,cx		       ; set segment of far call
	mov	cx,Linkbuf[2]	       ; get link address to elim func's
	mov	ah,3eh		       ; close device
	int	MS_DOS
	mov	ax,100h
	jmp	short Inst_okay

Inst_notfound:
	mov	ah,3eh		       ; close device
	int	MS_DOS

	mov	ax,0FFA5h		; special function call
	int	67h
	cmp	ax,845Ah		; invalid function w/ ROR AL,4
	mov	ax,100h
	jne	short Inst_exit
					; BX:CX=far entry point
	mov	es,bx
	jmp	short Inst_okay		; ES:CX=far entry point
Inst_chk	endp


;*****************************************************************************C
;	wait_key - wait for keyboard input or 10 seconds and return.	     *C
;		- must be in this seg since the rest is still packed	     *C
;	ENTRY: none
;									     *C
;	EXIT: none							     *C
;*****************************************************************************C
; This routine waits for keyboard input or 10 seconds before returning.      *C
; It should be called after displaying an error message during initialization*C
; of a device driver.							     *C
;									     *C
; Get keyboard input if module doesn't load. Shows: Press any key when ready *C
;									     *C
;*****************************************************************************C
wait_key	proc	near
	cmp	cs:[DOS_version],3
	jnb	short wait_cont
	ret
wait_cont:
	push	ds			;				     *C
	push	si
	push	ax			;save registers 		     *C
	push	bx			;	"                            *C
	push	cx			;	"                            *C
	push	dx			;	"                            *C

	push	cs			;				     *C
	pop	ds			; set ds==cs			     *C
	mov	dx,offset LAST:Any_key	;print "any key to continue"         *C
	mov	ah,9h			;print function 		     *C
	int	21h			;				     *C

	mov	si,182
	xor     ah,ah			;  read tick counter request
        int     1AH                     ;  call BIOS
keepDX:
        mov     bx,dx			;  establish base tick count
waittick:
        mov     ah,1			; check keyboard status request
        int     16H			;Q: Any characters?
        jnz	short ClearBuff		; Y: clear buffer
        xor     ah, ah                  ; N: read tick counter request
        int     1AH                     ;  call BIOS
	cmp	dx,bx			;Q: Has a tick ocurred?
	jz	short waittick		; N: continue
	sub	si,1			; Y: decrement tick count
	jz	short waitexit		; if zero, timeout
	jmp	short keepDX		; if not, loop again

ClearBuff:
        xor     ah,ah			;  else, get key from buffer
        int     16H                     ;  call BIOS
        mov     ah,1			;Q: buffer clear?
        int     16H
        jnz     short ClearBuff         ; N: clear buffer

waitexit:
	pop	dx			;restore registers
	pop	cx			;restore registers
	pop	bx			;restore registers
	pop	ax			;	"
	pop	si
	pop	ds			;
	ret
wait_key	endp

LAST    ends
	end	CEMM

