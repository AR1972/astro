;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */
	TITLE	EXTENDED MEMORY RAMDRIVE

	PAGE	58,132
; ---------------------------------------------------------------------------
; NOTE:  Change volume creation date (see VOLID) for each new driver version.
; ---------------------------------------------------------------------------
;
; Will use XMS extended memory or
;      use Above Board on PC, XT, or AT or
;      use main memory on PC, XT, or AT
;
;
;	device = ramdrive.sys [bbbb] [ssss] [dddd] [/E | /A]
;
;		bbbb  First numeric argument, if present, is disk size
;			in K bytes. Default value is 64. Min is 4. Max
;			is 32767 (32M-1K). (M001, M002)
;
;		ssss  Second numeric argument, if present, is sector size
;			in bytes. Default value is 512. Allowed values are
;			128, 256, 512, 1024.
;		      NOTE: In the case of IBM PC DOS the MAX value is 512.
;			If 1024 is specified the device will not be installed.
;			This "error" is detected by DOS and is not due to
;			the code in RAMDrive.
;			The 1024 byte size is included for those MS-DOS systems
;			where it might be allowed.
;
;		dddd  Third numeric argument, if present, is the number of
;			root directory entries. Default is 64. Min is 2
;			max is 1024. The value is rounded up to the nearest
;			sector size boundary.
;		      NOTE: In the event that there is not enough memory
;			to create the RAMDrive volume, RAMDrive will try to make
;			a DOS volume with 16 directory entries. This may
;			result in a volume with a different number of directory
;			entries than the dddd parameter specifies.
;
;		/E    Specifies that XMS Extended Memory is to be used.
;			It is an error if /E is specified and an XMS driver
;			is not already installed.
;
;		/A    Specifies that Above Board memory is to be used. It
;			is an error if the above board device driver is not
;			present.
;		      NOTE: Information on RAMDrive drives in Above Board memory
;			will be lost at system re-boot (warm or cold). This is
;			due to the fact that the EMM device driver performs a
;			destructive test when it is installed which zeros all
;			of the Above Board memory.
;
;		Neither /A or /E Specifies drive is to be set up below the
;			640K boundary in main memory.
;		      The RAMDRIVE.SYS program looks for memory to assign to the RAMDrive
;			drives by looking for functioning system RAM between the
;			"end of memory" as determined by the INT 12H ROM BIOS
;			function, and the start of the video RAM (0A000:0H).
;                       [This method no longer attempted.]
;		      If RAM is found by the above scan, it is assigned to
;			RAMDrive and managed in the same way as extended memory
;			is when the /E switch is used. As with /E there is
;			1k of RAMDrive overhead. That is to say, if there are 256k
;			bytes of memory above the INT 12 memory size, there
;			will be 255k bytes available for assignment to RAMDrive
;			drives. This 1k overhead is fixed and does not depend
;			on the number of RAMDrive drives installed.
;			Information on such RAMDrive drives will NOT be lost on
;			a "warm boot" (INT 19H or Ctrl-Alt-DEL).
;                       [This method no longer attempted.]
;		      If RAM is NOT found by the above scan, RAMDrive will attempt
;			to allocate memory for the device AS PART OF THE DEVICE.
;			In other words the device starts immediately after the
;			RAMDrive resident code.
;			Information on such RAMDrive drives WILL BE lost on
;			a "warm boot" (INT 19H or Ctrl-Alt-DEL).
;
;
; MODIFICATION HISTORY
;
;	1.00	5/30/85 ARR Initial version.
;
;	1.01	6/03/85 ARR Added CSIZE home code in INIDRV. Does a better
;			    job of computing good CSIZE value.
;
;	1.10	6/05/85 ARR Changed name of program from VDISK to RAMDRIVE
;
;	1.11	6/06/85 ARR Changed BAD_AT message
;
;	1.12	6/06/85 ARR Fixed bug in /A BLKMOV code. Was forgetting
;			    to save and restore page mapping context
;
;	1.13	6/14/85 ARR Was using 32 bit shifts to do div/mul by
;			    powers of two. As it turns out, using the
;			    DIV or MUL instruction is faster. This is
;			    so even for small numbers like 16. This is
;			    due to the fact that the LOOP involved in
;			    doing a 32 bit shift is expensive.
;
;	1.14	6/14/85 ARR dddd param minimum changed from 4 to 2
;			    to be IBM compatible. Code added to round
;			    up to sector size boundaries.
;
;	1.15	6/24/85 ARR Assorted clean up, mostly in Above Board
;			    code.
;
;	1.16	7/09/85 ARR Align code more closely to the G.L.
;			    coding standard.
;
;			    Changed ITOA routine. Smaller and will print any
;			    16 bit value.
;
;			    DISK_ABORT would run through EMM_CTRL reset code
;			    on a RESMEM_SPECIAL driver. Added code
;			    to skip if this type of driver.
;
;			     Added check in CHECK_DOS_VOL in event valid BPB
;			     is found to make sure SSIZE and DIRNUM values
;			     match. If you edit DEVICE = to change these
;			     values on an existing drive and re-boot
;			     RAMDrive would ignore you and suck up old
;			     values.
;
;		11/12/85 ARR DEBUG EQU added and some RESMEM debug code
;			     stuck in to discover that the HP Vectra is
;			     not as AT compatible as HP thinks.
;
;		02/11/86 ARR Message area identified by "TRANSLATION"
;			     and translation notes added to several
;			     messages
;
;		04/03/86 ARR Changed use of SIDT to set GDT descriptor
;			     in /E init code to SGDT. Previous masm wouldn't
;			     assemble SGDT, new one works OK.
;
;	1.17	5/26/86  ARR New version for "above" insignificgant changes. And
;			     fixed major oops in /e RESET_SYSTEM code which would
;			     hang the system if an interrupt occured at the wrong
;			     time.
;
;	1.19	3/4/87	 SP  Fixed CSIZ homing oscillation bug. Shifted Ramdriv
;			     configuration display code before relocation code
;			     to facilitate creation of message module. Shifted
;			     translatable messages to message module.
;
;	2.00	8/23/87  sp  386 support ( both prot mode transfer and int15 )
;			     286 loadall kludge
;			     new int15 allocation
;			     new above_blkmov routine (to handle overlapping
;			     transfers in above board memory
;			     olivetti support
;			     removed int 9 trapping
;			     reset code different for extended memory
;
;	2.01	9/28/87  sp  Fixed bug in parsing for /u option
;
;	2.02	3/02/88  sp  Extended PS2 model 80 recognition to more than
;			     one sub-model
;	2.03	5/13/88  SP  extended version check to include dos 4.00
;
;	2.04	5/23/88  SP  reworked messages to mention expanded memory
;
;	2.10	6/13/88  CHIPA Merged in HP Vectra stuff
;		11/20/87 RCP Fixed a20 enabling/disabling problems on
;			     Vectra machines.
;
;	2.12	7/26/88  SP  Ramdrives installed between int12 and A000 are
;			     no longer attempted.
;
;	3.00	9/12/89  DBO Use XMS driver for extended memory (type 1
;			     driver).  Cut out most support for pre-XMS
;			     extended memory.  Also removed vestigial
;			     support for type 3 driver.
;			     Removed BAD_AT message.
;			     Removed INT 9, INT 15h support.
;			     Restricted INT 19h support to TYPE 2 driver.
;			     DOS version check accepts up to 4.10.
;		10/9/89  DBO DOS version check accepts up to 4.01.
;	3.00.02	10/13/89 DBO DOS version check accepts 4.x.
;
;	3.00 (debug 00)
;		12/21/89 DBO Removed /V (himem.sys version check).
;			     Hook XMS int 2f to ward off Windows/386 v2.x.
;	3.00 (debug 01)
;		12/22/89 DBO Accept DOS versions thru 5.x.
;	3.00 (debug 02)
;		1/4/90   DBO Change XMS hook to approved method, don't watch
;			      int 2F.
;	3.03	2/1/90	     Replace popf in XmmGuard to work around old
;			      80286 bug.
;			     Sense errors correctly in XMS block move.
;	3.04	2/4/90	     2M byte ramdisk screws up as file goes off
;			      end of disk and wraps around.  Is cluster
;			      # FF0 valid?  Changed initialization so
;			      maximum # clusters is 4096-18 instead of
;			      4096-10.
;	3.05	7/30/90  DBO M00:  Make sure EMS page frame really exists.
;       3.05    11/27/90 DB  M001: Extend max. RAMDrive size from 4M to 32M-1K.
;       3.05    12/14/90 DB  M002: Decrease min. RAMDrive size from 16K to 4K.
;                                  Versions >= Dos 5.X use memory limit
;                                  provided in INIT packet BREAK address,
;				   rather value returned by INT 12h.
;       3.05    08/21/91 EA  Updated Vol Creation Date for Windows 3.10 Setup, left same version.   

BREAK	MACRO	subtitle
	SUBTTL	subtitle
	PAGE
	ENDM

DEBUG	EQU	0		; enable/disable debug messages

	IF1
	IF DEBUG
	%out DEBUG VERSION!!!!!!
	ENDIF
	ENDIF

.xlist
	include devsym.inc
	include syscall.inc
	include dirent.inc
.list

; The RAMDrive device driver has 4 basic configurations.
;
;	TYPE 1 - /E configuration using XMS extended memory.
;
;	TYPE 2 - /A configuration using Above Board memory and EMM device
;		driver.
;
;	TYPE 3 - Neither /A or /E (RESMEM) configuration using main memory
;		and normal 8086 addressing, RAMDrive memory is located
;		somewhere AFTER the "end of memory" as indicated by the
;		INT 12H memory size.
;		[NOTE:  Type 3 configuration no longer attempted.]
;
;	TYPE 4 - RESMEM configuration as TYPE 3 EXCEPT that the RAMDrive
;		memory is part of the RAMDrive device driver.
;
; The TYPE 2 driver uses the Above Board EMM device driver via INT 67H
;    to control access to, and to access the available memory.
;
; The TYPE 4 driver needs no external help to control access to the available
;    memory since the RAMDrive memory is part of the device driver and
;    immediately follows the RAMDrive code in memory.
;
; The TYPE 1 configuration uses a resident XMS manager to
;    control access to the available memory

	include emm.inc

	include above.inc

	include ab_macro.inc
	
	include xmm.inc

BREAK	<I/O Packet offset declarations>

;
; Define I/O packet offsets for useful values.
;
; SEE ALSO
;	MS-DOS Technical Reference manual section on Installable Device Drivers
;

; READ/WRITE PACKET OFFSETS
RW_COUNT	EQU	WORD PTR (SIZE SRHEAD) + 5
RW_TRANS	EQU	DWORD PTR (SIZE SRHEAD) + 1
RW_START	EQU	WORD PTR (SIZE SRHEAD) + 7

; MEDIA CHECK PACKET OFFSETS
MCH_RETVAL	EQU	BYTE PTR (SIZE SRHEAD) + 1
MCH_MEDIA	EQU	BYTE PTR (SIZE SRHEAD) + 0

; BUILD BPB PACKET OFFSETS
BPB_BUFFER	EQU	DWORD PTR (SIZE SRHEAD) + 1
BPB_MEDIA	EQU	BYTE PTR (SIZE SRHEAD) + 0
BPB_BPB 	EQU	DWORD PTR (SIZE SRHEAD) + 5

; INIT PACKET OFFSETS
INIT_NUM	EQU	BYTE PTR (SIZE SRHEAD) + 0
INIT_BREAK	EQU	DWORD PTR (SIZE SRHEAD) + 1
INIT_BPB	EQU	DWORD PTR (SIZE SRHEAD) + 5
INIT_DOSDEV	EQU	BYTE PTR (SIZE SRHEAD) + 9


BREAK	<Device header>

RAMCODE SEGMENT
ASSUME	CS:RAMCODE,DS:NOTHING,ES:NOTHING,SS:NOTHING

;**
;
;	RAMDRIVE DEVICE HEADER
;
;	COMMON TO TYPE 1, 2, 3, 4 drivers
;
;	SEE ALSO
;	  MS-DOS Technical Reference manual section on
;	  Installable Device Drivers
;

RAMDEV	LABEL	WORD
	DW	-1,-1
DEVATS	DW	DEVOPCL
	DW	STRATEGY
	DW	RAM$IN
	DB	1			;1 RAMDRIVE


BREAK	<Command dispatch table>

;**
;
; This is the device driver command dispatch table.
;
; The first byte indicates the size of the table and therefore defines
; which device function codes are valid.
;
; The entries in the table are NEAR word addresses of the appropriate
; device routine. Thus the address of the routine to handle device function
; 3 is:
;	WORD at ((RAMTBL + 1) + (2 * 3))
;
;	COMMON TO TYPE 1, 2, 3, 4 drivers
;
;

RAMTBL	LABEL	WORD
	DB	15			; Max allowed command code
	DW	RAM$INIT
	DW	MEDIA$CHK
	DW	GET$BPB
	DW	CMDERR
	DW	RAM$READ
	DW	DEVEXIT
	DW	DEVEXIT
	DW	DEVEXIT
	DW	RAM$WRIT
	DW	RAM$WRIT
	DW	DEVEXIT
	DW	DEVEXIT
	DW	DEVEXIT
	DW	DEVEXIT
	DW	DEVEXIT
	DW	RAM$REM


BREAK	<BPB and boot sector for installed device>

;**  RAMDRIVE BIOS PARAMETER BLOCK AND BOGUS BOOT SECTOR
;
;	This region is a valid DOS 2.X 3.X "boot sector" which contains
;	the BPB. This is used for signiture verification of a valid
;	RAMDrive as well as for storage of the relevant BPB parameters.
;
;	The BOOT_START code is a very simple stub which does nothing
;	except go into an infinite loop. THIS "CODE" SHOULD NEVER
;	BE EXECUTED BY ANYONE.
;
;	COMMON TO TYPE 1, 2, 3, 4 drivers
;
;

BOOT_SECTOR	LABEL	BYTE
	JMP	BOOT_START	; WARNING- don't change to short jump!
	DB	"RDV 1.20"

RDRIVEBPB:
SSIZE	DW	512		; Physical sector size in bytes
CSIZE	DB	0		; Sectors/allocation unit
RESSEC	DW	1		; Reserved sectors for DOS
FATNUM	DB	1		; No. allocation tables
DIRNUM	DW	64		; Number directory entries
SECLIM	DW	0		; Number sectors
	DB	0F8H		; Media descriptor
FATSEC	DW	1		; Number of FAT sectors
	DW	1		; Number of sectors per track
	DW	1		; Number of heads
	DW	0		; Number of hidden sectors

SEC_SHFT DB	8		; Shifting number of
				;  sectors LEFT by this
				;  many bits yields #words
				;  in that many sectors.
				;  128	 6
				;  256	 7
				;  512	 8
				;  1024  9

BOOT_START:
	JMP	BOOT_START

BOOT_SIG	LABEL BYTE
	DB	(128 - (OFFSET BOOT_SIG - OFFSET BOOT_SECTOR)) DUP ("A")

;
; The following label is used to determine the size of the boot record
;		OFFSET BOOT_END - OFFSET BOOT_SECTOR
;
BOOT_END LABEL BYTE

BREAK	<Common Device code>

;	RAMDRIVE DEVICE ENTRY POINTS - STRATEGY, RAM$IN
;
;	This code is standard DOS device driver function dispatch
;	code. STRATEGY is the device driver strategy routine, RAM$IN
;	is the driver interrupt routine.
;
;	RAM$IN uses RAMTBL to dispatch to the appropriate handler
;	for each device function. It also does standard packet
;	unpacking.
;
;	SEE ALSO
;	  MS-DOS Technical Reference manual section on
;	  Installable Device Drivers
;

ASSUME	CS:RAMCODE,DS:NOTHING,ES:NOTHING,SS:NOTHING

PTRSAV		DD	0		; Storage location for packet addr
XmmControl	dd	0		; interface to himem (XMS)
PrevXmm		dd	0		; previous installed XMS manager
ext_handle	dw	-1		; contains handle of ext. mem block
XmmMoveBuf	extmemmovestruct <>
reboot_flg	db	0		; true when reset code is being executed

;**	STRATEGY - Device strategy routine
;
;	Standard DOS 2.X 3.X device driver strategy routine. All it does
;	is save the packet address in PTRSAV.
;
;	ENTRY	ES:BX -> Device packet
;	EXIT	NONE
;	USES	NONE
;
;	COMMON TO TYPE 1, 2, 3, 4 drivers
;
;

STRATP	PROC	FAR

STRATEGY:
	MOV	WORD PTR [PTRSAV],BX	; Save packet addr
	MOV	WORD PTR [PTRSAV+2],ES
	RET

STRATP	ENDP

;**	RAM$IN - Device interrupt routine
;
;	Standard DOS 2.X 3.X device driver interrupt routine.
;
;
;	ENTRY	PTRSAV has packet address saved by previous STRATEGY call.
;	EXIT	Dispatch to appropriate function handler
;			CX = Packet RW_COUNT
;			DX = Packet RW_START
;			ES:DI = Packet RW_TRANS
;			DS = RAMCODE
;			STACK has saved values of all regs but FLAGS
;		    All function handlers must return through one of
;			the standard exit points
;	USES	FLAGS
;
;	COMMON TO TYPE 1, 2, 3, 4 drivers
;
;

RAM$IN:
	PUSH	SI
	PUSH	AX
	PUSH	CX
	PUSH	DX
	PUSH	DI
	PUSH	BP
	PUSH	DS
	PUSH	ES
	PUSH	BX

	LDS	BX,[PTRSAV]	       ;GET POINTER TO I/O PACKET
    ;
    ; Set up registers for READ or WRITE since this is the most common case
    ;
	MOV	CX,DS:[BX.RW_COUNT]	;CX = COUNT
	MOV	DX,DS:[BX.RW_START]	;DX = START SECTOR
	MOV	AL,DS:[BX.REQFUNC]	; Command code
	MOV	AH,BYTE PTR [RAMTBL]	; Valid range
	CMP	AL,AH
	JA	CMDERR			; Out of range command code
	MOV	SI,OFFSET RAMTBL + 1	; Table of routines
	CBW				; Make command code a word
	ADD	SI,AX			; Add it twice since one word in
	ADD	SI,AX			;  table per command.

	LES	DI,DS:[BX.RW_TRANS]	; ES:DI transfer address

	PUSH	CS
	POP	DS

ASSUME	DS:RAMCODE

	JMP	WORD PTR [SI]		; GO DO COMMAND

;**	EXIT - ALL ROUTINES RETURN THROUGH ONE OF THESE PATHS
;
;	Exit code entry points:
;
;	SEE ALSO
;	  MS-DOS Technical Reference manual section on
;	  Installable Device Drivers
;
;	GENERAL ENTRY for all entry points
;		All packet values appropriate to the specific device function
;		filled in except for the status word in the static request
;		header.
;
;	CMDERR - Used when an invalid device command is detected
;
;		ENTRY Stack has frame set up by RAM$IN
;		EXIT  Standard Device driver with error 3
;		USES  FLAGS
;
;	ERR$CNT - Used when READ or WRITE wants to return with error code.
;		   The packet RW_COUNT field is zeroed
;
;		ENTRY AL is error code for low byte of packet status word
;		      Stack has frame set up by RAM$IN
;		EXIT  Standard Device driver with error AL
;		USES  FLAGS
;
;	ERR$EXIT - Used when a function other that READ or WRITE wants to
;			return an error
;
;		ENTRY AL is error code for low byte of packet status word
;		      Stack has frame set up by RAM$IN
;		EXIT  Standard Device driver with error AL
;		USES  FLAGS
;
;	DEVEXIT - Used when a function wants to return with no error
;
;		ENTRY AL is value for low byte of packet status word
;		       NOTE: Typically there is no meaningful value
;			in the AL register when EXITing through here.
;			This is OK as the low 8 bits of the status word
;			have no meaning unless an error occured.
;		      Stack has frame set up by RAM$IN
;		EXIT  Standard Device driver with no error
;		USES  FLAGS
;
;	ERR1 - Used when a function wants to return with a value
;			for the whole status word
;
;		ENTRY AX is value for packet status word
;		      Stack has frame set up by RAM$IN
;		EXIT  Standard Device driver with or without error
;		USES  FLAGS
;
;	COMMON TO TYPE 1, 2, 3, 4 drivers
;
;

ASSUME	DS:NOTHING,ES:NOTHING,SS:NOTHING

CMDERR:
	MOV	AL,3			;UNKNOWN COMMAND ERROR
	JMP	SHORT ERR$EXIT

ERR$CNT:
	LDS	BX,[PTRSAV]
	MOV	[BX.RW_COUNT],0 	; NO sectors transferred
ERR$EXIT:				; Error in AL
	MOV	AH,(STERR + STDON) SHR 8  ;MARK ERROR RETURN
	JMP	SHORT ERR1

EXITP	PROC	FAR

DEVEXIT:
	MOV    AH,STDON SHR 8
ERR1:
	LDS	BX,[PTRSAV]
	MOV	[BX.REQSTAT],AX 	; Set return status

	POP	BX
	POP	ES
	POP	DS
	POP	BP
	POP	DI
	POP	DX
	POP	CX
	POP	AX
	POP	SI
	RET				;RESTORE REGS AND RETURN
EXITP	ENDP


;**	MEDIA$CHK - Device Driver Media check routine
;
;	RAMDRIVE Media check routine. ALWAYS returns media not changed
;
;	SEE ALSO
;	  MS-DOS Technical Reference manual section on
;	  Installable Device Drivers
;
;	ENTRY from RAM$IN
;	EXIT through DEVEXIT
;	USES DS,BX
;
;	COMMON TO TYPE 1, 2, 3, 4 drivers
;

MEDIA$CHK:
ASSUME	DS:RAMCODE,ES:NOTHING,SS:NOTHING
	LDS	BX,[PTRSAV]
ASSUME	DS:NOTHING
	MOV	[BX.MCH_RETVAL],1	; ALWAYS NOT CHANGED
	JMP	DEVEXIT

;**	GET$BPB - Device Driver Build BPB routine
;
;	RAMDRIVE Build BPB routine. Returns pointer to BPB at RDRIVEBPB
;
;	SEE ALSO
;	  MS-DOS Technical Reference manual section on
;	  Installable Device Drivers
;
;	ENTRY from RAM$IN
;	EXIT through DEVEXIT
;	USES DS,BX
;
;	COMMON TO TYPE 1, 2, 3, 4 drivers
;

GET$BPB:
ASSUME	DS:RAMCODE,ES:NOTHING,SS:NOTHING
	LDS	BX,[PTRSAV]
ASSUME	DS:NOTHING
	MOV	WORD PTR [BX.BPB_BPB],OFFSET RDRIVEBPB
	MOV	WORD PTR [BX.BPB_BPB + 2],CS
	JMP	DEVEXIT

;**	RAM$REM - Device Driver Removable Media routine
;
;	RAMDRIVE Removable Media routine. ALWAYS returns media not removable
;	NOTE: This routine is never called if running on DOS 2.X
;
;	SEE ALSO
;	  MS-DOS Technical Reference manual section on
;	  Installable Device Drivers
;
;	ENTRY from RAM$IN
;	EXIT through ERR1
;	USES AX
;
;	COMMON TO TYPE 1, 2, 3, 4 drivers
;

RAM$REM:
ASSUME	DS:RAMCODE,ES:NOTHING,SS:NOTHING
	MOV	AX,STBUI + STDON	; Media NOT removable
	JMP	ERR1

;**	RAM$READ - Device Driver READ routine
;
;	RAMDRIVE READ routine. Perform device READ by calling MEMIO
;
;	SEE ALSO
;	  MS-DOS Technical Reference manual section on
;	  Installable Device Drivers
;
;	DO_OP entry point used by RAM$WRITE
;
;	ENTRY from RAM$IN
;		ES:DI is transfer address
;		CX is sector transfer count
;		DX is start sector number
;	EXIT through DEVEXIT or ERR$CNT
;	USES ALL
;
;	COMMON TO TYPE 1, 2, 3, 4 drivers
;

RAM$READ:
ASSUME	DS:RAMCODE,ES:NOTHING,SS:NOTHING
	XOR	BH,BH
DO_OP:
	CALL	MEMIO
	JC	T_ERR
	JMP	DEVEXIT

T_ERR:					; AL has error number
	JMP	ERR$CNT

;**	RAM$WRITE - Device Driver WRITE routine
;
;	RAMDRIVE WRITE routine. Perform device WRITE by calling MEMIO
;
;	SEE ALSO
;	  MS-DOS Technical Reference manual section on
;	  Installable Device Drivers
;
;	ENTRY from RAM$IN
;		ES:DI is transfer address
;		CX is sector transfer count
;		DX is start sector number
;	EXIT Jump to DO_OP to call MEMIO with BH = 1 (WRITE)
;	USES BH
;
;	COMMON TO TYPE 1, 2, 3, 4 drivers
;

RAM$WRIT:
ASSUME	DS:RAMCODE,ES:NOTHING,SS:NOTHING
	MOV	BH,1
	JMP	DO_OP

;**	MEMIO - Perform READ or WRITE to RAMDrive
;
;	This routine performs common pre-amble code for the BLKMOV
;	routine which is the one which does the real work. It checks
;	the I/O parameters for validity and sets up the inputs to
;	BLKMOV. What it does is convert the sector count in CX to
;	the number of words in that many sectors or 8000H which ever
;	is less. It also converts the start sector number in DX into
;	a 32 bit byte offset equal to that many sectors.
;
;	NOTE that we convert the number of sectors to transfer
;	to a number of words to transfer.
;		Sector size is always a power of two, therefore a multiple
;			of two so there are no "half word" problems.
;		DOS NEVER asks for a transfer larger than 64K bytes except
;			in one case where we can ignore the extra anyway.
;
;	ENTRY:
;	    ES:DI is packet transfer address.
;	    CX is number of sectors to transfer.
;	    DX is starting sector number
;	    BH is 1 for WRITE, 0 for READ
;	EXIT:
;	    If error detected
;		Carry Set
;			Error on operation, AL is error number
;	    else
;		through BLKMOV
;		    ES:DI is packet transfer address.
;		    CX is number of words to transfer.
;		    DX:AX is 32 bit start byte offset (0 = sector 0 of RAMDrive drive)
;		    BH is 1 for WRITE, 0 for READ
;	USES:
;	    AX, DX, CX, FLAGS
;
;	COMMON TO TYPE 1, 2, 3, 4 drivers
;

SEC_NOT_FOUND:
	MOV	AL,8			; Sector not found error
	STC
	RET

MEMIO:
ASSUME	DS:RAMCODE,ES:NOTHING,SS:NOTHING
	CMP	DX,[SECLIM]		; Check for valid I/O
	JAE	SEC_NOT_FOUND		; Start is beyond end
	MOV	AX,DX
	ADD	AX,CX
	CMP	AX,[SECLIM]
	JA	SEC_NOT_FOUND		; End is beyond end
    ;
    ; Convert sector count to word count
    ;
	MOV	AX,CX
	MOV	CL,[SEC_SHFT]
	SHL	AX,CL			; AX is # words to move
	JNC	CNT_SET 		; Overflow???
	MOV	AX,8000H		; Limit to 64K bytes
CNT_SET:
	MOV	CX,AX
    ;
    ; Now compute start offset of I/O
    ;
	MOV	AX,DX
	MUL	[SSIZE] 		; DX:AX is byte offset of start
	JMP	SHORT BLKMOV		; Perform I/O


BREAK	<Drive code for /E driver>

;
; The following label defines the start of the I/O code which is driver type
; specific.
;
; THE TYPE 2 driver must REPLACE this code with code appropriate
;	to the driver type.
;
		EVEN		; Force start of drive code to word boundary

DRIVE_CODE	LABEL	WORD

EXTMEM_LOW	EQU	0000H	; 24 bit addr of start of extended memory
EXTMEM_HIGH	EQU	0010H

;**	BASE_ADDR data element
;
; The next value defines the 24 bit address of the start of the memory for
;  the cache.
;
; NOTE THAT IT IS INITIALIZED TO THE START OF EXTENDED MEMORY.
;
; NOTE: This data element is shared by TYPE 1, 2 drivers, but
;	its meaning and correct initial value are driver type specific.
;	[Now set but NOT USED by TYPE 1 driver.]
;

BASE_ADDR	LABEL	DWORD	; 24 bit address of start of this RAMDRV
		DW	EXTMEM_LOW
		DW	EXTMEM_HIGH

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;**	BLKMOV - Perform transfer for TYPE 1 driver
;
;	ENTRY:
;	    ES:DI is packet transfer address.
;	    CX is number of words to transfer.
;	    DX:AX is 32 bit start byte offset (0 = start of cache)
;	    BH is 1 for WRITE, 0 for READ
;
;	EXIT:
;	    Carry Clear
;		    OK, operation performed successfully
;	    Carry Set
;		    Error during operation, AL is error number (INT 13 error)
;
;	USES:
;	    ALL
;
;	This routine is specific to TYPE 1 driver
;

BLKMOV:
ASSUME	DS:ramcode,ES:NOTHING,SS:NOTHING

;############################################################################
;[This is the XMS version, copied from Smartdrv.]
;
;	Function	:	blkmov
;	Inputs		:	es:di -> points to packet 
;				dx:ax -> points to cache 
;				cx - > no. of words to transfer
;				bh = 1 (WRITE) 0 (READ)		
;
;	Outputs		: 	If error CY flag is set
;				else move accomplished
;
;	Description:
;		Fills up the XmmMoveBuf structure with the correct 
; 	values supplied by the input parameters and calls the XMM Extended
;	Block Move function to perform the move. 
;
;	written: HKN 2/15/89
;
;############################################################################

;	int	3

	push	dx
	push	ax
	push	bx
	push	cx
	push	si
	
	mov	si, offset XmmMoveBuf	; cs:si points to buffer


; initialize length field

	shl	cx, 1			; cx = # of bytes to transfer
	mov	word ptr cs:[si.mov_length], cx
	mov	word ptr cs:[si.mov_length + 2], 0

	or	bh, bh
	jnz	xmm_cache_write


; initialize source handle 

	mov	bx, [ext_handle]
	mov	cs:[si.src_handle], bx

; initialize source offset 

	mov	word ptr cs:[si.src_offset], ax
	mov	word ptr cs:[si.src_offset + 2], dx


; initialize destination handle	and offset 

	mov	cs:[si.dst_handle], 0		; offset is Segment:Offset
	mov	word ptr cs:[si.dst_offset], di
	mov	word ptr cs:[si.dst_offset + 2], es

	jmp	short do_move

xmm_cache_write:
; initialize source handle and offset

	mov	cs:[si.src_handle], 0		; offset is Segment:Offset
	mov	word ptr cs:[si.src_offset], di
	mov	word ptr cs:[si.src_offset + 2], es

; initialize destination handle

	mov	bx, [ext_handle]
	mov	cs:[si.dst_handle], bx

; initialize destination offset

	mov	word ptr cs:[si.dst_offset], ax
	mov	word ptr cs:[si.dst_offset + 2], dx

; call XMM to move memory

do_move:
	mov	ah, XMM_MOVE_EMB	; move ext. mem. block function
	call	[XMMcontrol]

	shr	ax,1			; rotate 'success' bit into carry
	cmc				; make it a failure flag

	pop	si
	pop	cx
	pop	bx
	pop	ax
	pop	dx
	ret



;***	XmmGuard - XMM front for Windows/386 v2.x detection
;
;	This routine gets in front of the XMM control function, in
;	order to detect Windows/386 v2.x as it loads.  These versions
;	of Windows corrupt XMS memory.  When we spot the weird XMS
;	call that these versions of Windows make, we'll fail the
;	request, and Windows won't load.
;
;	The call that Windows makes:
;
;		AX = 0140h
;		DX = current total free XMS extended memory
;
;	If we spot these register values in an XMS call, we return
;
;		AX = 0		'HMA not assigned to caller'
;		BL = 91h	'HMA already in use'
;		other registers unchanged
;
;	Otherwise we pass the call on to the XMM.

AnIret:	iret			; used for popf workaround for 80286 bug

	assume	cs:RAMCODE,ds:NOTHING,es:NOTHING,ss:NOTHING

XmmGuard	proc	far

;	Supply a standard XMS control header (see XMS document).

	jmp	short XmmGuardEntry
	nop
	nop
	nop

XmmGuardEntry:
	pushf			; preserve flags register
	cmp	ax,0140h
	je	CouldBe		; could be Windows/386 v2.x
ToXmm:
	push	cs
	call	AnIret		; restore flags w/o popf (old 286 bug)
	jmp	PrevXmm		; xfr to previous installed XMS handler 

CouldBe:
	push	ax		; save caller's registers
	push	bx
	push	dx
	mov	ah,XMM_QUERY_FREE_EXTMEM
	call	XmmControl
	mov	ax,dx		; AX = total kbytes free extended memory
	pop	dx		; restore caller's DX
	cmp	ax,dx		; caller's DX = total free ext'd mem?
	pop	bx		; restore rest of caller's registers
	pop	ax
	jne	ToXmm		; DX doesn't match, it's not Win/386 v2.x

;*	It's Windows/386 v2.x.  Fail the Request HMA call.

	xor	ax,ax		; AX = 'HMA not assigned to caller'
	mov	bl,91h		; BL = 'HMA already in use'
	popf			; restore flags register
	ret

XmmGuard	endp




;	Toss in enough space for swapped-in code.

	db	250h dup (?)



;**	TRUE LOCATION OF ABOVE_PID
;
;	Define the TRUE (runtime TYPE 2 driver) location of ABOVE_PID.
;	This is the only piece of TYPE 2 specific data that we need
;	in the resident image. We must define it HERE rather than down
;	at ABOVE_BLKMOV so that we have its TRUE location after the
;	TYPE 2 code is swapped in at initialization. If we defined
;	it down at ABOVE_BLKMOV any instruction like:
;
;		MOV	DX,[ABOVE_PID]
;
;	Would have to be "fixed up" when we moved the ABOVE_BLKMOV
;	code into its final location.
;

ABOVE_PID	EQU	WORD PTR $ - 2		; TRUE location of ABOVE_PID

;
; The following label defines the end of the region where BLKMOV code
;   may be swapped in. BLKMOV code to be swapped in MUST fit
;   between DRIVE_CODE and DRIVE_END
;
DRIVE_END	LABEL	WORD


BREAK	<BPB POINTER ARRAY>

;**	BPB pointer array data
;
; BPB pointer array returned by INIT call. Must be part of resident image.
;
;	SEE ALSO
;	  MS-DOS Technical Reference manual section on
;	  Installable Device Drivers
;

INITAB	DW	RDRIVEBPB

;
; The following label defines the end of the RAMDrive resident code
;  for cases where no INT 9/19 code is included.
;
DEVICE_END	LABEL	BYTE

BREAK	<INT 19 Handler. Incl if /A>

;
; The drive TYPE dependant piece of code works as follows:
;
;	TYPE 2 DOES NOT use the EMM_CTRL sector but it still has
;		a handler. What this handler does is issue an
;		ABOVE_DEALLOC call to deallocate the Above Board
;		memory allocated to the RAMDrive. In current versions
;		of the EMM device driver this step is unnecessary
;		as the EMM device driver is thrown away together
;		with all of the allocation information when the system
;		is re-booted. We do it anyway because some future version
;		of the EMM device driver may be smarter and retain
;		allocation information through a warm-boot. Currently,
;		doing this doesn't hurt anything. Since this code cannot
;		do a global ABOVE_DEALLOC for all TYPE 2 drivers in the
;		system, it does an ABOVE_DEALLOC only for its memory
;		and EACH TYPE 2 driver in the system includes the INT 19/9
;		code.

;
; Storage locations for the "next" INT 19 vector, the one
;  that was in the interrupt table when the device driver was loaded.
;  It is initialized to -1 to indicate it contains no useful information.
;
OLD_19	LABEL	DWORD
	DW	-1
	DW	-1


;**	INT 19 Software re-boot handler
;
;	All this piece of code does is sit on INT 19 waiting for
;	a re-boot to be signaled by being called. It calls
;	RESET_SYSTEM to perform driver TYPE specific re-boot code,
;	resets the INT 19 and INT 9 vectors,
;	and then jumps to OLD_19 to pass on the event.
;
;	NOTE THAT UNLIKE INT 9 THIS HANDLER NEEDS TO RESET
;	THE INT 9 AND INT 19 VECTORS. This is because the INT 19
;	IBM ROM re-boot code DOES NOT reset these vectors, and we
;	don't want to leave them pointing to routines that are not
;	protected from getting stomped on by the re-boot.
;
;	SEE ALSO
;	    INT 19 IBM ROM code in ROM BIOS listing of
;	    IBM PC Technical Reference manual for any PC family member
;
;	ENTRY
;	    NONE
;	EXIT
;	    NONE, via OLD_19
;	USES
;	    FLAGS
;
;	THIS CODE IS USED BY TYPE 1,2 and 3 drivers.
;

INT_19:
ASSUME	DS:NOTHING,ES:NOTHING,SS:NOTHING
	CALL	RESET_SYSTEM
	PUSH	AX
	PUSH	DS
	XOR	AX,AX
	MOV	DS,AX
    ;
    ; Since INT 19 DOES NOT reset any vectors (like INT 9 Ctrl Alt DEL does),
    ;	we must replace those vectors we have mucked with.
    ;
    ; NOTE THAT WE RESET VECTORS DIRECTLY!!!!!!!!!!!!!!!!!!
    ;	We are not sure that DOS is reliable enough to call.
    ;
	MOV	AX,WORD PTR [OLD_19]
	CLI
	MOV	WORD PTR DS:[19H * 4],AX
	MOV	AX,WORD PTR [OLD_19 + 2]
	MOV	WORD PTR DS:[(19H * 4) + 2],AX
	POP	DS
	POP	AX
	JMP	[OLD_19]

;**	RESET_SYSTEM perform TYPE 1 (/E) driver specific reboot code
;
;	NOTE: RESET_SYSTEM ALSO defines the start of ANOTHER piece of
;		driver TYPE specific code that TYPE 2, 3 and 4 drivers
;		will have to swap in a different piece of code for.
;
;	ENTRY
;	    NONE
;	EXIT
;	    NONE
;	USES
;	    NONE
;
; This code is specific to TYPE 1 drivers
;

RESET_SYSTEM:
ASSUME	DS:NOTHING,ES:NOTHING,SS:NOTHING
;
;   Is this ever executed for TYPE 1 driver?
;
;   must leave space because other guys relocate
;   on top of this
;
	.286				; enable 80286 instructions

	pusha
	mov	cs:[reboot_flg], 0ffh
	mov	dx, [ext_handle]
	cmp	dx, -1
	je	reset_skip

	mov	ah, XMM_UNLOCK_EMB
	call	[XmmControl]
	mov	ah, XMM_FREE_EMB
	call	[XmmControl]

reset_skip:
	popa
	ret

	.8086				; disable 80286 instructions

;	Allow space for swapped-in code.

	db	30h dup (?)

;
; The following label performs two functions. It defines the end of the
; Driver TYPE specific RESET_SYSTEM code which will have to be replaced
; for different driver TYPEs as the code between RESET_SYSTEM and
; RESET_INCLUDE. Swapped in code MUST FIT between RESET_SYSTEM and
; RESET_INCLUDE. It also defines the end of the resident device driver
; code for a driver which wants to include the INT 19/ INT 9 code.
;
RESET_INCLUDE  LABEL   BYTE

BREAK	<COMMON INIT CODE>

;**	DISPOSABLE INIT DATA
;
; INIT data which need not be part of resident image
;

DRIVER_SEL	DB	2	; 0 if /E (TYPE 1), 1 if /A (TYPE 2),
				;    2 if resmem (TYPE 3 or 4)

DEV_SIZE	DW	64	; Size in K of this device

EXT_K		DW	?	; Size in K of Exteneded memory.

NUM_ARG 	DB	1	; Counter for order dependent numeric
				;    arguments	bbbb ssss dddd.

INIT_DRIVE	DB	1	; 0 means drive is inited
				; 1 means drive is to be inited
				;    MUST BE DEFAULT SETTING
				; 2 means drive is to be inited
				;   REGARDLESS of the existence of
				;   a valid DOS volume signature.

GOTSWITCH	DB	0	; 0 if no switch, NZ if switch seen

DIRSEC		DW	?	; Number of directory SECTORS

TERM_ADDR	LABEL	DWORD	; Address to return as break address in INIT packet
		DW	OFFSET DEVICE_END   ; INIT to NOT include INT 19 code
		DW	?		; RAMDrive CS filled in at INIT

TRUE_CS 	DW	?	; Used to store the "true" location of
				;   the driver when the relocation at
				;   RAMDrive_RELOC is performed.

DosVersion      DW      ?       ; M002: MS-DOS version # (high byte = Major,
				; low byte = Minor).

RESMEM_SPECIAL	DB	0	; 0 means NORMAL TYPE 3 RAMDrive
				; NZ means SPECIAL TYPE 4 RESMEM version
				;   see code at RAMDrive_RELOC

XmmControlBase	label	dword	; base address of XMM control headers
		dw	-1
		dw	-1

XmmControlJmpVal db	?	; offset byte of short jmp instruction

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;**	PRINT - Print a "$" terminated message on stdout
;
;	This routine prints "$" terminated messages on stdout.
;	It may be called with only the DX part of the DS:DX message
;	pointer set, the routine puts the correct value in DS to point
;	at the RAMDrive messages.
;
;	ENTRY:
;	     DX pointer to "$" terminated message (RAMCODE relative)
;	EXIT:
;	     NONE
;	USES:
;	     AX
;
;	COMMON TO TYPE 1, 2, 3, 4 drivers
;

PRINT:
ASSUME	DS:NOTHING,ES:NOTHING,SS:NOTHING
	PUSH	DS
	PUSH	CS
	POP	DS
	MOV	AH,Std_Con_String_Output
	INT	21H
	POP	DS
	RET

;**	ITOA - Print Decimal Integer on stdout
;
;	Print an unsigned 16 bit value as a decimal integer on stdout
;	with leading zero supression. Prints from 1 to 5 digits. Value
;	0 prints as "0".
;
;	Routine uses divide instruction and a recursive call. Maximum
;	recursion is four (five digit number) plus one word on stack
;	for each level.
;
;	ENTRY	AX has binary value to be printed
;	EXIT	NONE
;	USES	AX,CX,DX,FLAGS
;
;	COMMON TO TYPE 1, 2, 3, 4 drivers
;

ITOA:
ASSUME	DS:NOTHING,ES:NOTHING,SS:NOTHING

	MOV	CX,10
	XOR	DX,DX
	DIV	CX			; DX is low digit, AX is higher digits
	OR	AX,AX
	JZ	PRINT_THIS_DIGIT	; No more higher digits
	PUSH	DX			; Save this digit
	CALL	ITOA			; Print higher digits first
	POP	DX			; Recover this digit
PRINT_THIS_DIGIT:
	ADD	DL,"0"			; Convert to ASCII
	MOV	AH,Std_CON_Output
	INT	21H
	RET


;**	RAM$INIT - Device Driver Initialization routine
;
;	RAMDRIVE Initialization routine. This is the COMMON initialization
;	code used by ALL driver TYPEs. Its jobs are to:
;
;	    1.	Initialize various global values
;	    2.	Check for correct DOS version and do changes to the device
;			based on the DOS version if needed.
;	    3.	Parse the command line and set values accordingly
;	    4.	Call a TYPE specific INIT routine based on the Parse
;			to set up a specific driver TYPE.
;	    5.	Initialize the DOS volume in the RAMDrive memory if appropriate
;	    6.	Print out report of RAMDrive parameters
;	    7.	Set the return INIT I/O packet values
;
;	The first two lines perform step 1. Step two starts after and
;	goes through VER_OK. Step 3 starts at VER_OK and goes through
;	ARGS_DONE. Step 4 starts at ARGS_DONE and goes through I001.
;	Step 5 starts at I001 and goes through DRIVE_SET. Step 6 starts
;	at DRIVE_SET and goes through SETBPB. Step 7 starts at SETBPB
;	and ends at the JMP DEVEXIT 10 lines later.
;
;	At any time during the above steps an error may be detected. When
;	this happens one of the error messages is printed and RAMDrive
;	"de-installs" itself by returning a unit count of 0 in the INIT
;	device I/O packet. The DOS device installation code is responsible
;	for taking care of the details of re-claiming the memory used by the
;	device driver. All RAMDrive needs to do is make sure any INT vectors
;	it changed (INT 19) get restored to what they were when RAMDrive
;	first started. A TYPE 1 driver must make sure any XMS memory it
;	allocated is deallocated and must unhook from the XMM control chain.
;	A TYPE 2 driver must ABOVE_DEALLOC any memory it allocated from the
;	EMM device. The duty of reclaiming XMS or Above Board memory,
;	re-setting vectors, and unhooking from the XMM control chain is done
;	by the DISK_ABORT routine which may be called by either this COMMON
;	INIT code, or the TYPE specific INIT code.
;
;	Step 1 initializes the segment part of TERM_ADDR to the correct
;	value for type 1, 2 and 3 drivers. A TYPE 4 driver will put a
;	different value in TERM_ADDR as it must include the space taken up
;	by the RAMDrive memory itself which is part of the device. TRUE_CS
;	is also initialized. This datum is relevant to the RESMEM_SPECIAL
;	(TYPE 4) driver which relocates the driver code at RAMDrive_RELOC.
;	This datum stores the CS of the REAL driver (the driver location
;	BEFORE the relocation took place).
;
;	Step 2 checks to make sure that we are running on a DOS in the
;	2.X or 3.X series which this driver is restricted to. If running
;	on a 2.X series the device header attribute word and device command
;	table are patched to exclude those device calls that don't exist
;	on DOS 2.X. The HEADERMES message is also patched to not include
;	the DOS drive letter part because 2.X DOS does not provide this
;	information to the device at INIT time.
;
;	Step 3 uses the "DEVICE = xxxxxxxxx" line pointer provided by
;	DOS to look for the various device parameters. NOTE: This pointer
;	IS NOT DOCUMENTED in the DOS 2.X tech ref material, but it does
;	exist in the same way as 3.X. This code is simple even though
;	it looks rather long. First it skips over the device name field
;	to get to the arguments. In then parses the arguments as they are
;	encountered. All parameter errors are detected here. NOTE THAT
;	THIS ROUTINE IS NOT RESPONSIBLE FOR SETTING DEFAULT VALUES OF
;	PARAMETER VARIABLES. This is accomplished by static initialization
;	of the parameter variables.
;
;	Step 4 calls a device TYPE specific initialization routine based
;	on the parse in step 3 (presence or absense of /E and /A switches).
;	NOTE THAT THERE IS ONE ROUTINE FOR TYPE 3 AND 4 DRIVERS. It is up
;	to this routine itself to make the distinction between TYPE 3 and
;	TYPE 4. NOTE that one of the prime jobs of these device TYPE specific
;	routines is to set all of the variables that are needed by Step
;	5 and 7 that haven't been set by the COMMON init code:
;
;			DEV_SIZE   set to TRUE size of device
;			BASE_ADDR  set to TRUE start of device so MEMIO
;					can be called
;			TERM_ADDR  set to correct end of device
;			INIT_DRIVE set to indicate if DOS volume needs to
;					be set up
;			RESMEM_SPECIAL set if TYPE 4 driver
;
;	Step 5 looks at the INIT_DRIVE variable to see if the DOS volume
;	needs to be initialized. The only time we do not need to INITialize
;	the DOS volume is when the driver TYPE specific INIT code finds
;	that there is a VALID DOS volume in the RAMDrive memory it just
;	set up. If the DOS volume does not need to be initialized, we
;	go on to step 6. Otherwise the device BPB must be set, the
;	RESERVED (boot) sector, FAT sectors, and root directory sectors
;	must be initialized and written out to the RAMDrive. The first step
;	is to initialize all of the BPB values. The code is a typical piece
;	of MS-DOS code which given BYTES/SECTOR, TOTAL DISK SIZE
;	and NUMBER OF ROOT DIRECTORY ENTRIES inputs figures out reasonable
;	values for SEC/CLUSTER and SECTORS/FAT and TOTAL NUMBER OF CLUSTERS.
;	NOTE THAT THIS CODE IS TUNED AND SPECIFIC TO 12 BIT FATS. Don't
;	expect it to work AT ALL with a 16 bit FAT. The next step is to write
;	out the BOOT record containing the BPB to sector 0, write out
;	a FAT with all of the clusters free, and write out a root directory
;	with ONE entry (the Volume ID at VOLID). Take CAREFUL note of the
;	special code and comments at RAMDrive_RELOC.
;
;	Step 6 makes the status report display of DEVICE SIZE, SECTOR SIZE,
;	CLUSTER SIZE, and DIRECTORY SIZE by simply printing out the values
;	from the BPB.
;
;	Step 7 sets the INIT I/O packet return values for # of units,
;	Break address, and BPB array pointer and returns via DEVEXIT.
;
;	SEE ALSO
;	  MS-DOS Technical Reference manual section on
;	  Installable Device Drivers
;
;	ENTRY from RAM$IN
;	EXIT Through DEVEXIT
;	USES ALL
;
;	COMMON TO TYPE 1, 2, 3, 4 drivers
;

RAM$INIT:
ASSUME	DS:RAMCODE,ES:NOTHING,SS:NOTHING

    ;
    ; 1.  Initialize various global values
    ;
	MOV	WORD PTR [TERM_ADDR + 2],CS
	MOV	[TRUE_CS],CS
    ;
    ; 2.  Check for correct DOS version and do changes to the device
    ;	     based on the DOS version if needed.
    ;
	CLD
	MOV	AH,GET_VERSION
	INT	21H
	XCHG	AH,AL
        MOV     DosVersion,AX           ; M002: Save MS-DOS version #.
	CMP	AX,(2 SHL 8) + 00
	JB	BADVER			; Below 2.00, BAD
	CMP	AX,(3 SHL 8) + 00
	JB	VER2X			; 2.X requires some patches
	CMP	AX,(7 SHL 8) + 00
	JB	VER_OK			; thru 6.x ok

BADVER:
	MOV	DX,OFFSET BADVERMES
	JMP	DEVABORT

VER2X:
	AND	[DEVATS],NOT DEVOPCL	    ; No such bit in 2.X
	MOV	BYTE PTR [RAMTBL],11	    ; Fewer functions too
	MOV	WORD PTR [PATCH2X],0A0DH    ; Don't know DOS drive
	MOV	BYTE PTR [PATCH2X + 2],"$"
VER_OK:
;
; 3.  Parse the command line and set values accordingly
;
	LDS	SI,[PTRSAV]
ASSUME	DS:NOTHING
	MOV	AL,[SI.INIT_DOSDEV] ; DOS drive letter
	ADD	CS:[DOS_DRV],AL     ; Need explicit over, this is a forward ref
	MOV	DX,OFFSET HEADERMES
	CALL	PRINT
	LDS	SI,[SI.INIT_BPB]    ; DS:SI points to config.sys
SKIPLP1:			    ; Skip leading delims to start of name
	LODSB
	CMP	AL," "
	JZ	SKIPLP1
	CMP	AL,9
	JZ	SKIPLP1
	CMP	AL,","
	JZ	SKIPLP1
	JMP	SHORT SKIPNM

ARGS_DONEJ:
	JMP	ARGS_DONE

SWITCHJ:
	JMP	SWITCH

SKIPLP2:			; Skip over device name
	LODSB
SKIPNM:
	CMP	AL,13
	JZ	ARGS_DONEJ
	CMP	AL,10
	JZ	ARGS_DONEJ
	CMP	AL," "
	JZ	FIRST_ARG
	CMP	AL,9
	JZ	FIRST_ARG
	CMP	AL,","
	JZ	FIRST_ARG
	CMP	AL,0		; Need this for 2.0 2.1
	JNZ	SKIPLP2
SCAN_LOOP:			; PROCESS arguments
	LODSB
FIRST_ARG:
	OR	AL,AL		; Need this for 2.0 2.1
	JZ	ARGS_DONEJ
	CMP	AL,13
	JZ	ARGS_DONEJ
	CMP	AL,10
	JZ	ARGS_DONEJ
	CMP	AL," "
	JZ	SCAN_LOOP
	CMP	AL,9
	JZ	SCAN_LOOP
	CMP	AL,","
	JZ	SCAN_LOOP
	CMP	AL,"/"
	JZ	SWITCHJ
	CMP	AL,"0"
	JB	BAD_PARMJ
	CMP	AL,"9"
	JA	BAD_PARMJ
	DEC	SI
	CALL	GETNUM
	CMP	[NUM_ARG],3
	JA	BAD_PARMJ		 ; Only 3 numeric arguments
	JZ	SET_DIR
	CMP	[NUM_ARG],2
	JZ	SET_SECTOR
SET_SIZE:
        CMP     BX,4                    ;M002: 4K minimum drive size.
	JB	BAD_PARMJ
	CMP	BX,32768		;M001: Drive size must be < 32M.
	JAE	BAD_PARMJ               ;M001
	MOV	[DEV_SIZE],BX
	JMP	SHORT NUM_DONE

;M001
;Currently, the drive size must be < 32Meg. The total sectors of the drive
;must be < 64K (16-bit #). This constrains the sector size as follows:
;
;       Sector size      Drive size     Shift factor
;       -----------      ----------     ------------
;         1024             < 32M            9
;          512             < 32M            8
;          256             < 16M            7
;          128             <  8M            6

SET_SECTOR:
        mov     [SSIZE],bx              ;Temporarily store input bytes/sect.
        mov     dx,[DEV_SIZE]		;Drive size has already been parsed.
	mov	al,6                    ;Shift factor = 6.

ss128:	cmp	bx,128                  ;Bytes/sect = 128?
	jne	ss256                   ; -no, jump.
	cmp	dx,8192                 ; -yes; drive size < 8M?
	jb	ssOK                    ;        -yes, jump.
	shl	bx,1                    ;        -no, adjust sector size.

ss256:	inc	al                      ;Shift factor = 7.
	cmp	bx,256                  ;Bytes/sect = 256?
	jne	ss512                   ; -no, jump.
	cmp	dx,16384                ; -yes; drive size < 16M?
	jb	ssOK                    ;        -yes, jump.
	shl	bx,1

ss512:	inc	al                      ;Shift factor = 8.
	cmp	bx,512                  ;Bytes/sect = 512?
	je	ssOK                    ; -yes, jump: 512 always OK.

ss1024:	inc	al                      ;Shift factor = 9.
	cmp	bx,1024                 ;Bytes/sect = 1024?
	jne	BAD_PARM                ; -no, jump: invalid input parameter.
                                        ; -yes: 1024 always OK.
ssOK:	mov	[SEC_SHFT],al
        cmp     bx,[SSIZE]              ;Sector adjusted?
        je      NUM_DONE                ; -no, jump.
	mov	[SSIZE],bx
        mov     dx,OFFSET SECT_ADJ
        call    PRINT              	;Notify user of sector adjustment.
	jmp	short NUM_DONE

BAD_PARMJ:
        jmp     short BAD_PARM
;M001


SET_DIR:
	CMP	BX,2
	JB	BAD_PARM
	CMP	BX,1024
	JA	BAD_PARM
    ;
    ; NOTE: Since DIRNUM is the 3rd numeric arg and SSIZE is the first,
    ;	    we know the desired sector size has been given.
    ;
	MOV	DI,[SSIZE]
	MOV	CL,5		; 32 bytes per dir ent
	SHR	DI,CL		; DI is number of dir ents in a sector
	MOV	AX,BX
	XOR	DX,DX
	DIV	DI		; Rem in DX is partial dir sector
	OR	DX,DX
	JZ	SET_DSZ 	; User specified groovy number
	SUB	DI,DX		; Figure how much user goofed by
	ADD	BX,DI		; Round UP by DI entries
SET_DSZ:
	MOV	[DIRNUM],BX
NUM_DONE:
	INC	[NUM_ARG]		; Next numeric argument
SCAN_LOOPJ:
	JMP	SCAN_LOOP

BAD_PARM:
	MOV	DX,OFFSET ERRMSG1
DEVABORT:
	CALL	PRINT
DEVABORT_NOMES:
	XOR	AX,AX			;Indicate no devices
	JMP	SETBPB			;and return

SWITCH:
	MOV	AL,0FFH
	XCHG	AL,[GOTSWITCH]		; Switch already?
	OR	AL,AL
	JNZ	BAD_PARM		; Yes, only one allowed
	LODSB
	CMP	AL,"E"
	JZ	EXT_SET
	CMP	AL,"e"
	JNZ	ABOVE_TEST
EXT_SET:
	MOV	[DRIVER_SEL],0
	JMP	SCAN_LOOP

ABOVE_TEST:
	CMP	AL,"A"
	JZ	ABOVE_SET
	CMP	AL,"a"
	JNZ	BAD_PARM
ABOVE_SET:
	MOV	[DRIVER_SEL],1
	JMP	SCAN_LOOP

ARGS_DONE:
;
; 4.  Call a TYPE specific INIT routine based on the Parse
;	 to set up a specific driver TYPE.
;
	PUSH	CS
	POP	DS
ASSUME	DS:RAMCODE
	MOV	AL,[DRIVER_SEL] 	; Find out which init to call
	OR	AL,AL
	JNZ	NEXTV
	CALL	EXT_INIT
	JMP	SHORT INI_RET

NEXTV:
	DEC	AL
	JNZ	DORESM
	CALL	ABOVE_INIT
	JMP	SHORT INI_RET

DORESM:
	CALL	RESMEM_INIT
INI_RET:
	JNC	I001
	JMP	DEVABORT_NOMES

I001:
;
; 5.  Initialize the DOS volume in the RAMDrive memory if appropriate
;
	CMP	[INIT_DRIVE],0
	JNZ	INIDRV			; Need to initialize drive
	JMP	DRIVE_SET		; All set to go

INIDRV:
;
; We must figure out what to do.
; All values are set so we can call MEMIO to read and write disk
; SSIZE is user sector size in bytes
; DIRNUM is user directory entries
; DEV_SIZE is size of device in K bytes
;
    ; Figure out total number of sectors in logical image
	MOV	AX,[DEV_SIZE]
	MOV	CX,1024
	MUL	CX		; DX:AX is size in bytes of image
	DIV	[SSIZE] 	; AX is total sectors
				; Any remainder in DX is ignored
	MOV	[SECLIM],AX
    ; Compute # of directory sectors
	MOV	AX,[DIRNUM]
	MOV	CL,5		; Mult by 32 bytes per entry
	SHL	AX,CL		; Don't need to worry about overflow, # ents
				;     is at most 1024
	XOR	DX,DX
	DIV	[SSIZE]
	OR	DX,DX
	JZ	NOINC
	INC	AX
NOINC:				; AX is # sectors for root dir
	MOV	[DIRSEC],AX
	ADD	AX,2		; One reserved, At least one FAT sector
	CMP	AX,[SECLIM]
	JB	OK001		; we're OK
	MOV	[DIRNUM],16	; Smallest reasonable number
	XOR	DX,DX
	MOV	AX,512		; 16*32 = 512 bytes for dir
	DIV	[SSIZE]
	OR	DX,DX
	JZ	NOINC2
	INC	AX
NOINC2: 			; AX is # sectors for root dir
	MOV	[DIRSEC],AX
	ADD	AX,2		; One reserved, At least one FAT sector
	CMP	AX,[SECLIM]
	JB	OK001		; 16 directory sectors got us to OK
	CALL	DISK_ABORT	; Barf
	MOV	DX,OFFSET ERRMSG2
	JMP	DEVABORT

OK001:
	mov	si,64		; set a loop bound for the homing process
				; to avoid oscillation in homing
CLUSHOME:
    ; Figure a reasonable cluster size
	MOV	AX,[SECLIM]	; AX is total sectors on disk
	SUB	AX,[RESSEC]	; Sub off reserved sectors
	MOV	CL,[FATNUM]	; CX is number of FATs
	XOR	CH,CH
FATSUB:
	SUB	AX,[FATSEC]	; Sub off FAT sectors
	LOOP	FATSUB
	SUB	AX,[DIRSEC]	; Sub off directory sectors, AX is # data sectors

;	BUGBUG
;	Note:  The following four instances of "4096-18" were
;	"4096-10".  I've changed to this because DOS was freaked
;	out by a 2M byte drive with 64 directory entries and
;	1 sector per cluster.  Note that the last cluster # for
;	such a drive is FF0.  I've seen it stated that FF0-FF6
;	are used for "reserved" clusters.  I'm going to force
;	a 2M byte ramdisk to use 2 sectors per cluster.  It seems
;	to fix the bug, and it may even be correct.  -davidols, 2/90

	MOV	BX,1		; Start at 1 sec per alloc unit
	CMP	AX,4096-18
	JB	CSET		; 1 sector per cluster is OK
	MOV	BX,2
	CMP	AX,(4096-18) * 2
	JB	CSET		; 2 sector per cluster is OK
	MOV	BX,4
	CMP	AX,(4096-18) * 4
	JB	CSET		; 4 sector per cluster is OK
	MOV	BX,8
	CMP	AX,(4096-18) * 8
	JB	CSET		; 8 sector per cluster is OK
	MOV	BX,16
;M001
	CMP	AX,(4096-18) * 16
	JB	CSET		; 16 sector per cluster is OK
	MOV	BX,32		; 32 sector per cluster is OK
;M001

CSET:
    ; Figure FAT size. AX is reasonable approx to number of DATA sectors
    ;  BX is reasonable sec/cluster
	XOR	DX,DX
	DIV	BX		; AX is total clusters, ignore remainder
				;  can't have a "partial" cluster
	MOV	CX,AX
	SHR	CX,1
	JNC	ADDIT
	INC	CX
ADDIT:
	ADD	AX,CX		; AX is Bytes for fat (1.5 * # of clusters)
	ADD	AX,3		; Plus two reserved clusters
	XOR	DX,DX
	DIV	[SSIZE] 	; AX is # sectors for a FAT this size
	OR	DX,DX
	JZ	NOINC4
	INC	AX		; Round up
NOINC4: 			; AX is # sectors for FAT
	XCHG	AX,[FATSEC]	; Set newly computed value
	XCHG	BL,[CSIZE]	; Set newly computed value
	dec	si		; have we looped enough?
	jz	homfin		; yes, time to get out
	CMP	BL,[CSIZE]	; Did we compute a different size?
	JNZ	CLUSHOME	; Keep performing FATSEC and CSIZE computation
				;   until the values don't change.
	CMP	AX,[FATSEC]	; Did we compute a different size?
	JNZ	CLUSHOME	; Keep performing FATSEC and CSIZE computation
				;   until the values don't change.
HOMFIN:
    ;
    ; 6.  Print out report of RAMDrive parameters
    ;
	MOV	DX,OFFSET STATMES1
	CALL	PRINT
	MOV	AX,[DEV_SIZE]
	CALL	ITOA
	MOV	DX,OFFSET STATMES2
	CALL	PRINT
	MOV	AX,[SSIZE]
	CALL	ITOA
	MOV	DX,OFFSET STATMES3
	CALL	PRINT
	MOV	AL,[CSIZE]
	XOR	AH,AH
	CALL	ITOA
	MOV	DX,OFFSET STATMES4
	CALL	PRINT
	MOV	AX,[DIRNUM]
	CALL	ITOA
	MOV	DX,OFFSET STATMES5
	CALL	PRINT
	CMP	[RESMEM_SPECIAL],0
	JZ	NO_RELOC
    ;
    ; We are in a special case. The RAMDrive driver area starts at DEVICE_END.
    ;  If we left this INIT code where it is and executed it the act of
    ;  Initializing the boot sector, FAT, and root directory would overwrite
    ;  this INIT code as we are executing it. So what we do is COPY this
    ;  code into the DATA area of the RAMDrive and execute it from there.
    ;
RAMDrive_RELOC:
	MOV	AX,1			; AX is sec # of start of FAT
	ADD	AX,[FATSEC]		; AX is sec # of start of directory
	ADD	AX,[DIRSEC]		; AX is sec # of start of DATA
	MUL	[SSIZE] 		; DX:AX is byte offset of start of DATA
	ADD	AX,WORD PTR [BASE_ADDR]
	ADC	DX,WORD PTR [BASE_ADDR + 2] ; DX:AX is 32 addr of first byte of DATA
	ADD	AX,15			; PARA round up
	ADC	DX,0
	MOV	CX,16
	DIV	CX			; AX is Seg addr of DATA region
    ;
    ; At this point we need to do a little check. We need to make
    ;	sure the distance between where we are now, and where we
    ;	are relocating to is AT LEAST as much as we are moving
    ;	so that we don't modify ourselves while we're moving
    ;
	MOV	BX,AX
	MOV	DX,CS
	SUB	BX,DX			; BX is para between segs
	CMP	BX,((OFFSET RAMDrive_END - OFFSET RAMDEV) + 15) / 16 ; CMP to para moving
	JAE	OKMOV			; Distance is enough
	MOV	AX,CS			; Move far enough away
	ADD	AX,((OFFSET RAMDrive_END - OFFSET RAMDEV) + 15) / 16
OKMOV:
	MOV	ES,AX
	XOR	SI,SI
	MOV	DI,SI
	MOV	CX,OFFSET RAMDrive_END	   ; Amount to move
	CLD
	REP	MOVSB			; Reloc to data region
	PUSH	ES			; Push FAR return
	MOV	AX,OFFSET NO_RELOC
	PUSH	AX
	PUSH	ES
	POP	DS			; DS is NEW RAMCODE
RELOCR	PROC	FAR
	RET
RELOCR	ENDP

NO_RELOC:
	PUSH	CS
	POP	ES
	XOR	DX,DX		; Sector 0
	MOV	CX,1		; One sector
	MOV	DI,OFFSET BOOT_SECTOR	; Boot sector
	MOV	BH,1		; Write
	CALL	INIMEMIO
	INC	DX		; First FAT sector
	MOV	DI,OFFSET SECTOR_BUFFER
	XOR	AX,AX
	MOV	CX,512
	CLD
	REP	STOSW
	MOV	DI,OFFSET SECTOR_BUFFER
	MOV	CX,1
	MOV	WORD PTR ES:[DI],0FFF8H
	MOV	BYTE PTR ES:[DI + 2],0FFH
	CALL	INIMEMIO
	INC	DX		; Next sector
	MOV	WORD PTR ES:[DI],0
	MOV	BYTE PTR ES:[DI + 2],0
	MOV	CX,[FATSEC]
	DEC	CX
	JCXZ	FATDONE
FATZERO:
	PUSH	CX
	MOV	CX,1
	CALL	INIMEMIO
	INC	DX		; Next sector
	POP	CX
	LOOP	FATZERO
FATDONE:
	MOV	CX,1
	MOV	DI,OFFSET VOLID
	CALL	INIMEMIO	; FIRST directory sector
	INC	DX
	MOV	CX,[DIRSEC]
	DEC	CX
	JCXZ	DRIVE_SET
	MOV	DI,OFFSET SECTOR_BUFFER
DIRZERO:
	PUSH	CX
	MOV	CX,1
	CALL	INIMEMIO
	INC	DX		; Next sector
	POP	CX
	LOOP	DIRZERO
;
DRIVE_SET:
;
;	BPB IS NOW ALL SET
;
	MOV	AL,1			;Number of ramdrives
;
;	NOTE FALL THROUGH!!!!!!!
;

;**	SETBPB - Set INIT packet I/O return values
;
;	This entry is used in ERROR situations to return
;	a unit count of 0 by jumping here with AL = 0.
;	The successful code path falls through to here
;	with AL = 1
;
;	ENTRY
;	    AL = INIT packet unit count
;	EXIT
;	    through DEVEXIT
;	USES
;	    DS, BX, CX
;
;	COMMON TO TYPE 1, 2, 3, 4 drivers
;

SETBPB:
ASSUME	DS:NOTHING
    ;
    ; 7.  Set the return INIT I/O packet values
    ;
	LDS	BX,[PTRSAV]
	MOV	[BX.INIT_NUM],AL
	MOV	CX,WORD PTR [TERM_ADDR]
	MOV	WORD PTR [BX.INIT_BREAK],CX		   ;SET BREAK ADDRESS
	MOV	CX,WORD PTR [TERM_ADDR + 2]
	MOV	WORD PTR [BX.INIT_BREAK + 2],CX
	MOV	WORD PTR [BX.INIT_BPB],OFFSET INITAB	   ;SET POINTER TO BPB ARRAY
	MOV	CX,[TRUE_CS]
	MOV	WORD PTR [BX.INIT_BPB + 2],CX
	JMP	DEVEXIT

;**	INIMEMIO call MEMIO but preserve registers
;
;	MEMIO is very register destructive, all this routine
;	does is provide a less destructive way to call MEMIO.
;
;	ENTRY
;	    Same as MEMIO
;	EXIT
;	    Same as MEMIO
;	USES
;	    AX, SI, BP
;
;	COMMON TO TYPE 1, 2, 3, 4 drivers
;

INIMEMIO:
ASSUME	DS:NOTHING,ES:NOTHING,SS:NOTHING
	PUSH	ES
	PUSH	DI
	PUSH	DS
	PUSH	CX
	PUSH	DX
	PUSH	BX
	CALL	MEMIO
	POP	BX
	POP	DX
	POP	CX
	POP	DS
	POP	DI
	POP	ES
	RET

;**	GETNUM - Read an unsigned integer
;
;	This routine looks at DS:SI for a decimal unsigned integer.
;	It is up to the caller to make sure DS:SI points to the start
;	of a number. If it is called without DS:SI pointing to a valid
;	decimal digit the routine will return 0. Any non decimal digit
;	defines the end of the number and SI is advanced over the
;	digits which composed the number. Leading "0"s are OK.
;
;	THIS ROUTINE DOES NOT CHECK FOR NUMBERS LARGER THAN WILL FIT
;	IN 16 BITS. If it is passed a pointer to a number larger than
;	16 bits it will return the low 16 bits of the number.
;
;	This routine uses the MUL instruction to multiply the running
;	number by 10 (initial value is 0) and add the numeric value
;	of the current digit. Any overflow on the MUL or ADD is ignored.
;
;	ENTRY:
;	     DS:SI -> ASCII text of number
;	EXIT:
;	     BX is binary for number
;	     SI advanced to point to char after number
;	USES:
;	     AX,BX,DX,SI
;
;	COMMON TO TYPE 1, 2, 3, 4 drivers
;

GETNUM:
ASSUME	DS:NOTHING,ES:NOTHING,SS:NOTHING

	XOR	BX,BX
GETNUM1:
	LODSB
	SUB	AL,"0"
	JB	NUMRET
	CMP	AL,9
	JA	NUMRET
	CBW
	XCHG	AX,BX
	MOV	DX,10
	MUL	DX
	ADD	BX,AX
	JMP	GETNUM1

NUMRET:
	DEC	SI
	RET


BREAK	<RAMDrive COMMON INIT ROUTINES>

;**	DISK_ABORT - De-install RAMDrive after init
;
;	This routine MUST BE CALLED to de-install a RAMDrive driver
;	if the de-installation takes place:
;
;		AFTER the INT 19 vector is replaced for TYPE 2
;		AFTER ABOVE_PID is valid for TYPE 2
;		AFTER XMS memory has been allocated for TYPE 1
;
;	NOTE: Since a TYPE 4 driver does NONE of the above things it is
;		not necessary to call this routine, but the routine is
;		designed so that it is OK to call for a TYPE 4 driver.
;
;	In all cases the INT 19 vector is replaced if the
;	value of both words of OLD_19 is NOT -1. This is why the initial value
;	of this datum is -1. In the event that the INT 19 vector
;	is replaced, this datum takes on some value other than -1.
;
;	If this is a TYPE 1 driver and we have allocated XMS memory
;	(true if ext_handle is NOT -1), the XMS memory block is deallocated.
;	Also, if we have hooked into the XMM control chain (true if the
;	doubleword at XmmControlBase is not -1), we unhook.
;
;	If this is a TYPE 2 driver, an ABOVE_DEALLOC call is made on
;	ABOVE_PID.
;
;	ENTRY:
;	    NONE
;
;	    ext_handle valid or -1 if TYPE 1
;	    ABOVE_PID valid if TYPE 2
;
;	EXIT:
;	    NONE
;	USES:
;	    ALL but DS
;
;	COMMON TO TYPE 1, 2, 3, 4 drivers
;

DISK_ABORT:
ASSUME	DS:RAMCODE,ES:NOTHING,SS:NOTHING

	CMP	[DRIVER_SEL],1
	JNZ	NOT_ABOVE
AGAIN:
    ;
    ; TYPE 2, Deallocate the Above Board memory
    ;
	MOV	DX,[ABOVE_PID]
	MOV	AH,ABOVE_DEALLOC
	INT	67H
	CMP	AH,ABOVE_ERROR_BUSY
	JZ	AGAIN
	JMP	SHORT RET002

NOT_ABOVE:
	CMP	[RESMEM_SPECIAL],0
	JNZ	RET002				; No EMM_CTRL on TYPE 4
    ;
    ; TYPE 1, Deallocate XMS memory
    ;
	mov	dx, [ext_handle]
	cmp	dx, -1
	je	xmsdealloc_skip			; XMS memory not allocated

	mov	ah, XMM_UNLOCK_EMB
	call	[XmmControl]
	mov	ah, XMM_FREE_EMB
	call	[XmmControl]
xmsdealloc_skip:
    ;
    ; Remove ourselves from the XMM control chain if XmmControlBase
    ; is not -1.
    ;
    ; We assume that no one has hooked us, since we're still in driver
    ; initialization.
    ;
	cmp	word ptr XmmControlBase,-1
	jne	UnhookXmm
	cmp	word ptr XmmControlBase+2,-1
	je	Ret002			; we didn't hook- don't unhook
UnhookXmm:
	mov	al,0EBh			; AL = opcode for short jump
	mov	ah,XmmControlJmpVal	; AH = displacement for short jump
	les	bx,XmmControlBase	; ES:BX = ptr to previous XMM header
	mov	word ptr es:[bx],ax	; restore previous XMM's short jump
	mov	word ptr es:[bx+2],9090h;  followed by nop's
	mov	byte ptr es:[bx+4],90h

RET002:
    ;
    ; Reset INT 19 if OLD_19 is not -1
    ;
	PUSH	DS
	LDS	DX,[OLD_19]
ASSUME	DS:NOTHING
	MOV	AX,DS
	CMP	AX,-1
	JNZ	RESET_VECS
	CMP	AX,DX
	JZ	NO_VECS
RESET_VECS:
	MOV	AX,(Set_Interrupt_Vector SHL 8) OR 19H
	INT	21H
NO_VECS:
	POP	DS
	RET

;**	CTRL_IO - Read/Write the first 1024 bytes at BASE_ADDR
;
;	This routine is used at INIT time to read the first 1024
;	bytes at BASE_ADDR. If TYPE 1 or TYPE 3 and BASE_ADDR points
;	to the EMM_CTRL address (initial value), the EMM_CTRL sector
;	is read/written. If TYPE 1 or TYPE 3 and BASE_ADDR has been set
;	to the start of a RAMDrive, the first 1024 bytes of the DOS volume
;	are read/written. If TYPE 2 or TYPE 4, the first 1024 bytes of
;	the DOS volume are read/written. All this routine does is
;	set inputs to BLKMOV to transfer 1024 bytes at offset 0 to/from
;	SECTOR_BUFFER.
;
;	ENTRY:
;	     BH = 0 for READ, 1 for WRITE
;	EXIT:
;	     SECTOR_BUFFER filled in with 1024 bytes at BASE_ADDR
;	USES:
;	     ALL but DS
;
;	COMMON TO TYPE 1, 2, 3, 4 drivers
;

CTRL_IO:
ASSUME	DS:RAMCODE,ES:NOTHING,SS:NOTHING
	XOR	DX,DX
	MOV	AX,DX		; Offset 0
	MOV	CX,512		; 1024 bytes
	PUSH	CS
	POP	ES
	MOV	DI,OFFSET SECTOR_BUFFER
	PUSH	DS
	CALL	BLKMOV		; Read in EMM_CTRL
	POP	DS
	RET


;**	CHECK_DOS_VOL  examine RAMDrive region for valid DOS volume.
;
;	This routine is used by TYPE 1, 2 and 3 drivers to check and see
;	if the RAMDrive memory contains a valid DOS volume (one that lived
;	through a re-boot). Its prime job is to set INIT_DRIVE to indicate
;	whether the DOS volume needs to be initialized.
;
;	First the first 1024 bytes of the drive are read in to SECTOR_BUFFER
;	Next we check for a match of the signature areas up at BOOT_SECTOR
;	  to see if this drive contains a VALID RAMDrive boot record.
;	IF the signatures are valid AND INIT_DRIVE != 2 (ignore valid signature)
;		We check to make sure that SSIZE and DIRNUM set by the user
;		match the values in the BPB we just found.
;		IF they match
;		    we set INIT_DRIVE to 0 (don't init)
;		    and transfer the BPB out of the boot sector on the drive
;		    (in SECTOR_BUFFER) into the BPB for this driver at
;		    RDRIVEBPB.
;		ELSE
;		    Leave INIT_DRIVE set to whatever it was on input (1 or 2)
;		    indicating that the drive must be INITed.
;	ELSE
;		Leave INIT_DRIVE set to whatever it was on input (1 or 2)
;		indicating that the drive must be INITed.
;
;	WARNING! This routine DOES NOT check to make sure that the size of
;		the device as indicated in the BPB transfered in if a valid
;		DOS volume is found is consistent with the actual size
;		of the memory allocated to the device (DEV_SIZE). It
;		is up to the caller to check this if so desired.
;
;	ENTRY:
;	    BASE_ADDR set to point at START of DOS device
;	    Except for TYPE 1, which uses ext_handle
;	EXIT:
;	    CARRY SET - error, message already printed
;	    CARRY CLEAR
;		INIT_DRIVE set
;		SECTOR_BUFFER contains first 1024 bytes of device
;	USES:
;	    All but DS
;
;	Used by TYPE 1, 2 and 3 drivers
;

CHECK_DOS_VOL:
ASSUME	DS:RAMCODE,ES:NOTHING,SS:NOTHING
	XOR	BH,BH		; READ
    ;
    ; NOTE: WE CANNOT CALL MEMIO, WE MUST STILL USE CTRL_IO because the BPB
    ;	 is not set up.
    ;
	CALL	CTRL_IO 	; Since BASE_ADDR is set, reads start of DEVICE
	MOV	DX,OFFSET INIT_IO_ERR
	JC	ERR_RET2
	PUSH	CS
	POP	ES
	MOV	DI,OFFSET SECTOR_BUFFER
	MOV	SI,OFFSET BOOT_SECTOR
	MOV	CX,OFFSET RDRIVEBPB - OFFSET BOOT_SECTOR
	CLD
	REPE	CMPSB
	JNZ	OK_RET		; No DOS device
	ADD	DI,OFFSET BOOT_START - OFFSET RDRIVEBPB
	ADD	SI,OFFSET BOOT_START - OFFSET RDRIVEBPB
	MOV	CX,OFFSET BOOT_END - OFFSET BOOT_START
	REPE	CMPSB
	JNZ	OK_RET		; No DOS device
	CMP	[INIT_DRIVE],2
	JZ	NOT_VALID		; Current value 2 means we CANNOT
					; assume this BPB is valid.
    ;
    ; Check to make sure found BPB has same SSIZE and DIRNUM values
    ;
	MOV	SI,OFFSET SECTOR_BUFFER + (OFFSET SSIZE - OFFSET BOOT_SECTOR)
	LODSW
	CMP	AX,[SSIZE]
	JNZ	NOT_VALID		; Sector size different than user request
	MOV	SI,OFFSET SECTOR_BUFFER + (OFFSET DIRNUM - OFFSET BOOT_SECTOR)
	LODSW
	CMP	AX,[DIRNUM]
	JNZ	NOT_VALID		; Sector size different than user request

	MOV	[INIT_DRIVE],0		; Found a DOS drive
	MOV	DI,OFFSET RDRIVEBPB
	MOV	SI,OFFSET SECTOR_BUFFER + (OFFSET RDRIVEBPB - OFFSET BOOT_SECTOR)
	MOV	CX,OFFSET BOOT_START - OFFSET RDRIVEBPB
	REP	MOVSB			; Set correct BPB
NOT_VALID:
OK_RET:
	CLC
	RET

ERR_RET2:
	CALL	PRINT
	STC
	RET

;**	SET_RESET - Set up INT 19/INT 9 vectors
;
;	This routine will do nothing if BX is non-zero
;	otherwise it will install the INT 19
;	code by saving the current INT 19
;	vector in OLD_19 (NOTE: the change in the value of OLD_19
;	to something other than -1 indicates that the vector has been
;	replaced), setting the vector to point to INT_19,
;	and adjusting TERM_ADDR to include the code as part of the resident
;	image.
;
;	ENTRY:
;	     BX is 0 if INT 19 code to be installed
;	EXIT:
;	     NONE
;	USES:
;	     None
;
;	COMMON TO TYPE 1, 2, 3, 4 drivers (?)
;

SET_RESET:
ASSUME	DS:RAMCODE,ES:NOTHING,SS:NOTHING
	OR	BX,BX
	JNZ	RET005
	PUSH	AX
	PUSH	DX
	PUSH	BX
	PUSH	ES
	MOV	AX,(Get_Interrupt_Vector SHL 8) OR 19H
	INT	21H
	MOV	WORD PTR [OLD_19],BX
	MOV	WORD PTR [OLD_19 + 2],ES
	MOV	DX,OFFSET INT_19
	MOV	AX,(Set_Interrupt_Vector SHL 8) OR 19H
	INT	21H
	MOV	WORD PTR [TERM_ADDR],OFFSET RESET_INCLUDE
	POP	ES
	POP	BX
	POP	DX
	POP	AX
RET005:
	RET


	page
;******************************************************************************
;	check_XMM: routine to check presence of XMM driver
;
;	ENTRY:	DS = INT13CODE
;	EXIT:	carry set if error occurred
;	USED:	none
;
;******************************************************************************
check_XMM	proc	near
		assume ds:RAMCODE, es:NOTHING, ss:NOTHING
;
; determine whether or not an XMM driver is installed
;
	push	ax
	mov	ax,XMM_MULTIPLEX SHL 8 + XMM_INSTALL_CHECK
	int	2Fh
	cmp	al,80h			; Q: installed
	jne	cXMM_no_driver		;   N: set error, quit
;
; get the XMM control functions entry point, save it, we
; need to call it later.
;
	push	es
	push	bx
	mov	ax,XMM_MULTIPLEX SHL 8 + XMM_FUNCTION_ADDR
	int	2Fh
;	push	ds
;	push	seg INT13CODE
;	pop	ds
	mov	word ptr [XMMcontrol], bx
	mov	word ptr [XMMcontrol+2],es
;	pop	ds
	pop	bx
	pop	es
	pop	ax
	clc
	ret				; done
;
; flag error : XMM driver not present
;
cXMM_no_driver:
	stc
	pop	ax
	ret

check_XMM	endp



BREAK	</E INIT Code>

;**	EXT_INIT - Perform /E (TYPE 1) specific initialization
;
;	This code does the drive TYPE specific initialization for TYPE 1
;	drivers.
;
;	Determine if XMS driver is installed.
;	If no XMS driver answers, cough politely and don't install.
;	Determine extended memory available.
;	If not enough memory is available, sniff and don't install.
;	Allocate extended memory via XMS driver.
;	Place ourselves in XMM control chain to watch for Windows/386 2.x.
;	If the previous control chain is invalid, flatulate and don't install.
;
;	ENTRY:
;	    Invokation line parameter values set.
;	EXIT:
;	    CARRY SET
;		Error, message already printed. Driver not installed.
;	    CARRY CLEAR
;		DEV_SIZE set to TRUE size
;		INIT_DRIVE set appropriatly
;		TERM_ADDR set to correct device end.
;		    RESET_SYSTEM code included if this is the first
;		    TYPE 1 RAMDrive in the system.
;
;	USES:
;	    ALL but DS
;
;	Code is specific to TYPE 1 driver
;

EXT_INIT:
ASSUME	DS:RAMCODE,ES:NOTHING,SS:NOTHING

	IF DEBUG
	JMP	SHORT DEBINIT
DEBINITMES DB	"Initializing /e version",13,10,"$"
DEBINIT:
	PUSH	DX
	PUSHF
	PUSH	AX
	MOV	DX,OFFSET DEBINITMES
	CALL	PRINT
	POP	AX
	POPF
	POP	DX
	ENDIF

;	XMM init code taken from smartdrv.asm (written by Harish?) --

;
;	Check for presence of XMM driver.
;
	call	check_xmm
	jnc	xmm_ok
	mov	dx, offset noxmm
	jmp	err_ret
xmm_ok:

;
;	Get extended memory in K size into AX
;

	mov	ah, XMM_QUERY_FREE_EXTMEM
	call	[XmmControl]

	or	ax, ax 			; is there any ext. memory?
	jnz	mem_avail		; yes.
	test	bl, XMM_ERROR_BIT	; error code returned?
	jz	no_mem_avail		; no.  no memory available
	mov	dx, offset errxmm	; yes.  problem from xmm
	jmp	err_ret

no_mem_avail:
	mov	dx, offset no_mem
	jmp	err_ret

mem_avail:
	mov	dx, offset errmsg2
        cmp     ax, 4                   ; M002: available memory >= 4K?
	jnb	@F			;  -yes, jump.
	jmp	err_ret			;  -no, memory is below minimum.
@@:
	mov	[ext_k], ax
	cmp	[dev_size], ax		; is there enuff mem for cache?
	jbe	dev_size_ok		; yes.
	mov	[dev_size], ax		; no - limit dev_size to size available
dev_size_ok:


	mov	dx, [dev_size]		; dx = amt. of mem requested in K
	mov	ah, XMM_ALLOC_EMB	; allocate dx K of extended mem.
	call	[XmmControl]
	or	ax, ax
	jnz	alloc_ok
	mov	dx, offset errxmm
	jmp	short err_ret
alloc_ok:

;	dx has handle for extended memory block.

	mov	[ext_handle], dx

;	Get in XMM control chain to watch for Windows/386 v2.x.

	mov	bx,word ptr XmmControl
	mov	es,word ptr XmmControl+2  ; ES:BX = ptr to 1st XMM header
NextXmmHeader:
	mov	word ptr PrevXmm+2,es		; save seg of prev control adr
	mov	word ptr XmmControlBase+2,es
	mov	word ptr XmmControlBase,bx
	mov	cx,word ptr es:[bx]
	cmp	cl,0EBh				; compare short jmp opcode
	je	ShortJmp
	cmp	cl,0EAh				; compare far jmp opcode
	jne	XmmChainHosed			; bad XMM control chain
FarJmp:
	mov	si,word ptr es:[bx+1]		; SI = offset of jmp
	mov	es,word ptr es:[bx+1+2]		; ES = segment of jmp
	mov	bx,si
	jmp	NextXmmHeader			; continue down control chain
ShortJmp:
	cmp	word ptr es:[bx+2],9090h	; check NOPs
	jne	XmmChainHosed			; bad XMM control chain
	cmp	byte ptr es:[bx+4],90h
	jne	XmmChainHosed			; bad XMM control chain
	mov	di,bx				; DI = ptr to XMM header
	xor	ax,ax
	mov	al,ch				; AX = offset of short jmp
	mov	XmmControlJmpVal,al		; save offset of short jmp
	add	ax,2				; add length of jmp instr
	add	bx,ax				; BX = target of jmp
	mov	word ptr PrevXmm,bx		; save previous control addr

;	Install ourselves in XMM control chain.

	mov	byte ptr es:[di],0EAh		; far immediate jmp opcode
	mov	word ptr es:[di+1],offset XmmGuard
	mov	word ptr es:[di+3],cs

	jmp	short ext_init_done

XmmChainHosed:
	mov	dx,offset XmmChain	; DX = ptr to bad chain msg

err_ret:
	call	print
	stc

ext_init_done:
	ret



BREAK	</A INIT Code>

;**	EMM device driver name
;
;	The following datum defines the Above Board EMM 8 character
;	device driver name that is looked for as part of TYPE 2
;	specific initialization.
;
;	This datum is specific to TYPE 2 drivers
;

ABOVE_DEV_NAME	DB	"EMMXXXX0"

;**	ABOVE_INIT - Perform /A (TYPE 2) specific initialization
;
;	This code performes the driver specific initialization for
;	type 2 drivers.
;
;	Swap ABOVE_BLKMOV code in for TYPE 1 code at BLKMOV
;	Swap ABOVE_RESET code in for TYPE 1 code at RESET_SYSTEM
;	Check to make sure EMM Above Board device driver is installed
;		by looking for device name relative to INT 67H segment
;		address. This is method 2 described on page 36 and 37
;		of the Expanded Memory Manager Programming Specification.
;
;		WARNING! If run on a version of DOS where all INT vectors
;		are managed by the kernel, or on a system where some
;		foreign program (not EMM.SYS) is also using INT 67H, this
;		method will fail to find the EMM device driver.
;		The reason this method was used rather than the more portable
;		method 1 described on pages 33 and 34 of the EMM Programming
;		Specification is that the DOS Installable Device Driver
;		document makes a statement about which DOS system calls
;		may be made in a device initialization routine, and
;		OPEN, IOCTL, and CLOSE are not included in the allowed
;		set. Adherance to the Installable Device Driver document,
;		therefore, excludes the use of method 1.
;
;	Check the EMM device status
;	Make sure the page frame really exists.  The first four EMS 
;		physical pages must be contiguous.  (M00)
;	Get the EMM map window address and set BASE_ADDR
;	Get the available Above Board memory
;	Adjust DEV_SIZE to be consistent with the available memory if needed,
;		and also round DEV_SIZE up so that it is a multiple of the 16K
;		granularity of the Above Board memory.
;	Allocate DEV_SIZE worth of Above Board memory and set ABOVE_PID.
;		After this point we can use CTRL_IO and/or BLKMOV to
;		read/write the memory we have allocated.
;	Install the INT 9 and INT 19 code by calling SET_RESET with BX = 0.
;	Adjust the TERM_ADDR set by SET_RESET to a more appropriate size.
;	Call CHECK_DOS_VOL to look for a DOS volume and set INIT_DRIVE.
;	IF INIT_DRIVE indicates that a DOS volume was found
;		Check to make sure that the size of the found DOS
;		volume is consistent with DEV_SIZE.
;		IF it is not
;			Set INIT_DRIVE to 2 to indicate that the found volume
;				is invalid and needs to be re-initialized.
;
;	SEE ALSO
;	    INTEL Expanded Memory Manager Programming Specification
;
;	ENTRY:
;	    Invokation line parameter values set.
;	EXIT:
;	    ABOVE_BLKMOV code swapped in at BLKMOV
;	    ABOVE_RESET code swapped in at RESET_SYSTEM
;	    CARRY SET
;		Error, message already printed. Driver not installed.
;			No Above Board memory allocated.
;	    CARRY CLEAR
;		BASE_ADDR set to segment address of Above Board map window
;		ABOVE_PID contains PID of allocated above board memory
;		DEV_SIZE set to TRUE size
;		INIT_DRIVE set appropriatly
;		TERM_ADDR set to correct device end.
;		    RESET_SYSTEM code and INT 9/INT 19 code included.
;
;	USES:
;	    ALL but DS
;
;	Code is specific to TYPE 2 driver
;

ABOVE_INIT:
ASSUME	DS:RAMCODE,ES:NOTHING,SS:NOTHING
    ;
    ; Swap above code into place
    ;
	PUSH	CS
	POP	ES
	MOV	SI,OFFSET ABOVE_CODE
	MOV	DI,OFFSET DRIVE_CODE
	MOV	CX,OFFSET DRIVE_END - OFFSET DRIVE_CODE
	REP	MOVSB
	MOV	SI,OFFSET ABOVE_RESET
	MOV	DI,OFFSET RESET_SYSTEM
	MOV	CX,OFFSET RESET_INCLUDE - OFFSET RESET_SYSTEM
	REP	MOVSB
    ;
    ; Check for presence of Above board memory manager
    ;
	MOV	AX,(Get_Interrupt_Vector SHL 8) OR 67H
	INT	21H
	MOV	DI,SDEVNAME
	MOV	SI,OFFSET ABOVE_DEV_NAME
	MOV	CX,8
	REPE	CMPSB
	JZ	GOT_MANAGER
	MOV	DX,OFFSET NO_ABOVE
ABOVE_ERR:
	CALL	PRINT
	STC
	RET

GOT_MANAGER:
    ;
    ; Check memory status
    ;
	MOV	CX,8000H
STLOOP:
	MOV	AH,ABOVE_STATUS
	INT	67H
	CMP	AH,ABOVE_SUCCESSFUL
	JZ	CHECK_PAGEFRAME		;M00
	CMP	AH,ABOVE_ERROR_BUSY
	LOOPZ	STLOOP
ST_ERR:
	MOV	DX,OFFSET BAD_ABOVE
	JMP	ABOVE_ERR

;	BEGIN M00 CODE

CHECK_PAGEFRAME:
    ;
    ; Make sure page frame really exists
    ;
GET_EMSVER:
	MOV	AH,ABOVE_GET_VERSION
	INT	67H
	CMP	AH,ABOVE_ERROR_BUSY
	JZ	GET_EMSVER
	CMP	AH,ABOVE_SUCCESSFUL
	JNZ	ST_ERR
	CMP	AL,40H
	JB	MEM_OK			; version < 4.0, page frame is ok

GET_PAGEMAP:
    ;
    ; Get addresses of mappable pages
    ;
	MOV	AX,ABOVE_GET_ADDR_MAP
	PUSH	DS
	POP	ES
	MOV	DI,OFFSET SECTOR_BUFFER	; ES:DI = ptr to buffer
	MOV	SI,DI			; DS:SI = ptr to buffer, too
	INT	67H
	CMP	AH,ABOVE_SUCCESSFUL
	JNZ	ST_ERR
	CLD

SCAN_PAGEMAP:
    ;
    ; Look for page 0
    ;
	LODSW				; AX = page segment address
	LODSW				; AX = physical page number
	OR	AX,AX
	JZ	CHECK_CONTIG		; found page 0, check pages 0-3
	LOOP	SCAN_PAGEMAP		; keep looking

CHECK_CONTIG:
    ;
    ; Make sure pages 0-3 are contiguous
    ;
	MOV	DX,AX			; DX = page # = 0
	MOV	BX,[SI-4]		; BX = page 0 segment address
	MOV	CX,3			; CX = # pages to check

NEXT_CONTIG:
    ;
    ; Note page address array is in order by segment address
    ;
	INC	DX			; DX = next expected page #
	ADD	BX,0400H		; BX = next expected seg addr
	LODSW				; AX = next segment address
	CMP	AX,BX
	JNE	ST_ERR			; segment address not contiguous
	LODSW				; AX = next page #
	CMP	AX,DX
	JNE	ST_ERR			; page # not contiguous
	LOOP	NEXT_CONTIG		; check rest of page frame pages

;	END M00 CODE

MEM_OK:
    ;
    ; Get base address of map region and set BASE_ADDR
    ;
	MOV	AH,ABOVE_GET_SEG
	INT	67H
	CMP	AH,ABOVE_ERROR_BUSY
	JZ	MEM_OK
	CMP	AH,ABOVE_SUCCESSFUL
	JNZ	ST_ERR
	MOV	WORD PTR [BASE_ADDR],0
	MOV	WORD PTR [BASE_ADDR + 2],BX
    ;
    ; Allocate drive memory
    ;
GET_AVAIL:
	MOV	AH,ABOVE_GET_FREE
	INT	67H
	CMP	AH,ABOVE_ERROR_BUSY
	JZ	GET_AVAIL
	CMP	AH,ABOVE_SUCCESSFUL
	JNZ	ST_ERR
	MOV	AX,DX		; AX is total 16K pages
				; BX is un-allocated 16K pages
	MOV	DX,OFFSET NO_MEM
	OR	AX,AX
	JNZ	@F
	JMP	ABOVE_ERR
@@:	MOV	DX,OFFSET ERRMSG2
	OR	BX,BX		; 16k is min. (expanded mem.) Ramdrive
	JNZ	@F
	JMP	ABOVE_ERR
@@:	TEST	BX,0F000H
	JNZ	AB001		; Avialable K is REAL big
	MOV	CX,4
	SHL	BX,CL		; BX is un-allocated K
	CMP	[DEV_SIZE],BX
	JBE	AB001		; DEV_SIZE OK
	MOV	[DEV_SIZE],BX	; Limit DEV_SIZE to available
AB001:
	MOV	BX,[DEV_SIZE]

; M002 (updated comment only)
    ;
    ; BX = drive size (Kbytes) requested (adjusted to be no greater than
    ;      available expanded memory).
    ; The drive size is now rounded up to the next multiple of 16K (since the
    ; granularity of expanded memory is 16K).
    ;
	MOV	AX,BX
	MOV	CX,4		; Convert back to # of 16K pages
	SHR	BX,CL
	TEST	AX,0FH		; Even????
	JZ	OKAYU		; Yes
	INC	BX		; Gotta round up
	PUSH	BX
	MOV	CX,4
	SHL	BX,CL
	MOV	[DEV_SIZE],BX	; Correct dev size too by rounding it up to
				;   next multiple of 16K, no sense wasting
				;   part of a page.
	POP	BX
OKAYU:
	MOV	AH,ABOVE_ALLOC
	INT	67H
	CMP	AH,ABOVE_ERROR_BUSY
	JZ	OKAYU
	CMP	AH,ABOVE_SUCCESSFUL
	JZ	GOT_ID
	CMP	AH,ABOVE_ERROR_MAP_CNTXT
	JZ	ST_ERRJ
	CMP	AH,ABOVE_ERROR_OUT_OF_PIDS
	JB	ST_ERRJ
	MOV	DX,OFFSET ERRMSG2
	JMP	ABOVE_ERR

ST_ERRJ:
	JMP	ST_ERR

GOT_ID:
	MOV	[ABOVE_PID],DX
    ;
    ; INSTALL ABOVE RESET handler
    ;
	XOR	BX,BX
	CALL	SET_RESET
    ;
    ; The above RESET_SYSTEM handler is real small, and since we include it in
    ;	EACH driver, we make sure the size is minimal
    ;
	MOV	WORD PTR [TERM_ADDR],OFFSET RESET_SYSTEM + (OFFSET ABOVE_RESET_END - OFFSET ABOVE_RESET)
    ;
    ; We are now in good shape. Can call BLKMOV to read drive
    ;
	CALL	CHECK_DOS_VOL		; Snoop for DOS volume
	JNC	DOUBLE_CHECK
	CALL	DISK_ABORT
	STC
	RET

DOUBLE_CHECK:
	CMP	[INIT_DRIVE],0
	JNZ	RETAB			; No DOS volume found
    ;
    ; We MUST check to see if the FOUND DOS volume is consistent
    ;	with DEV_SIZE.
    ;
	MOV	AX,[SECLIM]
	MUL	[SSIZE] 		; DX:AX is size of volume in bytes
	MOV	CX,1024
	DIV	CX			; AX is size in K
	CMP	AX,[DEV_SIZE]
	JE	RETAB			; Volume is OK
RE_INIT:
	MOV	[INIT_DRIVE],2		; Force re-compute of volume
RETAB:
	CLC
	RET

BREAK	<Drive code for /A driver. Swapped in at BLKMOV>

;
; This label defines the start of the code swapped in at DRIVE_CODE
;
ABOVE_CODE	LABEL	WORD

;
; WARNING DANGER!!!!!!!
;
; This code is tranfered over the /E driver code at DRIVE_CODE
;
; ALL jmps etc. must be IP relative.
; ALL data references must be to cells at the FINAL, TRUE location
;	(no data cells may be named HERE, must be named up at BLKMOV).
; OFFSET of ABOVE_BLKMOV relative to ABOVE_CODE MUST be the same as
;	the OFFSET of BLKMOV relative to DRIVE_CODE.
; SIZE of stuff between ABOVE_CODE and ABOVE_END MUST be less than
;	or equal to size of stuff between DRIVE_CODE and DRIVE_END.

IF2
  IF((OFFSET ABOVE_BLKMOV - OFFSET ABOVE_CODE) NE (OFFSET BLKMOV - OFFSET DRIVE_CODE))
	  %out ERROR BLKMOV, ABOVE_BLKMOV NOT ALIGNED
  ENDIF
  IF((OFFSET ABOVE_END - OFFSET ABOVE_CODE) GT (OFFSET DRIVE_END - OFFSET DRIVE_CODE))
	  %out ERROR ABOVE CODE TOO BIG
  ENDIF
ENDIF

		DD	?	; 24 bit address of start of this RAMDRV
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;**	ABOVE_BLKMOV - Perform transfer for TYPE 2 driver
;
;	This routine is the transfer routine for moving bytes
;	to and from the Above Board memory containing the cache.
;
;	The Above Board is implemented as 4 16K windows into the Above
;	Board memory, giving a total window of 64K wich starts on some
;	16K boundary of the Above Board memory. Given that a DOS I/O
;	request is up to 64K bytes starting on some sector boundary,
;	the most general I/O picture is:
;
;	|------------|------------|------------|------------|------------|
;	| Above Brd  | Above Brd  | Above Brd  | Above Brd  | Above Brd  |
;	|Log page n  |Log page n+1|Log page n+2|log page n+3|Log page n+4|
;	|------------|------------|------------|------------|------------|
;	|---|---|					    |	    |
;	|   |	|---------------- 64K bytes of sectors -------------|
;	   Byte |					    |	    |
;	  offset|------------------|------------------------|	    |
;	of first|	       Number of words in	    |	    |
;	byte of |	       first part of I/O that	    |---|---|
;	I/O in	|	       can be performed once	      Number
;	first	|	       logical pages n - n+3	      of words
;	Log page|	       are mapped into physical       in tail
;		|	       pages 0 - 3		      part of I/O
;	     Location of				      that have
;	     first byte 				      to be done
;	     of sector M,				      once logical
;	     the start sector				      page n+4 is
;	     of the I/O 				      mapped into
;							      physical page
;							      0
;
; One or both of "Byte offset of first byte of I/O in first page" and
; "Number of words in tail part of I/O" may be zero depending on the
; size of the I/O and its start offset in the first logical page it is
; possible to map.
;
; WARNING: IF A PRE-EMPTIVE MULTITASKING SYSTEM SCHEDULES A TASK WHICH
;	IS USING THE ABOVE BOARD DURING THE TIME THIS DRIVER IS IN THE
;	MIDDLE OF PERFORMING AN I/O, THE SYSTEM HAD BETTER MANAGE THE A
;	BOARD MAPPING CONTEXT CORRECTLY OR ALL SORTS OF STRANGE UNPLEASANT
;	THINGS WILL OCCUR.
;
;	SEE ALSO
;	    INTEL Expanded Memory Manager Programming Specification
;
;	ENTRY:
;	    ES:DI is packet transfer address.
;	    CX is number of words to transfer.
;	    DX:AX is 32 bit start byte offset (0 = start of cache)
;	    BH is 1 for WRITE, 0 for READ
;
;	    BASE_ADDR set to point to Above Board mapping window in main memory
;		This "input" is not the responsibility of the caller. It
;		is up to the initialization code to set it up when the
;		device is installed
;
;	EXIT:
;	    Carry Clear
;		    OK, operation performed successfully
;	    Carry Set
;		    Error during operation, AL is error number
;
;	USES:
;	    ALL
;
;	This routine is specific to TYPE 2 driver
;
;	sunilp - note that this has one limitation. in the case where
;		 one is using the above board for ramdrive and for
;		 the buffer then one is limited to 32k byte transfers
;
;	tonyg	- above limitation removed - now handles 64kb transfers
;		  which can overlap the page frame
;
above_blkmov:
assume ds:ramcode,es:nothing,ss:nothing
;
;	save mapping context and return with error if save fails
;
	save_mapping_context
	jnc	ab_blk$1
	ret
;
;	find logical page number, offset of i/o in first page
;
ab_blk$1:
	push	cx
	mov	cx,1024*16	; 16k bytes / page
	div	cx		; dx:ax / 16k --> log page numb in ax
				; 	      --> offset of i/o in dx
	mov	si,dx		; transfer offset to si
	mov	dx,ax		; store the page number in dx
	pop	cx
;
;	find case and dispatch accordingly
;
;	case 0 : user buffer below page map, can use aaron's code
;	case 1 : user buffer above page map, can use aaron's code
;	case 2 : user buffer partly/totally in page map, use pai's code
;
	push	bx
	push	cx
;
;	if( final_user_off < pm_base_addr ) then case 0
;
	mov	ax,di		; get user buffer initial offset into ax
	add	ax,1		; round up (add to get carry)
	rcr	ax,1		; convert to word offset
	dec	cx		; convert word count to 0 based number
	add	ax,cx		; user buffer final word offset
	shr	ax,1		; convert to segment
	shr	ax,1		;
	shr	ax,1		;
	mov	bx,es		; get segment of buffer
	add	ax,bx		; now we have the last segment of the user buffer
				; with offset < 16
	sub	ax,word ptr [base_addr+2] ; compare against page map
	jc	aar_cd		; if end below page map then execute old code
;
;	if( initial_user_off < pm_base_addr ) then case 2
;	
	mov	cx,4
	mov	bp,di		; get initial segment in bp
	shr	bp,cl		;
	add	bp,bx		;
	sub	bp,word ptr [base_addr +2]
	jc	within_pm	; case 2
;
;	if ( initial_user_off >= pm_end_addr ) then case1
;
	cmp	bp,4*1024	;
	jae	aar_cd		; case 1
;
;	case 2
;
within_pm:	jmp	new_code	; user buffer in page map
					; so we need to execute new code
aar_cd:
	pop	cx
	pop	bx
;	
; Referring back to the diagram given above the following routine is
; to take care of transfer of the most general case.
; What this routine does is break every I/O down into the above parts.
; The first or main part of the I/O is performed by mapping 1 to 4
; sequential logical pages into the 4 physical pages and executing one
; REP MOVSW. If the tail word count is non-zero then the fith sequential
; logical page is mapped into physical page 0 and another REP MOVSW is
; executed.
;
;	METHOD:
;	    Break I/O down as described above into main piece and tail piece
;	    Map the appropriate number of sequential pages (up to 4)
;	      into the page window at BASE_ADDR to set up the main piece
;	      of the I/O.
;	   Set appropriate seg and index registers and CX to perform the
;	      main piece of the I/O into the page window
;	   REP MOVSW
;	   IF there is a tail piece
;		Map the next logical page into physical page 0
;		Reset the appropriate index register to point at phsical page 0
;		Move tail piece word count into CX
;		REP MOVSW
;	   Restore Above Board page mapping context
;
	XOR	BP,BP		; No tail page
	PUSH	BX
    ;
    ; DX is first page #, SI is byte offset of start of I/O in first page
    ;
	MOV	AX,DX
	MOV	BX,SI
	SHR	BX,1		; # Words in first 16k page which are not part
				;	of I/O
	PUSH	CX
	ADD	BX,CX		; # of words we need to map to perform I/O
	MOV	DX,BX
	AND	DX,1FFFH	; DX is number of words to transfer last page
				;    remainder of div by words in 16K bytes
	MOV	CL,13		; Div by # words in 16K
	SHR	BX,CL		; BX is number of pages to map (may need round up)
	OR	DX,DX		; Remainder?
	JZ	NO_REM
	INC	BX		; Need one more page
NO_REM:
	MOV	CX,BX		; CX is total pages we need to map
	MOV	BX,AX		; BX is first logical page
	CMP	CX,4		; We can map up to 4 pages
	JBE	NO_TAIL
	MOV	BP,DX		; Words to move in tail page saved in BP
	DEC	CX		; Need second map for the 5th page
	POP	AX
	SUB	AX,DX		; Words to move in first 4 pages is input
				;   word count minus words in tail page
	PUSH	AX		; Count for first mapping back on stack
NO_TAIL:
    ; Map CX pages
	MOV	DX,[ABOVE_PID]
	MOV	AX,ABOVE_MAP SHL 8 ; Physical page 0
	PUSH	AX
MAP_NEXT:
	POP	AX		; Recover correct AX register
	PUSH	AX
	PUSH	BX
	PUSH	DX
	INT	67H		; Damn call ABOVE_MAP zaps BX,DX,AX
	POP	DX
	POP	BX
	OR	AH,AH
	JNZ	MAP_ERR1	; error
IF2
	IF (ABOVE_SUCCESSFUL)
		%out ASSUMPTION IN CODE THAT ABOVE_SUCCESSFUL = 0 IS INVALID
	ENDIF
ENDIF
NEXT_PAGE:
	INC	BX		; Next logical page
	POP	AX
	INC	AL		; Next physical page
	PUSH	AX
	LOOP	MAP_NEXT
	POP	AX		; Clean stack
	POP	CX		; Word count for first page mapping
	POP	AX		; Operation in AH
    ;
    ; BX has # of next logical page (Tail page if BP is non-zero)
    ; BP has # of words to move in tail page (0 if no tail)
    ; CX has # of words to move in current mapping
    ; SI is offset into current mapping of start of I/O
    ; AH indicates READ or WRITE
    ;
	PUSH	AX		; Save op for possible second I/O
	OR	AH,AH
	JZ	READ_A
    ;
    ; WRITE
    ;
	PUSH	ES
	PUSH	DI
	MOV	DI,SI		; Start page offset to DI
	POP	SI		; DS:SI is transfer addr
	POP	DS
ASSUME	DS:NOTHING
	MOV	ES,WORD PTR [BASE_ADDR + 2] ; ES:DI -> start
	JMP	SHORT FIRST_MOVE

READ_A:
ASSUME	DS:ramcode
	MOV	DS,WORD PTR [BASE_ADDR + 2]	; DS:SI -> start
ASSUME	DS:NOTHING
FIRST_MOVE:
	REP	MOVSW
	OR	BP,BP		; Tail?
	JNZ	TAIL_IO 	; Yup
ALL_DONE:
	POP	AX
	CLC
REST_CONT:
    ; Restore page mapping context
	PUSH	AX		; Save possible error code
	PUSHF			; And carry state
REST_AGN:
	MOV	DX,[ABOVE_PID]
	MOV	AH,ABOVE_RESTORE_MAP_PID
	INT	67H
	OR	AH,AH
	JZ	ROK
IF2
	IF (ABOVE_SUCCESSFUL)
		%out ASSUMPTION IN CODE THAT ABOVE_SUCCESSFUL = 0 IS INVALID
	ENDIF
ENDIF
	CMP	AH,ABOVE_ERROR_BUSY
	JZ	REST_AGN
	CMP	AH,ABOVE_ERROR_NO_CNTXT
	JZ	ROK		; Ignore the invalid PID error
	POP	DX
	POP	DX		; Clean stack
	MOV	AL,0cH	       ; General failure
	STC
	RET

ROK:
	POPF			; Recover carry state
	POP	AX		; and possible error code
	RET

TAIL_IO:
	MOV	DX,[ABOVE_PID]
MAP_AGN:
	MOV	AX,ABOVE_MAP SHL 8 ; map logical page BX to phys page 0
	PUSH	BX
	PUSH	DX
	INT	67H		; Damn call ABOVE_MAP zaps BX,DX,AX
	POP	DX
	POP	BX
	OR	AH,AH
	JNZ	MAP_ERR2	; Error
IF2
	IF (ABOVE_SUCCESSFUL)
		%out ASSUMPTION IN CODE THAT ABOVE_SUCCESSFUL = 0 IS INVALID
	ENDIF
ENDIF
SECOND_MOVE:
	POP	AX		; Recover Op type
	PUSH	AX
	OR	AH,AH
	JZ	READ_SEC
    ;
    ; WRITE
    ;
	XOR	DI,DI		; ES:DI -> start of tail
	JMP	SHORT SMOVE

READ_SEC:
	XOR	SI,SI		; DS:SI -> start of tail
SMOVE:
	MOV	CX,BP
	REP	MOVSW
	JMP	ALL_DONE

MAP_ERR1:
	CMP	AH,ABOVE_ERROR_BUSY ; Busy?
	JZ	MAP_NEXT	; Yes, wait till not busy (INTs are ON)
	ADD	SP,6		; Clean stack
	JMP	SHORT DNR_ERR

MAP_ERR2:
	CMP	AH,ABOVE_ERROR_BUSY
	JZ	MAP_AGN
	ADD	SP,2
DNR_ERR:
	MOV	AL,02H	       ; Drive not ready
	STC
	JMP	REST_CONT
;
;
;   this code has been written to handle te cases of overlapping usage
;   of the above board page frame segment by the cache and user buffer
;   assumption: in dos tracks cannot be more than 64 sectors long so
;   in the worst case we shall have the user buffer occupying three
;   pages is the page frame. we attempt to find the page that is
;   available for the cache and use it repeatedly to access the cache
;
;   above comment was for smartdrv. 128 sector reads are possible here
;   see the kludge in step 2 and step 4 to handle this


;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;   the algorithm is:
;   ******************************************************
;   [STEP1: determine the page we can use for the cache]
;
;   if (initial_para_offset_user in page 1, 2 or 3 ) then  {
;	    physical_cache_page = 0;
;	    cache_segment	= above board segment;
;		}
;						      else  {
;	    physical_cache_page = 3;
;	    cache_segment	= above_board_segment + 3*1024;
;		}
;
;   ******************************************************
;   [STEP2: initial setup]
;
;   count = user_count_requested;
;   number_to_be_transferred = min ( count, (16K - si) >> 2 );
;   exchange source and destination if necessary;
;
;   *******************************************************
;   [STEP3: set up transfer and do it]
;
;   count = count - number_to_be_transferred;
;   map_page cache_handle,physical_cache_page,logical_cache_page
;   mov data
;
;   *******************************************************
;   [STEP4: determine if another transfer needed and setup if so]
;
;   if ( count == 0 ) then exit;
;   if ( operation == read ) then source_offset = 0;
;			     else dest_offset	= 0;
;   number_to_be_transferred = min ( count, 8*1024 );
;   logical_page_number++ ;
;
;   *******************************************************
;   [STEP5: go to do next block]
;
;   goto [STEP3]
;^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;
new_code:
	assume	ds:ramcode,es:nothing,ss:nothing
;
;	input parameters:
;
;	bp : start para offset of user buffer from start of physical page frame
;	ax : end para offset of user buffer in physical page frame
;	di : transfer offset of user buffer
;	es : transfer segment of user buffer
;	dx : logical page number in cache
;	si : offset from start in logical page number
;
;	on stack { cx,bx } where cx = number of words, bx = read / write status
;
;   [STEP1: finding physical cache page and page frame]
;
    ;
    ; assume is physical page 0
    ;
	xor	al, al		; use page 0 for cache
	mov	bx,word ptr [base_addr+2]
    ;
    ; see if this assumption valid
    ;
	cmp	bp, 4*1024	; base is below start of page frame
	jae	ab$300
	cmp	bp,1024 	; is initial in page 1 or above
	jae	ab$30		; if so or assumption is valid
    ;
    ; else we have to correct our assumption
    ;
ab$300:
	mov	al, 3		; use page 3 for cache
	add	bx, 3*1024	; segment of page 3
    ;
    ; initialise page frame segment
    ;
ab$30:
	add	bp, 2*1024	; base of second transfer
	mov	cx, bp
	mov	ds,bx
    ;
assume	ds:nothing
;
;   [STEP2: initialising transfer parameters]
;
    ;
	pop	bp		; bp will have count of words left to be transferred
	pop	bx		; read / write status
;
; kludged to handle 64k byte transfers
;
	push	cx		; base of second transfer
    ;
    ; initialise the number of words needed for a second transfer to 0
    ;
	xor	cx,cx		;
    ;
    ; compare the number to be transferred to 16k words. any more than this
    ; will have to be done in the second transfer
    ;
	cmp	bp,16*1024	; more than 16k word transfers
	jbe	ab$301		; if not cx is fine
	mov	cx,bp		; else cx = number of words - 16*1024
	mov	bp,16*1024	; and bp = 16*1024
	sub	cx,bp		;
ab$301:
    ;
    ; store this on stack
    ;
	push	cx
;
; end of kludge in step 2
;
	push	bx		; save it back again
	push	dx		; save this too
    ;
    ; initially si offset into logical page, so we can only do 16*1024 - si
    ; byte transfer
    ;
	mov	cx,16*1024
	sub	cx,si
	shr	cx,1		; convert to word count
    ;
    ;	number to be transferred is the minimum of this and the user requested
    ;	count
    ;
	cmp	cx,bp
	jb	ab$31
	mov	cx,bp
    ;
ab$31:
    ;
    ;	see if write, then we have to switch source with destination
    ;
	or	bh,bh
	je	ab$32		; if read we don't have to do anything
				; else we have to switch
	src_dest_switch
ab$32:
    ;
    ;	set direction flag so that we don't have to do it repeatedly
    ;
	cld
;
;   [STEP3: set up transfer and do it]
;
ab$33:
    ;
    ;	update count of words still left to be transferred after this
    ;
	sub	bp,cx
    ;
    ;	map the logical page in cache to the physical page  selected
    ;
	mov	bx,dx		; get logical page into bx
				; al already holds the physical page #
	map_page
	jnc	ab$34		; suceeded ?
    ;
    ; else report error
    ;
	add	sp,6
	stc
	jmp	      restore_mp ; and go to restore page map
ab$34:
    ;
    ; succeeded, do the transfer
    ;
rep	movsw
    ;
;
;   [STEP4: check if transfer done, if not set up for next block]
;   [STEP5: go back to STEP3]
    ;
    ; check if done
    ;
	or	bp,bp		; count 0
	je	ab$40		; yes, go to finish up
    ;
    ;	recover original dx and bx, increment dx and then save both again
    ;
	pop	dx
	pop	bx
	inc	dx
	push	bx
	push	dx
    ;
    ; words to be transferred minimum of count and 8*1024 words
    ;
	mov	cx,8*1024	; 8k words in a page
	cmp	cx,bp		;
	jbe	ab$35		; if below or equal this is what we want
    ;
	mov	cx,bp		; else we can transfer the whole count
ab$35:
    ;
    ; see whether cache src or dest and accordingly reset either si or di
    ;
	or	bh,bh		; read?
	jne	ab$36		; if write go to modify
    ;
    ; read, zero si and go back to step3
    ;
	xor	si,si
	jmp	short ab$33	; to step 3
ab$36:
    ;
    ; write, zero di and go back to step3
    ;
	xor	di,di
	jmp	short ab$33	; to step 3
;
; finishing up we have to restore the page map
;
ab$40:
;
; also kludged to handle 64k byte transfers
;
	pop	dx
	pop	bx
	pop	bp		; number of words for second transfer
	pop	ax		; base of second transfer
	or	bp,bp		; are we done?
	jne	ab$407		; no, we have to do another transfer
	jmp	ab$405		; yes we can go to finish up
ab$407: 			; apologies for such abominations
	push	ax		; dummy transfer base
	xor	cx, cx
	push	cx		; zero count for next time
;
; restore the mapping context
;
	clc
	push	dx		; dx is destroyed by restore mapping context
	restore_mapping_context
	pop	dx		;
	jnc	ab$401
;
; error we should quit here
;
	add	sp, 4		; throw base & count
	ret
;
; we need to save the mapping context again
;
ab$401:
	save_mapping_context
	jnc	ab$406		; if we couldn't save it then error
	add	sp, 4
	ret
;
; reset physical page to be mapped to 0 and ds or es to page map base
; and increment logical page if we have si = 0 (read) or di=0 (write)
;
ab$406:
	mov	cx, word ptr [base_addr+2]
	cmp	ax, 1024	; new base in page 0?
	jb	ab$4060
	cmp	ax, 4*1024
	jae	ab$4060
	xor	ax, ax
	jmp	short ab$4061
ab$4060:
	mov	al, 3
	add	cx, 3*1024
ab$4061:
	or	bh,bh		; read or write?
	jne	ab$402		; if write branch
;
    ;
    ; read, reset ds to base address
    ;
	mov	ds,cx
	mov	cx,16*1024	;
	cmp	si, cx		; at end of page?
	jbe	ab$4030
	inc	dx
	xor	si, si
ab$4030:
	sub	cx,si
	shr	cx,1

ab$403:
	push	bx		; save these
	push	dx
;
	cmp	cx,bp		; is the cx appropriate
	jbe	ab$404		; if yes go to do transfer
	mov	cx,bp		; else cx <--- bp
ab$404:
	jmp	ab$33	  ; and go to do transfer
;
ab$402:
    ;
    ; write, reset es to base address
    ;
	mov	es,cx
	mov	cx,16*1024
	cmp	di, cx
	jb	ab$4020
	xor	di, di
	inc	dx
ab$4020:
	sub	cx,di
	shr	cx,1
	jmp	short ab$403
;
;	add	sp,4
ab$405:
	clc
restore_mp:
	restore_mapping_context
	ret

		DW	?		; SPACE for ABOVE_PID

;
; This label defines the end of the code swapped in at DRIVE_CODE
;
ABOVE_END	LABEL	WORD

BREAK	<Drive code for /A driver. Swapped in at RESET_SYSTEM>


;
; WARNING DANGER!!!!!!!
;
; This code is tranfered over the /E driver code at RESET_SYSTEM
;
; ALL jmps etc. must be IP relative.
; ALL data references must be to cells at the FINAL, TRUE location
;	(no data cells may be named HERE, must be named up at RESET_SYSTEM).
; SIZE of stuff between ABOVE_RESET and ABOVE_RESET_END MUST be less than
;	or equal to size of stuff between RESET_SYSTEM and RESET_INCLUDE.
;
; NOTE: EACH ABOVE BOARD driver has an INT 19 and 9 handler. This is
;	different from /E and RESMEM in which only the first
;	driver has an INT 19 and 9 handler.
;

IF2
  IF((OFFSET ABOVE_RESET_END - OFFSET ABOVE_RESET) GT (OFFSET RESET_INCLUDE - OFFSET RESET_SYSTEM))
	  %out ERROR ABOVE_RESET CODE TOO BIG
  ENDIF
ENDIF

;**	ABOVE_RESET perform TYPE 2 (/A) driver specific reboot code
;
;	This code issues an ABOVE_DEALLOC call for the memory
;	associated with this particular TYPE 2 RAMDrive since the
;	system is being re-booted and the driver is "gone".
;
;	ENTRY
;	    NONE
;	EXIT
;	    NONE
;	USES
;	    NONE
;
; This code is specific to TYPE 2 drivers
;

ABOVE_RESET:
ASSUME	DS:NOTHING,ES:NOTHING,SS:NOTHING
	PUSH	AX
	PUSH	DX
AGAIN_RESET:
	MOV	DX,[ABOVE_PID]
	MOV	AH,ABOVE_DEALLOC	; Close PID
	INT	67H
	CMP	AH,ABOVE_ERROR_BUSY
	JZ	AGAIN_RESET
	POP	DX
	POP	AX
	RET

;
; This label defines the end of the code swapped in at RESET_SYSTEM
;
ABOVE_RESET_END    LABEL   BYTE

BREAK	<RESMEM INIT Code>

;**	RESMEM specific data
;
;	The following datums are specific to the RESMEM (TYPE 3
;	or 4) drivers
;
;	Specific to TYPE 3 or TYPE 4 drivers
;

HIGH_SEG DW	?		; Segment addr of "end of memory" from INT 12
                                ; or from INIT packet BREAK address (M002).

RAMSEG	DW	0		; Segment addr of the start of RAMDrive memory.
				;   Basically a segment register version of
				;   BASE_ADDR

CRTSEG	EQU	0A000H		; Memory past this segment value is RESERVED
				;   Memory scan must stop here.


;**	RESMEM_INIT - Perform RESMEM (TYPE 3 or 4) specific initialization
;
;	This code performs the driver TYPE specific initialization for
;	TYPE 3 and TYPE 4 drivers.  [Type 3 no longer supported.]
;
;       M002: The size of memory is obtained from 1 of 2 sources; for DOS
;       versions prior to DOS 5.X, this size is obtained from INT 12h.  For
;       DOS version 5.X and beyond, this size is taken from the INIT packet
;       BREAK address.
;
;	Memory scan (No longer attempted)
;	    The method used by this code to "find" valid RAM between
;	    the "end of memory" as determined from the INT 12 memory
;	    size and CRTSEG is to look for memory which will correctly
;	    store data. It looks on 1K boundaries. If the first 2 words
;	    of a 1k block are good, it is assumed that the rest of the
;	    1K block is good without explicitly checking it. The scan
;	    is interested only in the FIRST block it finds. If two
;	    separated (by invalid RAM) blocks of RAM exist in the
;	    above range, the second block WILL NOT be found.
;	    NOTE that this can be fooled by a bad memory chip in
;	    a block of RAM. In this case RAMDrive will use the
;	    memory up to the bad spot and ignore the rest.
;	    Also note that since 16K is the minimum RAMDrive
;	    size, and the EMM_CTRL sector takes 1k, a block
;	    of size < 17K results in an insufficient memory error.
;           [M002: Minimum RAMDrive is now 4K, but this is irrelevant
;           since the described memory scan no longer takes place.]
;
;	    Since access to invalid RAM (RAM that isn't present)
;	    results in a parity error, the above scan must be done
;	    with parity checking disabled.
;
;	    Since the ROM BIOS memory initialization code and tests
;	    is only run on the memory indicated by INT 12, one of
;	    the things this code must do when it finds memory "above
;	    INT 12" is make sure all of the parity bits are set correctly.
;	    This is accomplished easily by just copying the memory to
;	    itself.
;
;	    The scan is NON-DESTRUCTIVE so that any data contained in
;	    the memory will not be destroyed.
;
;	    The result of this scan also makes the determination between
;	    a TYPE 3 and TYPE 4 RAMDrive. If memory is found, then we're
;	    TYPE 3. If no memory is found, then we're TYPE 4.
;
;
;	RESMEM_BLKMOV code swapped in at BLKMOV
;	RESMEM_RESET code swapped in at RESET_SYSTEM
;	    NOTE: This step is not needed for a TYPE 4 driver
;		    since TYPE 4 NEVER has an INT 9 or INT 19 handler,
;		    but it isn't harmful either, so we do it always.
;       IF Dos version < 5.X
;		Issue INT 12 to get size of memory
;       ELSE
;               Get memory size from INIT packet BREAK address
;	Convert INT 12 result to segment address of first byte after system
;	    memory.
;	IF this segment address is equal to or grater than CRTSEG
;	    There cannot be any memory "above INT 12" so we are TYPE 4.
;	    Skip the memory scan since there is no memory to scan and
;	    go to the TYPE 4 init code at CASE1.
;	Disable parity checking so access to non-existent RAM won't crash
;	    the system.
;	Perform the memory scan. This starts at FOO and ends at HAVE_MEM
;	    if we find some valid memory, or at CASE1 if we don't.
;	  A word about the scan.
;	    There are two cases for valid RAM.
;		1.) Valid memory starts at the INT 12 address
;		2.) There is invalid RAM for a while, then valid RAM starts.
;	    The DX register is used to tell us what is going on. It is
;	    non-zero if we are skipping over invalid RAM looking for
;	    some valid RAM (case 2), or 0 is we have found some valid RAM
;	    (case 1, or case 2 after skipping invalid RAM) and are scanning
;	    to set parity and find the end of the valid RAM.
;	    RAMSEG is given the initial value of 0 to indicate we have not
;	    found the start of a valid block.
;	    When the scan is finished ENABLE_PARITY is called to turn parity
;	    checking back on.
;	IF we have valid RAM and end at HAVE_MEM
;	    We are TYPE 3.
;	    RAMSEG contains the segment address of the start of the block
;	    BX is the segment address of the end of the block
;	    Subtract RAMSEG from BX to get size of region in paragraphs
;	    Convert size in Paragraphs to size in K
;	    Check that size is AT LEAST 17k (minimum size)
;	    Jump to GOT_RESMEM if OK else error
;	    Set EXT_K to size of block
;	    Adjust DEV_SIZE if bigger than EXT_K - 1 (-1 for EMM_CTRL)
;	    Convert RAMSEG to 32 bit address and set it into BASE_ADDR
;		This sets BASE_ADDR to point to EMM_CTRL sector.
;	    Set BASE_RESET to BASE_ADDR plus 1024
;	    Call MM_SETDRIVE to complete TYPE 3 specific initialization
;	ELSE we end up at CASE1
;	    We are TYPE 4.
;	    Set RESMEM_SPECIAL to indicate TYPE 4
;	    Set INIT_DRIVE to 2 (DOS volume MUST be initialized)
;	    Set BASE_ADDR to be the first para boundary after the resident
;		code (which DOES NOT include INT 19/INT 9 code).
;	    Compute TERM_ADDR based on DEV_SIZE Kbytes of device starting at
;		BASE_ADDR.
;	    NOTE: We must make sure the specified DEV_SIZE is reasonable:
;		It must not be bigger than 10 bits (1 Meg)
;			as this is the memory limit of the 8086.
;		It must not be so big that there is less than 48k of system
;			memory after the device is installed.
;			This is checked by computing the segment address
;			of the end of the device and comparing it to the
;			INT 12 memory end address minus 48k worth of paragraphs
;
;	ENTRY:
;	    Invokation line parameter values set.
;	EXIT:
;	    RESMEM_BLKMOV code swapped in at BLKMOV
;	    RESMEM_RESET code swapped in at RESET_SYSTEM
;	    Determination of TYPE 3 or TYPE 4 made by setting RESMEM_SPECIAL
;		if TYPE 4.
;	    CARRY SET
;		Error, message already printed. Driver not installed.
;		    If TYPE 3
;			EMM_CTRL not marked (but MAY be initialized if
;			a valid one was not found).
;	    CARRY CLEAR
;		DEV_SIZE set to TRUE size
;		INIT_DRIVE set appropriatly
;		IF TYPE 3
;		    BASE_ADDR set for this drive from EMM_BASE of EMM_REC
;		    BASE_RESET set from BASE_ADDR
;		    EMM_REC is marked EMM_ISDRIVER
;		    TERM_ADDR set to correct device end.
;			RESET_SYSTEM code and INT 9/INT 19 code included,
;			INT 19 and 9 vector patched if this is the first
;			TYPE 3 RAMDrive in the system.
;		IF TYPE 4
;		    BASE_ADDR set for this drive by computing address of
;			start of memory after RAMDrive code.
;		    BASE_RESET set from BASE_ADDR
;		    TERM_ADDR set to correct device end which includes
;			the memory taken up by the RAMDrive itself.
;
;	USES:
;	    ALL but DS
;
;	Code is specific to TYPE 3 and TYPE 4 drivers
;

RESMEM_INIT:
ASSUME	DS:RAMCODE,ES:NOTHING,SS:NOTHING
    ;
    ; Swap RESMEM code into place
    ;
	PUSH	CS
	POP	ES
	MOV	SI,OFFSET RESMEM_CODE
	MOV	DI,OFFSET DRIVE_CODE
	MOV	CX,OFFSET DRIVE_END - OFFSET DRIVE_CODE
	REP	MOVSB
	MOV	SI,OFFSET RESMEM_RESET
	MOV	DI,OFFSET RESET_SYSTEM
	MOV	CX,OFFSET RESET_INCLUDE - OFFSET RESET_SYSTEM
	REP	MOVSB
    ;
    ; We have THREE cases to contend with:
    ; [M002: For Dos versions >= 5.X, the memory limit passed to RAMDrive in
    ; the INIT packet BREAK address is used instead of the INT 12h value.]
    ;
    ;  1. There is NO memory above the INT 12H switch setting.
    ;	     In this case we will use the user specified device
    ;	     size (within limits) to allocate some memory as part
    ;	     of the RAMDRIVE.SYS resident image.
    ;	  NOTE: This type of a RAMDrive will not live through a warm boot
    ;
    ;  2. There is memory immediately after the INT 12H memory size.
    ;	     We will check for a EMM_CTRL there etc.
    ;
    ;  3. There is memory after the INT 12H memory size, but not
    ;	     Immediately after.
    ;	     We will check for a EMM_CTRL there etc.
    ;

;M002
        cmp     DosVersion,(5 shl 8)+00 ; DOS version < 5.X ?
        jb      rmi20                   ;  -yes, jump.
                                        ;  -no, get memory size from INIT
                                        ;    packet BREAK address.

        push    es
ASSUME ES:NOTHING
        les     bx,[PTRSAV]

IF DEBUG
     	jmp	short deb0

Deb0MesA DB      " Init Break SEG = $"
Deb0MesB DB      " Init Break OFFSET = $"

deb0:	push	cx
        push    dx
        mov	dx,offset Deb0MesA
	call	PRINT
        mov     ax,word ptr es:[bx].INIT_BREAK+2
        call    ITOA

	mov	dx,offset Deb0MesB
	call	PRINT
        mov	ax,word ptr es:[bx].INIT_BREAK
        call    ITOA
        pop	dx
        pop     cx
ENDIF

        mov     ax,WORD PTR es:[bx].INIT_BREAK+2
        pop     es
        jmp     short rmi40             ; AX = Segment # of top of available
        				;      memory.


rmi20:	INT	12H			; Get size of memory set on switches

;M002

IF DEBUG

	JMP	SHORT DEB1

DEB1MES DB	13,10,"INT 12 returned $"

DEB1:
	PUSH	CX
	PUSH	DX
	PUSHF
	PUSH	AX
	MOV	DX,OFFSET DEB1MES
	CALL	PRINT
	POP	AX
	PUSH	AX
	CALL	ITOA
	POP	AX
	POPF
	POP	DX
	POP	CX
ENDIF

	MOV	CL,6
	SHL	AX,CL			; Convert to Segment register value
rmi40:	MOV	BX,AX			; M002: BX = [HIGH_SEG] = end Segment
	MOV	[HIGH_SEG],AX		;       of available memory.

;
;*****************************************************************************
; Ramdrives installed between int12 reported memory and crtseg (A000h) are
; no longer allowed because on several machines including the model 50/60
; and the Tandy AT clone this area is used for something else.	The idea to
; install a ramdrive in system memory is bad anyway but we shall still support
; the installation of a ramdrive in low memory as part of the driver. isp
;
; [Some code was commented out here.  In other places, uncalled routines
; still supported type 3 driver.  I deleted it.  -dbo]

CASE1:
    ;
    ; Have CASE 1.
    ; Driver is TYPE 4
    ;

IF DEBUG

	JMP	SHORT DEB4

DEB4MES DB	13,10,"CASE 1$"

DEB4:
	PUSH	CX
	PUSH	DX
	PUSHF
	PUSH	AX
	MOV	DX,OFFSET DEB4MES
	CALL	PRINT
	POP	AX
	POPF
	POP	DX
	POP	CX
ENDIF

	PUSH	CS
	POP	DS
ASSUME	DS:RAMCODE
	INC	[RESMEM_SPECIAL]	; Flag SPECIAL case for INIDRV
	MOV	[INIT_DRIVE],2		; This type must ALWAYS be inited
    ;
    ; Compute BASE_ADDR to be right after DEVICE_END, NO INT 19/9 handler
    ;
	MOV	AX,OFFSET DEVICE_END
	ADD	AX,15			; Para round up
	MOV	CL,4
	SHR	AX,CL			; # of para in RAMDrive resident code
	MOV	DX,CS
	ADD	AX,DX			; AX is seg addr of start of RAMDrive
	PUSH	AX
	MOV	CX,16
	MUL	CX			; DX:AX is byte offset of that many paras
	MOV	WORD PTR [BASE_ADDR],AX
	MOV	WORD PTR [BASE_ADDR + 2],DX
	POP	AX
    ;
    ; Compute correct ending address and set TERM_ADDR
    ; Check that there is at least 48k of system memory after device end
    ; AX is the segment address of the start of the device
    ;
	MOV	DX,[DEV_SIZE]		; Get size in K
    ;
    ; DEV_SIZE can be at most a 10 bit number as that is 1 Meg, the memory
    ;	 limit on the 8086
    ;
	TEST	DX,0FC00H		; If any of high 6 bits set, too big
        JNZ     rmi60                   ; M002
	MOV	CL,6
	SHL	DX,CL			; DX is # of PARA in that many k
	ADD	AX,DX			; AX is end seg addr

;M002
	jnc	rmi70			; No overflow
rmi60:	jmp	short RES_NOMEM         ; Overflow.
rmi70:
    ;
    ; Dos versions < 5.X:  Leave at least 48K after RAMDrive for COMMAND.COM;
    ;                      This is really only a guess, and may be
    ;			   insufficient if other less considerate devices
    ;			   are loaded after RAMDrive.
    ; Dos versions >= 5.X: Since RAMDrive could be loaded into UMBs (which
    ;                      wouldn't contain COMMAND.COM), leaving space for
    ;                      COMMAND.COM would not be valid in all situations.
    ;                      Thus, in this case, we do not make any attempt to
    ;                      leave extra memory.
    ; Note that in all cases, the system will die gracefully if insufficient
    ; memory is left to run COMMAND.COM. (This is a characteristic of MS-DOS,
    ; and is independent of RAMDrive.)
    ;
	mov	dx,[HIGH_SEG]           ; DX = end Segment of available mem.
        cmp     DosVersion,(5 shl 8)+00 ; DOS version >= 5.X ?
        jae     rmi80                   ;  -yes, jump: don't leave extra.
	sub	dx,0C00H		;  -no: leave 48K (in PARAs) for
					;       COMMAND.COM.
rmi80:                                  ; DX = adjusted ending Segment of
                                        ;      available memory.
;M002

IF DEBUG

	JMP	SHORT DEB5

DEB5MESA DB	 " Max end is $"
DEB5MESB DB	 " end is $"

DEB5:
	PUSH	CX
	PUSHF
	PUSH	DX
	PUSH	AX
	MOV	DX,OFFSET DEB5MESA
	CALL	PRINT
	POP	DX
	POP	AX
	PUSH	AX
	PUSH	DX
	CALL	ITOA
	MOV	DX,OFFSET DEB5MESB
	CALL	PRINT
	POP	AX
	PUSH	AX
	CALL	ITOA
	POP	AX
	POP	DX
	POPF
	POP	CX
ENDIF

	JC	RES_NOMEM
	CMP	AX,DX
	JA	RES_NOMEM		; Too big
	MOV	WORD PTR [TERM_ADDR],0
	MOV	WORD PTR [TERM_ADDR + 2],AX

IF DEBUG

	JMP	SHORT DEB6

DEB6MES DB	" OK term $"

DEB6:
	PUSH	CX
	PUSHF
	PUSH	DX
	PUSH	AX
	MOV	DX,OFFSET DEB6MES
	CALL	PRINT
	POP	AX
	PUSH	AX
	CALL	ITOA
	POP	AX
	POP	DX
	POPF
	POP	CX
ENDIF
	CLC
	RET

RES_NOMEM:
ASSUME	DS:NOTHING,ES:NOTHING,SS:NOTHING
	MOV	DX,OFFSET ERRMSG2
	CALL	PRINT
	PUSH	CS
	POP	DS
	STC
	RET


BREAK	<Drive code for resmem driver. Swapped in at BLKMOV>

;
; This label defines the start of the TYPE 3 and 4 code swapped
;  in at BLKMOV
;
RESMEM_CODE	 LABEL	 WORD

;
; WARNING DANGER!!!!!!!
;
; This code is tranfered over the /E driver code at DRIVE_CODE
;
; ALL jmps etc. must be IP relative.
; ALL data references must be to cells at the FINAL, TRUE location
;	(no data cells may be named HERE, must be named up at BLKMOV).
; OFFSET of RESMEM_BLKMOV relative to RESMEM_CODE MUST be the same as
;	the OFFSET of BLKMOV relative to DRIVE_CODE.
; SIZE of stuff between RESMEM_CODE and RESMEM_END MUST be less than
;	or equal to size of stuff between DRIVE_CODE and DRIVE_END.

IF2
  IF((OFFSET RESMEM_BLKMOV - OFFSET RESMEM_CODE) NE (OFFSET BLKMOV - OFFSET DRIVE_CODE))
	  %out ERROR BLKMOV, RESMEM_BLKMOV NOT ALIGNED
  ENDIF
  IF((OFFSET RESMEM_END - OFFSET RESMEM_CODE) GT (OFFSET DRIVE_END - OFFSET DRIVE_CODE))
	  %out ERROR RESMEM CODE TOO BIG
  ENDIF
ENDIF

		DD	?	; 24 bit address of start of this RAMDRV

;**	RESMEM_BLKMOV - Perform transfer for TYPE 3 and 4 driver
;
;	This routine is the transfer routine for moving bytes
;	to and from a RAMDrive located in main memory.
;
;	METHOD:
;	    Convert start address into segreg index reg pair
;	    Mov computed segreg index reg pairs into correct registers
;	    Execute REP MOVSW to perform transfer
;
;	ENTRY:
;	    ES:DI is packet transfer address.
;	    CX is number of words to transfer.
;	    DX:AX is 32 bit start byte offset (0 = sector 0 of RAMDrive drive)
;	    BH is 1 for WRITE, 0 for READ
;
;	    BASE_ADDR set to point to start of RAMDrive memory
;		This "input" is not the responsibility of the caller. It
;		is up to the initialization code to set it up when the
;		device is installed
;
;	EXIT:
;	    Carry Clear
;		    OK, operation performed successfully
;	    Carry Set
;		    Error during operation, AL is error number
;
;	USES:
;	    ALL
;
;	This routine is specific to TYPE 3 and 4 drivers
;

RESMEM_BLKMOV:
ASSUME	DS:RAMCODE,ES:NOTHING,SS:NOTHING

	ADD	AX,WORD PTR [BASE_ADDR]
	ADC	DX,WORD PTR [BASE_ADDR + 2]
	PUSH	CX
	MOV	CX,16
	DIV	CX		; AX is seg reg value, DX is index register
	POP	CX
	OR	BH,BH
	JZ	READ_ITR
    ;
    ; WRITE
    ;
	PUSH	ES
	POP	DS
ASSUME	DS:NOTHING
	MOV	SI,DI
	MOV	ES,AX
	MOV	DI,DX
TRANS:
	REP	MOVSW
	CLC
	RET

READ_ITR:
	MOV	DS,AX
ASSUME	DS:NOTHING
	MOV	SI,DX
	JMP	TRANS

;
; This label defines the end of the RESMEM code swapped in at BLKMOV
;
RESMEM_END	 LABEL	 WORD

BREAK	<Drive code for resmem driver. Swapped in at RESET_SYSTEM>


;
; WARNING DANGER!!!!!!!
;
; This code is tranfered over the /E driver code at RESET_SYSTEM
;
; ALL jmps etc. must be IP relative.
; ALL data references must be to cells at the FINAL, TRUE location
;	(no data cells may be named HERE, must be named up at RESET_SYSTEM).
; SIZE of stuff between RESMEM_RESET and RESMEM_RESET_END MUST be less than
;	or equal to size of stuff between RESET_SYSTEM and RESET_INCLUDE.

IF2
  IF((OFFSET RESMEM_RESET_END - OFFSET RESMEM_RESET) GT (OFFSET RESET_INCLUDE - OFFSET RESET_SYSTEM))
	  %out ERROR RESMEM_RESET CODE TOO BIG
  ENDIF
ENDIF

;**	RESMEM_RESET perform TYPE 3 (RESMEM) driver specific reboot code
;
;	This code performs the EMM_ISDRIVER reset function as described
;	in EMM.ASM for all EMM_REC structures which are EMM_ALLOC and
;	EMM_ISDRIVER and of type EMM_MSDOS.
;
;	ENTRY
;	    NONE
;	EXIT
;	    NONE
;	USES
;	    NONE
;
; This code is specific to TYPE 3 drivers
;

RESMEM_RESET:
ASSUME	DS:NOTHING,ES:NOTHING,SS:NOTHING
	PUSH	SI
	PUSH	DI
	PUSH	AX
	PUSH	BX
	PUSH	CX
	PUSH	DX
	PUSH	DS
	PUSH	ES
	PUSH	CS
	POP	DS
ASSUME	DS:RAMCODE
	MOV	AX,WORD PTR [BASE_ADDR]
	MOV	DX,WORD PTR [BASE_ADDR + 2]
	SUB	AX,1024 		; Point back to EMM block
	SBB	DX,0
;
; NOTE: We can address the EMM block by just backing up
;	by 1024 bytes from BASE_ADDR because the RESET_SYSTEM handler
;	is in the FIRST RAMDrive driver
;
	MOV	CX,16
	DIV	CX			; AX is seg reg, DX is index reg
	MOV	DS,AX
ASSUME	DS:NOTHING
	MOV	SI,DX			; DS:SI -> EMM_CTRL
	MOV	DI,SI
	ADD	DI,EMM_RECORD
	MOV	CX,EMM_NUMREC
LOOK_RECRY:
    ;
    ; Scan EMM_CTRL for all ISDRIVER MS-DOS regions and turn off ISDRIVER
    ;
	TEST	[DI.EMM_FLAGS],EMM_ALLOC
	JZ	DONERY
	TEST	[DI.EMM_FLAGS],EMM_ISDRIVER
	JZ	NEXTRECRY		 ; No Driver
	CMP	[DI.EMM_SYSTEM],EMM_MSDOS
	JNZ	NEXTRECRY
	AND	[DI.EMM_FLAGS],NOT EMM_ISDRIVER
NEXTRECRY:
	ADD	DI,SIZE EMM_REC
	LOOP	LOOK_RECRY
DONERY:
	POP	ES
	POP	DS
ASSUME	DS:NOTHING
	POP	DX
	POP	CX
	POP	BX
	POP	AX
	POP	DI
	POP	SI
	RET

;
; This label defines the end of the RESMEM code swapped in at RESET_SYSTEM
;
RESMEM_RESET_END    LABEL   BYTE

BREAK <messages and common data>

;**	Message texts and common data
;
;	Init data. This data is disposed of after initialization.
;	it is mostly texts of all of the messages
;
;	COMMON to TYPE 1,2,3 and 4 drivers
;
;
;	translatable messages moved to message module (SP)

	EXTRN	NO_ABOVE:BYTE,BAD_ABOVE:BYTE,NO_MEM:BYTE
	EXTRN	NOXMM:BYTE,ERRXMM:BYTE,XMMCHAIN:BYTE
	EXTRN	ERRMSG1:BYTE,ERRMSG2:BYTE,INIT_IO_ERR:BYTE,BADVERMES:BYTE
	EXTRN	HEADERMES:BYTE,PATCH2X:BYTE,DOS_DRV:BYTE
	EXTRN	STATMES1:BYTE,STATMES2:BYTE,STATMES3:BYTE
	EXTRN	STATMES4:BYTE,STATMES5:BYTE
        EXTRN   SECT_ADJ:BYTE           	;M001
	db	"RAMDrive is a trademark of Microsoft Corporation."
	db	"This program is the property of Microsoft Corporation."

VOLID	DB	'MS-RAMDRIVE',ATTR_VOLUME_ID
	DB	10 DUP (0)
;
; Volume creation date = driver release date
;
	DW	0				;time=midnight
	DW	((1992-1980)*512)+(7*32)+21	;date=1992 Jul 21
	DW	0,0,0

SECTOR_BUFFER	DB	1024 DUP(0)
;
; Note (M00):  SECTOR_BUFFER is used during ABOVE_INIT to hold
; the addresses of mappable EMS pages.  Max size required is
; 1024K / 16K * 4 = 256 bytes.
;

RAMDrive_END	   LABEL   BYTE

RAMCODE ENDS
	END


