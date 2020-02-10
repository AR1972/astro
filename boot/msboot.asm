	Page ,132
TITLE BOOT	SECTOR 1 OF TRACK 0 - BOOT LOADER
;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

;   Rev 1.0 ChrisP, AaronR and others.	2.0 format boot
;
;   Rev 3.0 MarkZ   PC/AT enhancements
;		    2.50 in label
;   Rev 3.1 MarkZ   3.1 in label due to vagaries of SYSing to IBM drive D's
;		    This resulted in the BPB being off by 1.  So we now trust
;		    2.0 and 3.1 boot sectors and disbelieve 3.0.
;
;   Rev 3.2 LeeAc   Modify layout of extended BPB for >32M support
;		    Move PHYDRV to 3rd byte from end of sector
;		    so that it won't have to be moved again
;		    FORMAT and SYS count on PHYDRV being in a known location
;
;   Rev. 3.3 D.C. L. Changed Sec 9 EOT field from 15 to 18. May 29, 1986.
;
;   Rev 3.31 MarkT  The COUNT value has a bogus check (JBE????) to determine
;		    if we've loaded in all the sectors of IBMBIO. This will
;		    cause too big of a load if the sectors per track is high
;		    enough, causing either a stack overflow or the boot code
;		    to be overwritten.
;
;   Rev 4.00 J. K.  For DOS 4.00 Modified to handle the extended BPB, and
;		    32 bit sector number calculation to enable the primary
;		    partition be started beyond 32 MB boundary.
;
;
; The ROM in the IBM PC starts the boot process by performing a hardware
; initialization and a verification of all external devices.  If all goes
; well, it will then load from the boot drive the sector from track 0, head 0,
; sector 1.  This sector is placed at physical address 07C00h.	The initial
; registers are set up as follows:  CS=DS=ES=SS=0.  IP=7C00h, SP=0400H.
;
; The code in this sector is responsible for locating the MSDOS device drivers
; (IBMBIO) and for placing the directory sector with this information at
; physical address 00500h.  After loading in this sector, it reads in the
; entirety of the BIOS at BIOSEG:0 and does a long jump to that point.
;
; If no BIOS/DOS pair is found an error message is displayed and the user is
; prompted to reinsert another disk.  If there is a disk error during the
; process, a message is displayed and things are halted.
;
; At the beginning of the boot sector, there is a table which describes the
; MSDOS structure of the media.  This is equivalent to the BPB with some
; additional information describing the physical layout of the driver (heads,
; tracks, sectors)
;
;==============================================================================
;REVISION HISTORY:
;AN000 - New for DOS Version 4.00 - J.K.
;AC000 - Changed for DOS Version 4.00 - J.K.
;AN00x - PTM number for DOS Version 4.00 - J.K.
;==============================================================================
;AN001; d52 Make the fixed positioned variable "CURHD" to be local.  7/6/87 J.K.
;AN002; d48 Change head settle at boot time.			     7/7/87 J.K.
;AN003; P1820 New message SKL file				   10/20/87 J.K.
;AN004; D304 New structrue of Boot record for OS2.		   11/09/87 J.K.
;AN005; Changed version to 5.0					   03/08/90 E.A.
;AN006; Changed to remove MSLOAD in first cluster restriction	   04/23/90 J.H.
;==============================================================================


ORIGIN		EQU	7C00H		; Origin of bootstrap LOADER
BIO_SEG		EQU	70H		; Destingation segment of BIOS
BIO_OFFSET	EQU	700H		; Offset of bios
SECTOR_SIZE	EQU	512		; Sector size in bytes
DIR_ENTRY_SIZE	EQU	32		; Size of directory entry in bytes
DIR_OFF		EQU	500h
IBM_LOAD_SIZE	EQU	3		;J.K. Size of IBMLOAD module in sectors
ROM_DISKRD	EQU	2
DSK_PARMS	EQU	1EH*4		;POINTER TO DRIVE PARAMETERS



; ==========================================================================

include version.inc
include	dirent.inc

; ==========================================================================

SEGBIOS SEGMENT AT BIO_SEG

	; Define the destination segment of the BIOS, including the
	; initialization label

BIOS	LABEL	BYTE

SEGBIOS ENDS

; ==========================================================================

CODE	SEGMENT
	ASSUME CS:CODE,DS:NOTHING,ES:NOTHING,SS:NOTHING
	ORG	ORIGIN


	Public $START
$START:
			; WARNING -- Don't change this to a short jmp
	jmp	Main			; Jump to start of code


; ==========================================================================

					; Start of BPB area of the boot record
IF IBMCOPYRIGHT
		DB	"IBM  "
ELSE
		DB	"MSDOS"
ENDIF

;	BUGBUG -- 7 Dec 1992 -- chuckst -- changed version back to 5.0 to
;					avoid bug in PC-TOOLS DISKFIX
OsVersion	DB	"5.0"		; DOS version number
BytesPerSector	DW	SECTOR_SIZE	; Size of a physical sector
SecsPerClust	DB	8		; Sectors per allocation unit
ReservedSecs	DW	1		; Number of reserved sectors
NumFats		DB	2		; Number of fats
NumDirEntries	DW	512		; Number of direc entries
TotalSectors	DW	4*17*305-1	; Number of sectors - number of hidden
					; sectors (0 when 32 bit sector number)
MediaByte	DB	0F8H		; MediaByte byte
NumFatSecs	DW	8		; Number of fat sectors
SecPerTrack	DW	17		; Sectors per track
NumHeads	DW	4		; Number of drive heads

HiddenSecs	DD	1		; Number of hidden sectors
BigTotalSecs	DD	0		; 32 bit version of number of sectors
BootDrv		DB	80h
CurrentHead	DB	0h		; Current Head
ExtBootSig	DB	41
SerialNum	DD	0
VolumeLabel	DB	'NO NAME    '
FatId		DB	'FAT12   '

		; Danger!!! If not 32 bit sector number calculation,
		; FORMAT should set the value of HiddenSecsHigh and
		; BigTotalSectors to 0 !!!

	PUBLIC uData
uData	LABEL	BYTE
						;Equates to allow access to
						;storage where Main is now
Sec9		EQU	BYTE PTR uData+0	;11 byte diskette parm. table
BiosLow 	EQU	WORD PTR uData+11
BiosHigh	EQU	WORD PTR uData+13
CurTrk		EQU	WORD PTR uData+15
CurSec		EQU	BYTE PTR uData+17
DirLow		EQU	WORD PTR uData+18
DirHigh		EQU	WORD PTR uData+20

; =========================================================================

		; First thing is to reset the stack to a better and more known
		; place. The ROM  may change, but we'd like to get the stack
		; in the correct place.
MAIN:
	cli				;Stop interrupts till stack ok
	xor	AX,AX
	mov	SS,AX			;Work in stack just below this routine

	ASSUME	SS:CODE
	mov	SP,ORIGIN
	push	SS
	pop	ES

	ASSUME	ES:CODE

		; We copy the disk parameter table into a local area. We scan
		; the table above for non-zero parameters.  Any we see get
		; changed to their non-zero values. We copy the disk parameter
		; table into a local area (overlayed into the code)

	mov	BX,DSK_PARMS
	lds	SI,DWORD PTR SS:[BX]	; get address of disk table
	push	DS			; save original vector for possible
	push	SI			; restore
	push	SS
	push	BX
	mov	DI,OFFSET Sec9
	mov	CX,11
	cld

	repz	movsb
	push	ES
	pop	DS			; DS = ES = code = 0.
	assume	DS:CODE

		; Set the head settle time to 15ms because we don't have room
		; to do a disk retry and then set sectors per from the value
		; in the BPB

	mov	BYTE PTR [DI-2], 0fh	; Head settle time
	mov	CX, SecPerTrack
	mov	BYTE PTR [DI-7], cl	; End of Track

	mov	[BX+2],AX		; Place in new disk parameter
	mov	[BX],offset Sec9	; table vector

		; We may now turn interrupts back on. Before this, there is
		; a small window when a reboot command may come in when the
		; disk parameter table is garbage

	sti				; Interrupts OK now
	int	13h			; Reset the disk system just in case
	jc	CkErr			; any thing funny has happened.

		; The system is now prepared for us to begin reading.
		; First, determine logical sector numbers of the start of the
		; directory and the start of the data area.

	xor	AX,AX
	cmp	TotalSectors,AX		; 32 bit calculation?
	je	Dir_Cont

	mov	CX,TotalSectors
	mov	WORD PTR BigTotalSecs,CX ; BigTotalSecs

Dir_Cont:
	mov	AL,NumFats		;Determine sector dir starts on
	mul	NumFatSecs		;DX;AX
	add	AX,WORD PTR HiddenSecs
	adc	DX,WORD PTR HiddenSecs[2]
	add	AX,ReservedSecs
	adc	DX,0

		; DX:AX = NumFats * NumFatSecs + ReservedSecs + cSecHid

	mov	[DirLow],AX
	mov	[DirHigh],DX
	mov	[BiosLow],AX
	mov	[BiosHigh],DX

		; Take into account size of directory (only know number
		; of directory entries)

	mov	AX,DIR_ENTRY_SIZE	; bytes per directory entry
	mul	NumDirEntries		; convert to bytes in directory
	mov	BX,BytesPerSector	; add in sector size
	add	AX,BX
	dec	AX			; decrement so that we round up
	div	BX			; convert to sector number
	add	[BiosLow],AX		; Start sector # of Data area
	adc	[BiosHigh],0

		; We load in the first directory sector and examine it to
		; make sure the the BIOS and DOS are the first two directory
		; entries. If they are not found, the user is prompted to
		; insert a new disk. The directory sector is loaded into 00500h

	mov	BX,DIR_OFF		; sector to go in at 00500h

	mov	DX,[DirHigh]
	mov	AX,[DirLow]		; logical sector of directory
	call	DoDiv			; convert to sector, track, head
	jc	CkErr			; Overflow? BPB must be wrong!!

	mov	al, 1			; disk read 1 sector
	call	DoCall			; do the disk read
	jb	CkErr			; if errors try to recover


		; Now we scan for the presence of BIOS file.

	mov	DI,BX
	mov	CX,11
	mov	SI,OFFSET Bio		; point to "ibmbio  com"
	repz	cmpsb			; see if the same
	jnz	CkErr			; if not there advise the user

		; Found the BIOS. Check the second directory entry.
		; SI will already point to "MSDOS  SYS" if first compare
		; was successful

	lea	DI,[BX+20h]
;**	mov	SI,OFFSET Dos
	mov	CX,11
	repz	cmpsb
	jz	DoLoad

		; There has been some recoverable error. Display a message
		; and wait for a keystroke.


CkErr:	mov	SI,OFFSET SysMsg	; point to no system message

ErrOut:
	call	Write			; and write on the screen


	xor	AX,AX			; wait for response
	int	16h			; get character from keyboard
	pop	SI			; reset disk parameter table back to
	pop	DS			; rom
	pop	[SI]
	pop	[SI+2]
	int	19h			; Continue in loop till good disk

Load_Failure:
	pop	ax			;adjust the stack
	pop	ax
	pop	ax
	jmp	short CkErr		;display message and reboot.

		; We now begin to load the BIOS in.
		; All we have to do is just read is multiply the BioStartClus
		; by SecsPerClust to find the logical sector for the start
		; of the BIOS file. When this value is added to the double
		; word BiosHigh:BiosLow we get the absolute sector offset
		; for the start of the file and then read the  sectors
		; contiguously IBM_LOAD_SIZE times. We here assume that
		; IBMLOAD module is contiguous. Currently we estimate that
		; IBMLOAD module will not be more than 3 sectors.
DoLoad:
	mov	AX,[BX].DIR_FIRST	; AX = BIOS starting cluster
	dec	AX			; Subtract first 2 reserved clusters
	dec	AX
	mov	BL,SecsPerClust		; BX = Sectors per cluster
	xor	BH,BH
	mul	BX			; DX:AX = first logical sector of bios

	add	AX,[BiosLow]		; Add absolute start sector
	adc	DX,[BiosHigh]		; DX:AX = Absolute bios sector offset

	mov	BX,BIO_OFFSET		;offset of ibmbio(IBMLOAD) to be loaded.
	mov	CX,IBM_LOAD_SIZE	;# of sectors to read.

Do_While:
	push	AX
	push	DX
	push	CX
	call	DoDiv			; DX:AX = sector number.
	jc	Load_Failure		; Adjust stack. Show error message
	mov	al, 1			; Read 1 sector at a time.
					; This is to handle a case of media
					; when the first sector of IBMLOAD is the
					; the last sector in a track.
	call	DoCall			; Read the sector.
	pop	CX
	pop	DX
	pop	AX
	jc	CkErr			; Read error?
	add	AX,1			; Next sector number.
	adc	DX,0
	add	BX,BytesPerSector	; Adjust buffer address.
	loop	Do_While


; =========================================================================
; Main read-in loop.
; ES:BX points to area to read.
; Count is the number of sectors remaining.
; BIOS$ is the next logical sector number to read
;
; CurrentHead is the head for this next disk request
; CurTrk is the track for this next request
; CurSec is the beginning sector number for this request
;
; AX is the number of sectors that we may read.
;
; =========================================================================
;
; IBMINIT requires the following input conditions:
;
;   DL = INT 13 drive number we booted from
;   CH = media byte
;   IBMBIO init routine should check if the boot record is the
;   extended one by looking at the extended_boot_signature.
;   If it is, then should us AX;BX for the starting data sector number.
; =========================================================================

DISKOK:
	mov	CH,MediaByte
	mov	DL,BootDrv
	mov	BX,[BiosLow]		; J.K.I1.Get bios sector in bx
	mov	AX,[BiosHigh]		; J.K.I1.
	jmp	FAR PTR Bios		;CRANK UP THE DOS

WRITE:
	lodsb				;GET NEXT CHARACTER
	or	AL,AL			;Clear the high bit
	jz	EndWr			;ERROR MESSAGE UP, JUMP TO BASIC
	mov	AH,14			;WILL WRITE CHARACTER & ATTRIBUTE
	mov	BX,7			;ATTRIBUTE
	int	10h			;PRINT THE CHARACTER
	jmp	Write

		; Convert a logical sector into Track/sector/head.
		; DX;AX has the sector number. Because of not enough space, we
		; are  going to use Simple 32 bit division here.
		; Carry set if DX;AX is too big to handle.

DODIV:
	cmp	DX,SecPerTrack		; To prevent overflow!!!
	jae	DivOverFlow		; Compare high word with the divisor.
	div	SecPerTrack		; AX = Total tracks, DX = sector number
	inc	DL			; We assume SecPerTrack < 255 & DH=0
					; curSec is 1-based.
	mov	CurSec, DL		; Save it
	xor	DX,DX
	div	NumHeads
	mov	CurrentHead,DL		;Also, NumHeads < 255.
	mov	CurTrk,AX
	clc
	ret

DivOverFlow:
	stc

EndWR:
	ret
; =========================================================================
;
; Issue one read request. ES:BX have the transfer address,
; AL is the number of sectors.
;
; =========================================================================

DOCALL:
	mov	AH,ROM_DISKRD			;=2
	mov	DX,CurTrk
	mov	CL,6
	shl	DH,CL
	or	DH,CurSec
	mov	CX,DX
	xchg	CH,CL
	mov	DL, BootDrv
	mov	DH, CurrentHead
	int	13h
	ret

; =========================================================================

include boot.cl1

; =========================================================================

	IF IBMCOPYRIGHT
Bio	DB	"IBMBIO  COM"
Dos	DB	"IBMDOS  COM"
	ELSE
Bio	DB	"IO      SYS"
Dos	DB	"MSDOS   SYS"
	ENDIF

; =========================================================================

Free	EQU (SECTOR_SIZE - 4) - ($-$start)

if Free LT 0
    %out FATAL PROBLEM:boot sector is too large
	.ERR
endif

	org	origin + (SECTOR_SIZE - 2)
	db	55h,0aah			; Boot sector signature

; =========================================================================

CODE	ENDS
	END 

